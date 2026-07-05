# socio_public_services — Project Instructions

## What This Project Is

R data formatting pipeline for French sociological surveys (ELIPSS, Virage, Enquête Emploi, etc.).
Not an R package — a standalone script project. No `DESCRIPTION` file, no `devtools::check()`.

Main source: `R/data_formatting_pipeline.R` (~6000 lines). All functions live in this single file.

---

## Pipeline Architecture

The pipeline transforms raw survey data (`.dta`, `.sav`, `.csv` with SAS format files) into
clean, labelled, analysis-ready tibbles. Steps run sequentially, each writing to a single
unified JSON file (`*.survey_meta.json`) that grows brick-by-brick:

```
1. extract_survey_metadata(df, meta_json, ...)
   → Detects column roles, writes initial JSON with levels/labels/role
   → Numeric vars keep ONLY their special codes as levels (missing_num matches +
     labelled codes, flagged missing:true); plain data-range values never become
     levels. Labelled codes are auto-flagged missing ONLY when SPARSE sentinels
     (≤2 extra labelled codes, or unlabelled-observed > max_levels_cat): a fully-
     labelled count (integer_count with a label per value, e.g. NB_PERS_DOM) drops
     its labels and stays a clean count, never 100% NA. Sparsely-labelled numerics
     stay numeric, not factor (coverage).
   → Value-label codes sort NUMERICALLY when all integer-like (string codes "1".."10"
     otherwise sort lexically, "10" between "1" and "2"). A bare 0/1 numeric with no
     value labels → factor_binary with synthesized Non(0)/Oui(1), positive=order 1.
   → empty_levels = c("small_factors","all","none") controls declared value-label codes
     NOT observed in the data (EMPTY levels). Default "small_factors" keeps them as levels
     when the var declares ≤ max_levels_cat codes (binaries, Likert, small nominal — keeps
     every battery member on one level set), drops them from over-declared sets; "all"
     always keeps, "none" drops (classic inner join). Kept empties are flagged n:0 AT
     EXTRACT (visible in manual review). Empties are FACTOR-only (numeric roles keep only
     observed special/missing codes). Consequence: a labelled 0/1 with only "Non" observed
     stays factor_binary with an empty Oui pole (n:0) — no more factor_unique_value.
   → Returns invisible(survey_meta) — enables |> piping

2. ai_classify_roles(meta_json, ...)
   → AI disambiguates ordinal vs nominal, writes role + desc + order to JSON

3. metadata_add_level_stats(meta_json, df)
   → Adds n/pct per level + num_stats + config.n_individuals
   → num_stats EXCLUDE the per-variable missing:true level codes (single source of
     truth — NOT config.missing_num). Counts each missing code (factor AND numeric)
     and stores it as the level's `n`.
   → Top-level na_n/na_pct for EVERY var (NA + missing-coded, post-format); text/other
     vars also get an `examples` array (first 5 distinct raw values)
   → For factors, adds df-observed codes absent from the value labels (empty label,
     flagged for review); required before ai_suggest_labels / generate_codebook

4. ai_suggest_labels(meta_json, ...)
   → AI suggests short display labels, writes new_label to JSON levels

5. ai_suggest_varnames(meta_json, ...)
   → AI suggests short variable names, writes new_name to JSON

5.5 ai_build_outline(meta_json, seed = TRUE, min_size = 3, use_batch, resume_batch_id, dry_run, ...)
   → ONE global AI pass that covers every variable with a SINGLE AI level, the #### group, so the
     codebook reads as a table of contents. The outline SECTIONS are user-provided and FIXED: the
     ## blocs always (set_headers / extract headers=), and optionally ### subthemes for big surveys.
     The AI does NOT create sections — it only partitions each section into #### groups (batteries +
     thematic groupings), covering everything. Input = every var IN ORDER with the fixed ##/###
     sections interleaved as {"section":"..."} rows + the config.survey_description prefix (set at
     extract, read from the JSON — not an argument) + a deterministic battery-candidate seed
     (`batt`, a HINT). Output = contiguous spans [{title, from, to, battery}] (battery true|false).
   → Authoritative: clears the AI-owned level (#### in `headers` and all `battery`), validates each
     span (unknown/reversed/crosses-##/crosses-###/overlap rejected — a #### stays inside one ## and
     one ### section). #### groups have NO minimum size (they tile every section); a battery:true
     below min_size is DEMOTED to a thematic group (kept, not boxed) so coverage holds. Writes
     non-battery #### to `headers` (start-markers) and true batteries (>=min_size) to the repeated
     `battery` field; reports variables in no #### (incomplete coverage). ##/### anchors never
     touched; JSON untouched if 0 valid spans. seed=FALSE: batt null everywhere.
     Batteries are strictly CONTIGUOUS same-role runs (NO mixed/interleaved, NO bridge — those are
     handled upstream by check_batteries() + manual reorder). Prompt = instructions/outline_prompt.md
     (real pps20 examples; seed framed as a mere candidate the AI overrides; teaches contiguous
     batteries, split-when-several-questions, numeric-grid-is-a-battery, and recaps OUT of the item
     battery — but >=3 parallel recaps form their OWN battery:true).
     Reuses all shared AI infra (.build_message_body / ai_call_claude / batch / .cache_ai_raw / parse).
   → Internal seed = .batt_seed_candidates(): same role + level-code signature + a precision gate
     (.batt_precision_ok: name-token prefix OR >=10-char label stem — role+codes alone is not enough),
     split by prefix cluster, heals + FLAGS type-outliers. preview_outline(meta_json) prints the
     full ##/###/#### markdown outline (batteries expanded), usable before/after this step.

5.4 check_batteries(meta_json, min_size = 3) — pre-AI, read-only console diagnostic (run after
     extract, before ai_build_outline). Reuses the seed signature + precision gate. Flags (A) INTERLEAVED
     (mixed) batteries — non-contiguous same-signature groups — with a copy-paste relocate() to gather
     them (apply on df, then extract recreate=TRUE); (B) TYPE-OUTLIERS — a single wrong-role member
     between same-question neighbours (catches a binary mis-typed integer_count, e.g. LIVRE) — to fix
     before the AI. Returns invisible(list(reorder, outliers)).

6. generate_format_script(meta_json, output_path = NULL)
   → Generates executable R script that applies all formatting
   → Reads numeric stats from JSON (run metadata_add_level_stats() first)
   → Factor blocks: fct_recode(factor(as.character(x)) [|> fct_expand(<codes>)], ...). The
     forcats fct_expand() step is inserted ONLY for codes with n==0 (empty levels), so the
     empty level survives into the R factor (fully-observed vars emit no fct_expand → output
     unchanged); if any level's n is unknown (stats not run) all declared codes are expanded.
   → Simplified: no codebook / no "## Variable list" / no "# Select and reorder" sections;
     each block applies its var label inline via
     "label" -> varlab  then  ... |> `attr<-`("label", varlab)  (survives conversion)
   → Emits the JSON outline as RStudio/Positron FOLDABLE section comments (same headers/battery
     source as generate_codebook): each var's `headers` (##/###/####) → a "<##> Title ----" section
     (level = leading-# count clamped 2..4, #s kept so nesting matches the codebook); the "Rename
     variables"/"Format variables" banners are single-# level-1 containers. True batteries render a
     foldable "#### ◆ Batterie — <title>  (N variables) ----" opener (via `.gfs_battery_open`, N =
     contiguous run length) closed by a plain "# └── fin batterie ──" rule (`.gfs_battery_close`,
     no trailing ----, so it lives inside the fold). Injected at the top of `.gfs_format_blocks()`'s
     per-var loop via `prev_battery`, mirroring `.cb_build_tibble()`; `.gfs_section_comment()` does
     the #-count/clamp/strip. The per-var "# \"VAR\" role" comment stays a plain (non-section) comment.
   → Section titles get DECORATIVE bars for visibility: level 2 (survey blocs) a heavy "# ═…═" box
     above+below, level 3 (subthemes) a light "# ─…─" rule above, level 4 plain. KEY: the bars (and
     the top file banner, now `banner_bar`) use BOX-DRAWING chars (═ ═ / ─ ─), NOT ASCII
     #/=/- — a pure "# ====" / "####…" rule line matches the "#+ <label> [-=#]{4,}$" section rule with
     an EMPTY label and pollutes the Positron/RStudio outline; box chars can't match, so only the
     "## Title ----" line is ever an outline node. `.gfs_section_comment()` returns a char VECTOR.
     All new box/marker glyphs are \u-escaped (Windows source-encoding safety, file convention).

7. generate_codebook(meta_json, output_path = NULL, lang = "fr", keep_original = FALSE, ...)
   → Styled .xlsx codebook (openxlsx2): one row per level / numeric stat, variable
     info merged over rows, headers, frozen panes, selective borders.
   → Headers are DATA-DRIVEN from the JSON (no more titles/binary_batteries args): each
     var's `headers` array holds its ##/### outline titles (## stripped for display); its
     `battery` field the #### question-battery title (one header per run). A battery is CLOSED
     by an empty 2 cm row, unless the next variable already carries a header (a new #### battery
     or a ##/### outline) — so standalone variables never read as part of the battery above them.
   → TOP MATTER (config-driven): config.survey_title → a level-1 `# ` title row
     "Dictionnaire des codes – <title>" (fr) / "Codebook – …" (en); then ONE row PER non-empty
     survey_* field (survey_description + Champ / Producteur / Diffuseur / Source / Méthodologie,
     from `.cb_frontmatter_fields()`), each spanning columns description..valeur (merged for width),
     bold prefixes + markdown ** / * converted to Excel bold/italic via `.md_to_fmt_txt()`
     (`.md_tokens()` splits the runs). The survey_population row also shows config.n_individuals in
     the `n` column. Merged rows get an explicit height (est. lines), since merges don't auto-fit.
   → BATTERIES: when the JSON has ≥1 true battery, a last `prefixe_question` column is added
     AUTOMATICALLY (no arg) holding a ready-to-use dplyr SELECTOR of the battery's final
     `new_name`s — the unique common prefix (use starts_with(), e.g. "PAP_") or, failing that,
     the pipe-joined names (use matches(), "V1|V2"), from `.battery_selector()`; merged into one
     wrapped cell across the (contiguous) battery. Each true battery also gets a dark-red MEDIUM
     rectangle around its valeur|n|freq block (`wb_add_border(update=TRUE)`, numfmt preserved).
   → Reads JSON only — NO df param (examples/NA now stored in the JSON by
     metadata_add_level_stats). NA cell = missing-value summary (see below), all types.
   → meta_json may be a DATA FRAME: runs extract + metadata_add_level_stats silently on a
     temp JSON (tempdir), keep_original forced TRUE (raw labels, code order, no prefix);
     `...` forwarded to extract_survey_metadata.
   → val labels/order reuse .gfs_build_entries()/.gfs_level_label() → identical to
     generate_format_script(). Run metadata_add_level_stats() first for n/pct/NA.
```

All functions take `meta_json` (path string or `survey_meta` object) as their first argument.
The metadata tibble is an internal implementation detail; users never construct it directly.

### JSON as Source of Truth

The unified JSON (`*.survey_meta.json`) is the single source of truth between steps.
Users can (and do) manually edit the JSON between AI steps. Key fields per variable:

- `role`: factor_binary, factor_nominal, factor_ordinal, integer, integer_count, double, identifier
- `desc`: boolean — TRUE = descending order for ordinal factors
- `new_name`: short variable name suggested by AI
- `levels.{code}.new_label`: short display label suggested by AI
- `levels.{code}.missing`: TRUE for missing-value levels (old `null_coded` field renamed). The set of
  missing:true level codes is the SINGLE source of truth for stats-exclusion + format-script NA-conversion.
- `levels.{code}.n`: written for every level incl. missing (missing-value counts); `pct` non-missing only.
  An `n: 0` on a level = an EMPTY level (declared value label with no observation); written at extract for
  kept-unobserved codes (see `empty_levels`) and re-confirmed by metadata_add_level_stats().
- `levels.{code}.order`: integer for ordinal level ordering (missing levels have none)
- `config.n_individuals`: total row count (written by extract_survey_metadata / backfilled by
  metadata_add_level_stats).
- `config.survey_*` (scalar free text, all optional): `survey_title` (codebook level-1 heading),
  `survey_description` (also read by ai_build_outline), `survey_population`, `survey_producer`,
  `survey_source`, `survey_distributor`, `survey_methodology`. Set via extract_survey_metadata()
  args (source of truth when supplied, preserved on re-extract) or by editing the JSON. `_description`
  / `_methodology` support markdown ** / *. Each new scalar MUST be in the `cfg_fields` allow-list AND
  the scalar-branch list in `.write_meta_json()` (both extended together as `.survey_scalars`), else
  it is dropped on write.
- `na_n` / `na_pct` (top-level, per variable, ALL types): count/percent of individuals NA after
  formatting = NA + missing-coded. Written by metadata_add_level_stats. Codebook prefers these;
  falls back to n_individuals − Σ(non-missing level n) for factors on older JSONs.
- `examples` (top-level, text/"other" vars only): first 5 distinct raw values, for the codebook.
- `battery` (top-level, per variable): title of the TRUE question battery the variable belongs to
  (membership + header text in one field, deliberately REPEATED on every member). Only true
  multi-answer batteries use this field — they alone get the boxed rendering, the dark-red valeur|n|freq
  rectangle, and the auto-added prefixe_question SELECTOR column. Written by ai_build_outline();
  preserved on re-extract.
- `keep_codes` (top-level, per variable, boolean): TRUE keeps the ORIGINAL level codes as the final
  numbering (prefix = the code's LEADING number `^\s*(\d+)`, zero-padded to the widest code; levels
  sorted by that number) instead of clean sequential numbering — for nomenclatures (region, month, PCS,
  age/year ranges…). A code must START with the ordering number: `"01 - GUADELOUPE"`→01, `"80-84"`→80,
  but a leading-text code like `"Avant 1930"` (or a duplicate number) makes the whole variable fall back
  to normal numbering, with a message naming the offending code. Set by `set_keep_codes(meta_json, vars)`
  or `extract_survey_metadata(keep_codes = c(...))` (additive, preserved on re-extract); candidates from
  `suggest_keep_codes()`. Applied in `.gfs_build_entries()`.
- `headers` (top-level, per variable): array of markdown outline titles (`"## ..."`, `"### ..."`,
  and `"#### ..."` for a non-battery thematic GROUP) rendered ONCE before this variable in the
  codebook. Start-markers, not repeated; the level = the count of `#` (clamped 2..4). The USER owns
  `##` (and optionally `###`), set the named-vector way `c("## Titre" = "VARNAME", ...)` via
  `extract_survey_metadata(df, meta_json, headers = titles)` OR `set_headers(meta_json, titles)`.
  The argument is source of truth for the USER levels (`##`/`###`): it clears+rewrites them and
  PRESERVES the AI `####` across re-extract (keeps only level-4 headers, then overlays the arg).
  ai_build_outline() owns `####` only — it writes non-battery thematic groups here (batteries go to
  the `battery` field). Survives the serializer (optional blocks after `examples` in
  `.write_meta_json`) and is carried onto codebook entries by `.gfs_build_entries()`.
- `num_stats`: mean/sd/min/q1/median/q3/max (numeric vars), each rounded to 5 digits. Field order
  is fixed in `.gfs_compute_numeric_stats()` (list) + `ns_fields` (serializer). NA moved OUT to
  top-level na_n/na_pct.

### SAS Format File Support

`parse_sas_formats()` parses SAS PROC FORMAT files to extract value labels.
`apply_sas_labels()` applies parsed labels to plain tibbles (haven_labelled output).
The mapping section (`data; set; format VAR $FMTf;`) links format names to variable names.

`apply_sas_value_labels(df, path)` is the recommended df-aware entry point: it reads a
SAS format **script** (not a `.sas7bcat` catalog) and attaches value labels to `df`
without changing the stored codes. Resolution is **case-insensitive** (INSEE scripts use
mixed/lower-case names while imported columns are upper-cased) and **df-aware for the
trailing-"f" convention**: an as-is match wins, and a single trailing "f" is stripped only
as a fallback — so a variable that legitimately ends in "f" (e.g. `PAP_TIR_SPTF` ←
`pap_tir_sptff`) is never truncated. Unmatched formats are reported, not silently dropped.
`apply_sas_labels()` is now also case-insensitive and takes `overwrite = FALSE`.

**Key Design Decision** — SAS var/format→column resolution is df-aware + case-insensitive;
strip-"f" is fallback-only and never overrides a real column match. This fixes the silent
no-op that `apply_sas_labels()` (case-sensitive) had on lower-case INSEE variables against
an all-upper-case df. Parsing is factored into `.parse_sas_value_blocks()` /
`.parse_sas_format_mapping()` / `.parse_sas_var_labels()`, shared by `parse_sas_formats()`
and `apply_sas_value_labels()`; resolution helpers are `.match_df_col()` /
`.resolve_sas_name_to_col()`.

**Key Design Decision** — `generate_codebook()` (section 9b in the source) builds a long
tibble (`.cb_build_tibble()`) then styles an xlsx (`.cb_write_xlsx()`, openxlsx2). It shares
`.gfs_build_entries()` + `.gfs_level_label()` with `generate_format_script()` so the `val`
column is byte-identical to the fct_recode LHS. Numeric summary values are written as exact
numbers with number formats decided **per value**: a whole-number stat uses `#,##0` (no decimals),
a fractional one `#,##0.0` (one decimal); factor `freq` is an Excel percentage (value stored as a
0–1 fraction, format `0%`); sd keeps `"σ"0.0`. Not pre-rounded text, so precision is preserved.
`type`/`role` labels come from
`.cb_type_label()` / `.cb_role_label()` (FR default, `lang="en"` option); `type` is derived
from `role` (+`r_class` only for identifier/other). A `factor_binary` with exactly 2
non-missing levels renders one row (positive/order-1 level; `orig_val` shows both labels
"Oui / Non"); if it has ≠2 levels it falls back to showing all levels and is flagged. Numeric
blocks: **mean+sd row first**, then max/Q3/median/Q1/min, thin rule between. The generated
**format script** label form (`"label" -> varlab` … `|> \`attr<-\`("label", varlab)`) applies
the label to the final converted object so it survives`factor(as.character())` /
`as.integer(...)`. No`df` param: text example values + NA come from the JSON (stored by
`metadata_add_level_stats`), so the codebook is fully JSON-driven.
`keep_original = TRUE` (forced in df-first mode) shows factor labels as-is, sorted by numeric
code, no ordering prefix and no binary 1-row collapse — via the `natural_order` path in
`.cb_build_tibble()`. Passing a **data frame** as the first arg builds a temp JSON silently
(extract + metadata_add_level_stats,`...` → extract) and sets `keep_original`.

**Key Design Decision** — Codebook xlsx layout (`.cb_write_xlsx`): column order
`h | variable | description | type | role | missing_values | valeur | n | freq | sep | orig_val | code`
(FR/EN headers via `.cb_headers`; `role` has no accent; `identifier`→`identifiant`/`identifier`; an
empty thin `sep` column separates the value block from the original-label block). All borders are
**black thin**. The `sep` + `orig_val`/`orig_code` borders (and the box extension over those columns)
are drawn **only for factor blocks** — the only ones that fill them; non-factor blocks are boxed
`variable → pct` with just the `val` left separator. Each factor block is boxed top+bottom (skipping
`h` + `sep`), independently, so adjacent battery members (now under one `####` header, no blank
spacers) keep their own box; `orig_val` a left border, `orig_code` a right border (rightmost). When the
JSON has ≥1 true battery, a rightmost `prefixe_question`/`question` column is added AUTOMATICALLY (no
arg) holding a `.battery_selector()` string (unique common prefix → `starts_with()`, else pipe-joined
names → `matches()`), merged into one wrapped cell per (contiguous) battery, styled outside the box.
Each true battery also gets a dark-red MEDIUM rectangle around its `valeur|n|freq` block
(`wb_add_border(update=TRUE)` overlay, after the xf palette — numfmt/fill preserved; `.battery` internal
col drives it). A top level-1 `#` title (`config.survey_title`) + ONE front-matter row per survey_*
field (each merging `description..valeur`, rich text via `.md_to_fmt_txt()`; `config.n_individuals` in
the `n` column of the survey_population row; explicit row height since merges don't auto-fit) precede the
variables. Header/title/front-matter rows carry no block borders.
`description` is always bold. Widths: `description` 72, `missing_values` 30, `orig_val` 60; `variable`
is 18 but widens to 27 only when the longest name would wrap. Section titles sit in column `h` and
**overflow** into the empty cells to their right: the data write uses `na = NULL` so trailing cells are
genuinely empty (writing `""` counts as content and clips them) — confirmed readable across the row
even with the freeze pane on. Internal `.cb_write_xlsx` args `title_mode` (`overflow` default,
`merge` fallback) + `freeze`. The
`missing_values` cell is built by the shared `.format_missing_summary()` (same string in the
format-script `# Valeurs manquantes` comment): `NA: <na_n> (<na_pct>%) ; <n1> <label1> ; … ; <n_blank>
vide` — **only missing levels with a real label** are listed (biggest→smallest); unlabelled coded
sentinels (e.g. numeric 999) fold into the `NA:` total, never shown by code; this applies to numeric
vars too. Genuine blanks (`na_n − Σ all counts`) appended last as `<n> vide`; only `NA: <n>` (front)
is bolded. TWO redundancy guards: NO labelled level → just `NA: n (pct%)` (no bare `<n> vide`); a SINGLE
labelled level whose count == na_n → `NA: n (pct%) ; <label>` (drop the repeated count). Graceful (any
missing level lacks `n`): plain labelled-only list (no counts / no `vide`).
It wraps for all types EXCEPT factor binaries (kept on one row). `orig_val`/`orig_code` never wrap;
text/other `valeur` = `Ex. : "v1", "v2", "v3", "v4", …` (4 values).

**Key Design Decision** — Codebook styling uses a **fixed xf palette** (openxlsx2 low-level), not
per-block `wb_add_*`. `.cb_write_xlsx()` computes each value cell's full appearance as a key
(`font | h | v | wrap | numfmt | top bot left right`), registers **one** `create_cell_style()` xf per
distinct key via `wb$styles_mgr$add()` (fonts/borders/numfmts deduped through `create_font`/
`create_border`/`create_numfmt` caches), then stamps it with a single `wb_set_cell_style(dims = comma-
joined cells, style = name)` per xf. This cut the ~500-var export from ~116 s to ~22 s and the style
catalog from ~1600 xfs to ~46 — with byte-identical appearance (verified by a per-cell border/numfmt/
bold/alignment diff). **Ordering constraint**: the merge + NA-rich-text calls clone the workbook, so the
styles manager is captured AFTER the block loop and the whole palette is registered BEFORE any
`wb_set_cell_style()` reassigns `wb` — otherwise later registrations land on an orphaned manager and are
lost on save. Base font set once via `wb_set_base_font()`; merges + rich-text NA prefix still run per
block (they set values, not xf). `sd` numfmt is `"σ"0.0`.

**Key Design Decision** — The whole codebook outline is **data-driven** (per-variable `headers`
array + `battery` title in the JSON) and built in **one AI pass**, `ai_build_outline()`, replacing
the old `detect_batteries()` + `ai_name_batteries()` two-step (and the `titles`/`binary_batteries`
codebook args, long gone). The outline SECTIONS are user-provided and fixed — the **`##` blocs**
always (documentation), plus optional **`###` subthemes** for big surveys. The AI owns a **single
level**, the **`####` group**, and covers EVERY variable with one (full coverage), so the codebook
reads as a table of contents. A `####` is either a **battery** or a **thematic group**; together
they leave nothing loose. Two storage concerns, deliberately split: `headers` holds the outline as
**start-markers** (`##`/`###` user anchors + non-battery `####` groups, rendered once, level = `#`
count clamped 2..4), and `battery` (REPEATED on every member) flags a **true multi-answer battery** —
which alone gets the boxed rendering + closing spacer + the auto-added prefixe_question selector column
and a dark-red valeur|n|freq rectangle. The codebook
renderer already treats a `####` in `headers` as a size-10 title, so non-battery groups need **no
renderer change**. Input to the model = every var in order with the fixed `##`/`###` sections
interleaved as `{"section":"..."}` rows + `config.survey_description` + a deterministic candidate
`batt` seed; output = **contiguous spans** `[{title, from, to, battery}]` (battery boolean, no level).
Applied authoritatively: clears the AI-owned level (`####` in `headers`, all `battery`), validates each
span (unknown/reversed/**crosses-##**/**crosses-###**/overlap rejected — a `####` stays inside one
`##` and one `###` section). `####` groups have **no minimum size** (they tile every section); a
`battery:true` below `min_size` is **demoted** to a thematic group (kept, not boxed) so coverage never
breaks. Writes non-battery `####` to `headers` and true batteries (>=`min_size`) to `battery`,
re-sorts headers outermost-first, and reports any variable left in no `####` (incomplete coverage);
JSON untouched on 0 valid spans. `seed=FALSE` → no seed. The internal `.batt_seed_candidates()` seed is
just a HINT: same role + level-code signature with a **precision gate** (must share a name-token prefix
OR a >=10-char label stem — role+codes alone is not a battery), prefix-cluster split, type-outlier heal
- flag; it does NOT persist and the prompt frames it as a candidate the AI overrides (and to keep
derived/recap variables OUT of batteries). `extract(headers=)` is source of truth for the USER levels
(`##`/`###`): it clears+rewrites them and keeps only the AI `####` across re-extract.
`ai_suggest_labels()` chunking never cuts a battery in two.
`preview_outline()` prints the full `##`/`###`/`####` markdown outline (batteries expanded).
Batteries are strictly **CONTIGUOUS same-role runs** — no mixed/interleaved batteries, no bridging a
mis-typed member. Interleaved (mixed) batteries are **reordered by hand before extract**, surfaced by
`check_batteries()` (which also flags type-outliers to fix); the AI only ever sees clean contiguous
batteries. Prompt = `instructions/outline_prompt.md` — 4 real pps20 examples teaching the seed-override:
one battery + a thematic group; **items battery + a SEPARATE recap battery** (≥3 parallel recaps →
their own `battery:true`, e.g. `ACTI_CULT`/`LECTURE`/`ACTU_EVEN_SPORT`); numeric-grid-is-a-battery +
computed indicators as a group; no-batteries standalones. `desc` truncated to 160 chars so the shared
question stem (often at the label's END) is visible.

**Key Design Decision** — `check_batteries()` is the pre-AI battery health check (run after extract,
before `ai_build_outline()`). Deterministic + read-only; reuses `.batt_signature` + `.batt_precision_ok`
(the seed's precision gate, factored out and shared). It reports (A) **interleaved batteries** —
same-signature groups that are **SHREDDED** (≥`min_size`, passing the precision gate, AND whose largest
contiguous cluster is `< min_size` — a group already forming ≥1 battery-sized block is legitimate
separate batteries, NOT flagged; this cluster gate is what keeps it quiet on a correctly-ordered survey)
— each with a copy-paste `relocate(all_of(c(...)), .after = ...)` that gathers them contiguous (apply on
`df`, then re-extract `recreate = TRUE`); and (B) **type-outliers** — a single variable whose `role` differs from its two
same-question neighbours (same signature, shared name prefix OR ≥10-char label stem, so it catches a
no-name-prefix case like `LIVRE` mis-typed `integer_count` among binaries). Console style mirrors
`suggest_keep_codes()`; returns `invisible(list(reorder, outliers))`.

**Key Design Decision** — A `battery` title must sit on a CONTIGUOUS run of variables (the codebook
merges/boxes each battery over one `[min,max]` span; the format script opens one fold per battery).
A manual JSON edit that mistypes/duplicates a title on non-consecutive variables (e.g. a dropped
leading letter splitting `"Nombre…"`/`"ombre…"`) would otherwise surface only as an opaque openxlsx2
`Merge intersects` crash (codebook) or a silent double fold box (format script). The shared guard
`.check_battery_contiguity(entries, fn)` — called right after `.gfs_build_entries()` in BOTH
`.cb_build_tibble()` and `generate_format_script()` — aborts early (`stop`, `call.=FALSE`) with a
French, `check_batteries()`-style message naming the split title, its non-consecutive variables, and
the intervening (usually typo'd) sibling title, so the JSON is fixable in seconds. `check_batteries()`
does NOT catch this (same-signature groups, not mistyped title text).

**Key Design Decision** — Missing-value flagging in `extract_survey_metadata()`. FACTOR levels: **exact**
by design — flagged `missing` only when the (normalized) label is literally in `config.missing_chr`, OR
the code is in `config.missing_num`, OR the label matches the conservative `missing_lbl_pattern` regex
(NSP/NR/REFUS/ne sait pas/non répondu/sans réponse). Tolerant/fuzzy matching was rejected (risks flagging
real levels), so a label variant like `"Non concerné(e)"` must be in `missing_chr` (or marked in the JSON).
NUMERIC vars: keep ONLY special codes as levels — a value in `missing_num` (always flagged), plus labelled
codes flagged `missing:true` ONLY when they are SPARSE sentinels: `n_extra_lab <= 2` (labelled, not already
missing) OR `n_unlabelled_obs > max_levels_cat`. A fully/mostly-labelled numeric (a label per value, e.g.
NB_PERS_DOM "1 personne".."9 personnes", forced to `integer_count`) is descriptive, NOT missing — its
labels are dropped and it stays a clean count (num_stats over the real values), never 100% NA. The extract
prints auto-flagged labelled codes; override with `"missing": false` for a rare real code (top-coding).
`.detect_role_v3()` uses the same label COVERAGE gate (`max_levels_cat`): a numeric column whose labels
cover only a few of many observed values is a partially-labelled numeric, not a factor. A bare numeric with
exactly 2 distinct values ⊆ {0,1} and NO value labels → `factor_binary` (Non/Oui synthesized, positive=code
"1"). Value-label codes sort NUMERICALLY when all integer-like (else lexical → "10" between "1" and "2");
same numeric sort for observed-but-undeclared codes in `metadata_add_level_stats()`. NOTE: `order` and
`role` are PRESERVED across re-extract (and `ai_classify_roles` skips binary order once a level has
`order:1`), so a stale order set before the missing-config was complete stays frozen — finalize
`missing_chr`/yes-no labels BEFORE the first extract, or fix that variable's `order` in the JSON once.
Separately, `ai_classify_roles()` never writes `factor_binary` without exactly 2 non-missing levels (→
`factor_nominal`) — the single "born-consistent" guard.

**Key Design Decision** — Empty levels & the removal of `factor_unique_value`. A declared value label
whose code is never observed in the data is a legitimate EMPTY level, not an error (Stata/SPSS/SAS value
labels are metadata independent of the data; `labelled::to_factor(drop_unused_labels = FALSE)` keeps
them). `extract_survey_metadata(empty_levels = c("small_factors","all","none"))` decides: default
`"small_factors"` keeps unobserved codes as levels when the var declares ≤ `max_levels_cat` codes (else
drops them — over-declared shared label sets); `"all"` always keeps; `"none"` is the classic inner-join
drop. Applied in the `has_val_labs` + `is.factor` branches via `.keep_empty_levels(observed, n_declared,
mode, max_levels_cat)`; an `is_observed` mask is threaded to (a) confine empties to FACTORS (numeric-role
pruning intersects with `is_observed`) and (b) flag each kept-unobserved level `n: 0` at creation
(visible in manual review; re-confirmed by `metadata_add_level_stats`). Consequence: a labelled 0/1 with
only "Non" observed keeps both poles → `factor_binary` (Oui `order 1`, empty, `n:0`), so binary batteries
never lose a member's level set. `factor_unique_value` is **deleted** — the `ai_classify_roles()` auto-nd1
`else` branch now leaves a genuine single-category factor as its detected `factor_nominal`.
`generate_format_script()` emits `factor(as.character(x)) |> fct_expand(<empty codes>)` before
`fct_recode` (only when an empty level exists) so the pole survives with no forcats warning. Q2 choice:
unlabelled single-value numerics stay `integer_count` (no `.detect_role_v3` change).

**Key Design Decision** — `metadata_add_level_stats(meta_json, df, add_observed_levels = TRUE,
max_new_levels = 50L)` adds, for **factor** variables, value codes present in `df` but absent from the
JSON value labels (e.g. a level missing from the SAS format script). They get an empty `label` (flagged
for manual review — fill it or mark `missing`), a provisional `order` after the current max, and are
counted in `n`/`pct` as ordinary non-missing levels; a per-variable count above `max_new_levels` is
reported but not added (likely a nomenclature). Done here (post role-classification) so numeric vars
mis-detected as factors don't accumulate spurious levels. Empty-label levels are skipped by
`ai_suggest_labels()`, and `.gfs_build_entries()` falls back to the code (via `.first_nzchar()`) for
display so `generate_format_script()`/`generate_codebook()` stay clean until the label is filled.

**Key Design Decision** — Final level numbering (the `NN-Label` prefix) has THREE modes, all decided in
the shared `.gfs_build_entries()` so `generate_format_script()` and `generate_codebook()` stay identical:
(1) **default** = sequential per-level `order`, zero-padded to `nchar(max_order)` via `.gfs_numeric_prefix()`;
(2) **binary-battery** = automatic for every battery whose members are all `factor_binary` — positive level
gets its battery position (1,2,3…), the negative an all-nines sentinel `.nines_sentinel(N)` (N≤8→9, 9–98→99,
99–998→999); width tracks battery size. Mixed batteries are skipped + reported. (3) **keep_codes** = the
code's leading number as prefix (`^\s*(\d+)`, zero-padded to the widest code, stored per-level as
`num_prefix`, sorted by code); leading-text or duplicate-number codes fall back to mode 1 with a message.
`.gfs_level_label()` prefers `num_prefix` when present. `suggest_keep_codes()` is a deterministic,
content-based console detector (sibling of `detect_nomenclature_vars()`) for mode-3 candidates: it flags a
factor by **variable name** (PCS/CS/GS/REGION/DEP/COMMUNE/MOIS/geo-typologies…), **label vocabulary**
(≥3 French regions, ≥2 PCS niveau-1/2 stems, ≥3 months, ≥2 "NN ans" classes, "décile"), or **`codes non
entiers`** (codes not round-tripping as plain integers: `01`, `80-84`, `01 - GUADELOUPE`, PCS `311a`). It
deliberately does NOT use code contiguity or display-vs-code order (those flag ordinary Likert batteries);
commune names / generic geo codes are left to the name rule (unreliable by content).

---

## Test Suite Design

### Running Tests

Since this is not a package, directly source the test main script, which should internally use `testthat::test_dir`.

```r
# In a temp .R file (outside tests/), then run:  Rscript that_file.R   (isolated; tests live source)
source("tests/testthat.R", encoding="UTF-8")
```

### Shared Fixtures (`tests/testthat.R`)

All dummy datasets, missing-value configs, expected roles, and helpers live here.
Test files reference them by name (e.g., `.virage_dummy`, `.emploi_expected_roles`).

**Three dummy datasets:**

| Dataset         | Type            | Rows | Vars | Source                      |
|-----------------|-----------------|------|------|-----------------------------|
| `.virage_dummy` | haven_labelled  | 30   | 6    | Real Virage survey extract  |
| `.emploi_dummy` | plain chr/num   | 30   | 5    | Real Enquête Emploi extract |
| `.edge_dummy`   | mixed synthetic | 10   | 6    | Hand-crafted edge cases     |

Each dummy has matching configs:
- `.{name}_missing_num`, `.{name}_missing_chr` — missing value codes
- `.{name}_yes_labels`, `.{name}_no_labels` — binary detection labels
- `.{name}_expected_roles` — named character vector of expected role per variable

**Helpers:**
- `tmp_json()` — creates a temp JSON path
- `make_meta_list(vars)` — builds a `list(config=..., variables=vars)` suitable for `.write_meta_json()`
- `mock_ai(text)` — returns a function that mimics `ai_call_claude()` returning `text`
- `extract_dummy_meta(dummy, ...)` — wrapper around `extract_survey_metadata()` for tests

### Adding a New Dummy Dataset

1. Use `make_dummy_tibble(real_df, n = 30)` on real data to extract a representative sample
2. Copy-paste the `dput()` output into `tests/testthat.R` as `.new_dummy`
3. Add `.new_missing_num`, `.new_missing_chr`, `.new_yes_labels`, `.new_no_labels`
4. Add `.new_expected_roles` — run `extract_dummy_meta()` first to see detected roles
5. Add test cases in `test-extract-metadata.R` following the E1-E3 pattern

### Test File Organization

| File                            | Prefix | What it tests                                              |
|---------------------------------|--------|------------------------------------------------------------|
| `test-extract-metadata.R`       | E      | Role detection for all 3 dummies + regression cases        |
| `test-sas-format-parser.R`      | P      | `parse_sas_formats()` and `apply_sas_labels()` unit tests  |
| `test-sas-value-labels.R`       | V      | `apply_sas_value_labels()` df-aware value-label apply      |
| `test-pipeline-integration.R`   | INT    | End-to-end pipeline with mocked AI calls                   |
| `test-ai-classify-roles.R`      | A/AC   | `ai_classify_roles()` logic + auto-classification          |
| `test-ai-suggest-labels.R`      | L/B    | `ai_suggest_labels()` prompt building + JSON writing       |
| `test-ai-merge-levels.R`        | M      | `ai_merge_levels()` logic                                  |
| `test-generate-format-script.R` | G/CV/H | `generate_format_script()` + level-label / stats-comment   |
| `test-generate-codebook.R`      | C      | `generate_codebook()` tibble build + xlsx write            |
| `test-outline-seed.R`           | D      | `.batt_seed_candidates()` seed + precision gate + preview  |
| `test-ai-build-outline.R`       | OU     | `ai_build_outline()` #### spans → headers/battery (mock)   |
| `test-keep-codes.R`             | KC     | keep_codes numbering + set_keep_codes / suggest_keep_codes |
| `test-json-roundtrip.R`         | J/K/BT | JSON roundtrip, backup, migration, battery/headers fields  |
| `test-nomenclatures-insee.R`    | O      | INSEE nomenclature helpers                                 |

### Mocking AI Calls

The project uses `assign()/on.exit()` for mocking — NOT `withr::local_bindings()` (not available):

```r
.orig_ai <- get("ai_call_claude", envir = globalenv())
on.exit(assign("ai_call_claude", .orig_ai, envir = globalenv()), add = TRUE)
assign("ai_call_claude", mock_ai(response_text), envir = globalenv())
```

### Known Constraints

- `\uXXXX` unicode escapes do NOT work inside backtick-quoted R names — use double-quoted names
- `metadata_add_level_stats()` must run before `ai_suggest_labels()` (needs n/pct)
- `metadata_add_level_stats()` must run before `generate_format_script()` / `generate_codebook()`
  for numeric stats, factor NA rates (`config.n_individuals`) and `num_stats.na_n/na_pct`
- `generate_codebook()` reads the JSON only; pass `df` to list example values for text/other vars
- Do NOT construct metadata tibbles in tests — use JSON write + `.load_meta()` roundtrip pattern
- SAS inline format string `.sas_emploi_inline` is shared — don't redefine in test files
- Adding a new `config.*` scalar requires updating the `cfg_fields` allow-list in `.write_meta_json()`

---

## Code Style

- Base pipe `|>`, never `%>%`
- Explicit namespace: `dplyr::filter()`, `purrr::map()`, etc.
- Double quotes for strings
- French accented characters use `\uXXXX` escapes in test data (e.g., `\u00e9` for é)
- Variable names in data are always turned UPPER_CASE in `import_survey()`
- R function names are snake_case with dots for internal helpers (e.g., `.detect_role_v3`)
- Read JSON level fields with EXACT `lv[["n"]]` / `lv[["pct"]]`, never `lv$n` — R `$` partial-matches
  and would resolve `lv$n` to `lv$new_label` on a level that has no `n` (silent NA-coercion warnings)

---

## AI Integration

- All AI calls go through `ai_call_claude()` / `ai_batch_submit()` (Anthropic API)
- Default model: Sonnet 5 (`claude-sonnet-5`) via the `.DEFAULT_AI_MODEL` constant. Haiku 4.5 is legacy.
- Request body built by `.build_message_body()` (shared by call + batch). Reasoning-tier models
  (Sonnet 5 / Opus 4.8 family, gated by `.is_reasoning_tier_model()`) get `thinking:{type:"adaptive"}`
  + `output_config.effort="low"` and `+.AI_THINKING_HEADROOM` on `max_tokens`; Haiku/older models get
  the plain body (they 400 on effort/adaptive thinking). No `temperature`/`top_p`/`top_k` ever.
- Response text read via `.ai_extract_text()` (skips the leading adaptive-thinking block).
  `.warn_if_truncated()` warns on `stop_reason=="max_tokens"` (sync + per batch item) so batches
  are never silently lost.
- Chunk sizes raised ~2–3× for Sonnet 5's 1M context (classify `chunk_size` 1000, varnames 800,
  labels `max_levels` 400, merge `max_levels` 600). All four auto-scale `max_tokens` from their
  chunk budget (labels/varnames no longer fixed), clamped to 128K.
- All four AI functions cache their system prompt via `cache_control` (labels/varnames were
  previously uncached).
- Labels + merge parse chunks via `.parse_var_object_chunk()`: whole-object parse, then a
  per-variable recovery fallback (`.extract_var_objects()` + `.match_json_delim()`) so one stray
  brace on a large chunk no longer discards every variable in it (varnames already had this).
  `ai_suggest_labels(..., resume_batch_id="msgbatch_...")` re-parses an existing batch (free
  recovery); raw chunk responses cached to `tempdir()/labels_cache` via `.cache_ai_raw()`.
- AI prompts are built by `build_*_prompt()` functions, sent in chunks
- `dry_run = TRUE` returns prompts without calling API (useful for debugging)
- NEVER use AI API calls in tests to avoid costs (mock them instead)

## CLAUDE.md Update Instruction

When you modify the package structure (add modules, rename functions, change config fields), suggest the relevant CLAUDE.md update lines in your response : it should be minimalistic, concice, no bullshit, with nothing useless that would clutter the prompt. When there is nothing to change, skip it.
