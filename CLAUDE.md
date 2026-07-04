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
     labelled codes, all flagged missing:true); plain data-range values never
     become levels. Sparsely-labelled numerics stay numeric, not factor (coverage).
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

6. generate_format_script(meta_json, output_path = NULL)
   → Generates executable R script that applies all formatting
   → Reads numeric stats from JSON (run metadata_add_level_stats() first)
   → Simplified: no codebook / no "## Variable list" / no "# Select and reorder" sections;
     each block applies its var label inline via
     "label" -> varlab  then  ... |> `attr<-`("label", varlab)  (survives conversion)

7. generate_codebook(meta_json, output_path = NULL, lang = "fr", titles = NULL,
                      binary_batteries = NULL, keep_original = FALSE, ...)
   → Styled .xlsx codebook (openxlsx2): one row per level / numeric stat, variable
     info merged over rows, section titles, frozen panes, selective borders.
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
- `levels.{code}.n`: written for every level incl. missing (missing-value counts); `pct` non-missing only
- `levels.{code}.order`: integer for ordinal level ordering (missing levels have none)
- `config.n_individuals`: total row count (written by extract_survey_metadata / backfilled by
  metadata_add_level_stats).
- `na_n` / `na_pct` (top-level, per variable, ALL types): count/percent of individuals NA after
  formatting = NA + missing-coded. Written by metadata_add_level_stats. Codebook prefers these;
  falls back to n_individuals − Σ(non-missing level n) for factors on older JSONs.
- `examples` (top-level, text/"other" vars only): first 5 distinct raw values, for the codebook.
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
the label to the final converted object so it survives `factor(as.character())` /
`as.integer(...)`. No `df` param: text example values + NA come from the JSON (stored by
`metadata_add_level_stats`), so the codebook is fully JSON-driven.
`keep_original = TRUE` (forced in df-first mode) shows factor labels as-is, sorted by numeric
code, no ordering prefix and no binary 1-row collapse — via the `natural_order` path in
`.cb_build_tibble()`. Passing a **data frame** as the first arg builds a temp JSON silently
(extract + metadata_add_level_stats, `...` → extract) and sets `keep_original`.

**Key Design Decision** — Codebook xlsx layout (`.cb_write_xlsx`): column order
`h | variable | description | type | role | missing_values | valeur | n | freq | sep | orig_val | code`
(FR/EN headers via `.cb_headers`; `role` has no accent; `identifier`→`identifiant`/`identifier`; an
empty thin `sep` column separates the value block from the original-label block). All borders are
**black thin**. The `sep` + `orig_val`/`orig_code` borders (and the box extension over those columns)
are drawn **only for factor blocks** — the only ones that fill them; non-factor blocks are boxed
`variable → pct` with just the `val` left separator. Each factor block is boxed top+bottom (skipping
`h` + `sep`) so battery runs separated by spacer rows keep their upper border; `orig_val` a left
border, `orig_code` a right border (rightmost). Header/empty/title rows carry no block borders.
`description` is always bold. Widths: `description` 72, `missing_values` 30, `orig_val` 60; `variable`
is 18 but widens to 27 only when the longest name would wrap. Section titles sit in column `h` and
**overflow** into the empty cells to their right: the data write uses `na = NULL` so trailing cells are
genuinely empty (writing `""` counts as content and clips them). Internal `.cb_write_xlsx` args
`title_mode` (`overflow`/`merge`/`centercont`) + `freeze` exist to compare renderings. The
`missing_values` cell is built by the shared `.format_missing_summary()` (same string in the
format-script `# Valeurs manquantes` comment): `NA: <na_n> (<na_pct>%) ; <n1> <label1> ; … ; <n_blank>
vide` — **only missing levels with a real label** are listed (biggest→smallest); unlabelled coded
sentinels (e.g. numeric 999) fold into the `NA:` total, never shown by code; this applies to numeric
vars too. Genuine blanks (`na_n − Σ all counts`) appended last as `<n> vide`; only `NA: <n>` (front)
is bolded. Graceful (any missing level lacks `n`): plain labelled-only list (no counts / no `vide`).
It wraps for all types EXCEPT factor binaries (kept on one row). `orig_val`/`orig_code` never wrap;
text/other `valeur` = `Ex. : "v1", "v2", "v3", "v4", …` (4 values).

**Key Design Decision** — Missing-value flagging in `extract_survey_metadata()`. FACTOR levels: **exact**
by design — flagged `missing` only when the (normalized) label is literally in `config.missing_chr`, OR
the code is in `config.missing_num`, OR the label matches the conservative `missing_lbl_pattern` regex
(NSP/NR/REFUS/ne sait pas/non répondu/sans réponse). Tolerant/fuzzy matching was rejected (risks flagging
real levels), so a label variant like `"Non concerné(e)"` must be in `missing_chr` (or marked in the JSON).
NUMERIC vars: keep ONLY special codes as levels — a value in `missing_num` OR any value carrying a genuine
value label (on a numeric column a label always marks a special/non-response code) → all flagged
`missing:true` (auto-flag; the extract prints which labelled codes it flagged, override with
`"missing": false` for a rare real code like top-coding). `.detect_role_v3()` uses label COVERAGE
(`max_levels_cat`, revived): a numeric column whose labels cover only a few of many observed values is a
partially-labelled numeric, not a factor. Separately, `ai_classify_roles()` never writes `factor_binary`
without exactly 2 non-missing levels (→ `factor_nominal`) — the single "born-consistent" guard.

**Key Design Decision** — `metadata_add_level_stats(meta_json, df, add_observed_levels = TRUE,
max_new_levels = 50L)` adds, for **factor** variables, value codes present in `df` but absent from the
JSON value labels (e.g. a level missing from the SAS format script). They get an empty `label` (flagged
for manual review — fill it or mark `missing`), a provisional `order` after the current max, and are
counted in `n`/`pct` as ordinary non-missing levels; a per-variable count above `max_new_levels` is
reported but not added (likely a nomenclature). Done here (post role-classification) so numeric vars
mis-detected as factors don't accumulate spurious levels. Empty-label levels are skipped by
`ai_suggest_labels()`, and `.gfs_build_entries()` falls back to the code (via `.first_nzchar()`) for
display so `generate_format_script()`/`generate_codebook()` stay clean until the label is filled.

---

## Test Suite Design

### Running Tests

```r
# In a temp .R file (outside tests/), then run:  Rscript that_file.R   (isolated; tests live source)
devtools::test("d:/Statistiques/github/socio_public_services")                  # whole suite (~46s)
devtools::test("d:/Statistiques/github/socio_public_services", filter = "tab")  # one/few files: regex on test-<name>.R
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

| File                            | Prefix | What it tests                                             |
|---------------------------------|--------|-----------------------------------------------------------|
| `test-extract-metadata.R`       | E      | Role detection for all 3 dummies + regression cases       |
| `test-sas-format-parser.R`      | P      | `parse_sas_formats()` and `apply_sas_labels()` unit tests |
| `test-sas-value-labels.R`       | V      | `apply_sas_value_labels()` df-aware value-label apply      |
| `test-pipeline-integration.R`   | INT    | End-to-end pipeline with mocked AI calls                  |
| `test-ai-classify-roles.R`      | A/AC   | `ai_classify_roles()` logic + auto-classification         |
| `test-ai-suggest-labels.R`      | L/B    | `ai_suggest_labels()` prompt building + JSON writing      |
| `test-ai-merge-levels.R`        | M      | `ai_merge_levels()` logic                                 |
| `test-generate-format-script.R` | G/CV/H | `generate_format_script()` + level-label / stats-comment  |
| `test-generate-codebook.R`      | C      | `generate_codebook()` tibble build + xlsx write           |
| `test-json-roundtrip.R`         | J/K    | JSON read/write roundtrip, backup, migration helpers      |
| `test-nomenclatures-insee.R`    | O      | INSEE nomenclature helpers                                |

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
