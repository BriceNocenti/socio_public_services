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
   → Returns invisible(survey_meta) — enables |> piping

2. ai_classify_roles(meta_json, ...)
   → AI disambiguates ordinal vs nominal, writes role + desc + order to JSON

3. metadata_add_level_stats(meta_json, df)
   → Adds n/pct counts per level to JSON (required before ai_suggest_labels)

4. ai_suggest_labels(meta_json, ...)
   → AI suggests short display labels, writes new_label to JSON levels

5. ai_suggest_varnames(meta_json, ...)
   → AI suggests short variable names, writes new_name to JSON

6. generate_format_script(meta_json, output_path = NULL)
   → Generates executable R script that applies all formatting
   → Reads numeric stats from JSON (run metadata_add_level_stats() first)
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
- `levels.{code}.missing`: TRUE for missing-value levels (old `null_coded` field renamed)
- `levels.{code}.order`: integer for ordinal level ordering

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
| `test-generate-format-script.R` | G      | `generate_format_script()` code generation                |
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
- `metadata_add_level_stats()` must run before `generate_format_script()` for numeric stats
- Do NOT construct metadata tibbles in tests — use JSON write + `.load_meta()` roundtrip pattern
- SAS inline format string `.sas_emploi_inline` is shared — don't redefine in test files
- Pre-existing P5 test failure: prompt file `instructions/classify_roles_prompt.md` is missing

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
- AI prompts are built by `build_*_prompt()` functions, sent in chunks
- `dry_run = TRUE` returns prompts without calling API (useful for debugging)
- NEVER use AI API calls in tests to avoid costs (mock them instead)

## CLAUDE.md Update Instruction

When you modify the package structure (add modules, rename functions, change config fields), suggest the relevant CLAUDE.md update lines in your response : it should be minimalistic, concice, no bullshit, with nothing useless that would clutter the prompt. When there is nothing to change, skip it.
