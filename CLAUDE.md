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

---

## Test Suite Design

### Running Tests

```r
source("tests/testthat.R", encoding = "UTF-8")
```

Always use this runner — never run individual test files directly. They depend on shared
fixtures loaded by the runner.

### Shared Fixtures (`tests/testthat.R`)

All dummy datasets, missing-value configs, expected roles, and helpers live here.
Test files reference them by name (e.g., `.virage_dummy`, `.emploi_expected_roles`).

**Three dummy datasets:**

| Dataset | Type | Rows | Vars | Source |
|---------|------|------|------|--------|
| `.virage_dummy` | haven_labelled | 30 | 6 | Real Virage survey extract |
| `.emploi_dummy` | plain chr/num | 30 | 5 | Real Enquête Emploi extract |
| `.edge_dummy` | mixed synthetic | 10 | 6 | Hand-crafted edge cases |

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

| File | Prefix | What it tests |
|------|--------|---------------|
| `test-extract-metadata.R` | E | Role detection for all 3 dummies + regression cases |
| `test-sas-format-parser.R` | P | `parse_sas_formats()` and `apply_sas_labels()` unit tests |
| `test-pipeline-integration.R` | INT | End-to-end pipeline with mocked AI calls |
| `test-ai-classify-roles.R` | A/AC | `ai_classify_roles()` logic + auto-classification |
| `test-ai-suggest-labels.R` | L/B | `ai_suggest_labels()` prompt building + JSON writing |
| `test-ai-merge-levels.R` | M | `ai_merge_levels()` logic |
| `test-generate-format-script.R` | G | `generate_format_script()` code generation |
| `test-json-roundtrip.R` | J/K | JSON read/write roundtrip, backup, migration helpers |
| `test-nomenclatures-insee.R` | O | INSEE nomenclature helpers |

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

- All AI calls go through `ai_call_claude()` which calls the Anthropic API
- Default model: Haiku 4.5 (`claude-haiku-4-5`) for cost efficiency
- AI prompts are built by `build_*_prompt()` functions, sent in chunks
- `dry_run = TRUE` returns prompts without calling API (useful for debugging)
- NEVER use AI API calls in tests to avoid costs (mock them instead)
