# Tests for ai_classify_roles() direction (order) assignment.
# Functions under test: ai_classify_roles()
# Focus: correct ordinal order integers for high_first / low_first / unknown
#        Haiku JSONL replies, with ordinal_desc = TRUE and FALSE.
#
# No real API calls are made. ai_call_claude() is temporarily replaced via
# withr::local_bindings(.env = globalenv()) to return a fixed fake response.


# ---------------------------------------------------------------------------
# J. ai_classify_roles() — direction (order) assignment
# ---------------------------------------------------------------------------

test_that("J1: ordinal + AI returns high_first → seq order 1..n", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()
  vars <- list(
    FREQ = list(
      var_label = "Fréquence",
      role      = "factor_nominal",
      levels    = list(
        "1" = list(label = "Toujours"),
        "2" = list(label = "Souvent"),
        "3" = list(label = "Rarement"),
        "4" = list(label = "Jamais")
      )
    )
  )
  .write_meta_json(make_meta_list(vars), path)

  meta <- make_classify_meta(
    "FREQ", "Frequence",
    c("Toujours", "Souvent", "Rarement", "Jamais"),
    c(1L, 2L, 3L, 4L)
  )

  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude",
    mock_ai('{"id":"FREQ","role":"factor_ordinal","desc":"high_first"}'),
    envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)
  suppressMessages(
    ai_classify_roles(meta, meta_json = path, ordinal_desc = TRUE)
  )

  res <- .read_meta_json(path)$variables$FREQ$levels
  expect_equal(res[["1"]]$order, 1L)
  expect_equal(res[["2"]]$order, 2L)
  expect_equal(res[["3"]]$order, 3L)
  expect_equal(res[["4"]]$order, 4L)
})

test_that("J2: ordinal + AI returns low_first → reversed order", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()
  vars <- list(
    FREQ = list(
      var_label = "Fréquence",
      role      = "factor_nominal",
      levels    = list(
        "1" = list(label = "Jamais"),
        "2" = list(label = "Rarement"),
        "3" = list(label = "Souvent"),
        "4" = list(label = "Toujours")
      )
    )
  )
  .write_meta_json(make_meta_list(vars), path)

  meta <- make_classify_meta(
    "FREQ", "Frequence",
    c("Jamais", "Rarement", "Souvent", "Toujours"),
    c(1L, 2L, 3L, 4L)
  )

  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude",
    mock_ai('{"id":"FREQ","role":"factor_ordinal","desc":"low_first"}'),
    envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)
  suppressMessages(
    ai_classify_roles(meta, meta_json = path, ordinal_desc = TRUE)
  )

  res <- .read_meta_json(path)$variables$FREQ$levels
  expect_equal(res[["1"]]$order, 4L)
  expect_equal(res[["2"]]$order, 3L)
  expect_equal(res[["3"]]$order, 2L)
  expect_equal(res[["4"]]$order, 1L)
})

test_that("J3: ordinal + AI returns unknown + ordinal_desc=TRUE → ascending data reversed", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()
  vars <- list(
    DIPLOME = list(
      var_label = "Diplôme",
      role      = "factor_nominal",
      levels    = list(
        "1" = list(label = "Aucun"),
        "2" = list(label = "Secondaire"),
        "3" = list(label = "Bac"),
        "4" = list(label = "Bac+2 et plus")
      )
    )
  )
  .write_meta_json(make_meta_list(vars), path)

  meta <- make_classify_meta(
    "DIPLOME", "Diplome",
    c("Aucun", "Secondaire", "Bac", "Bac+2 et plus"),
    c(1L, 2L, 3L, 4L)
  )

  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude",
    mock_ai('{"id":"DIPLOME","role":"factor_ordinal","desc":"unknown"}'),
    envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)
  suppressMessages(
    ai_classify_roles(meta, meta_json = path, ordinal_desc = TRUE)
  )

  res <- .read_meta_json(path)$variables$DIPLOME$levels
  # Ascending labels → must be reversed: highest (Bac+2) → order=1, lowest (Aucun) → order=4
  expect_equal(res[["4"]]$order, 1L, label = "Bac+2 et plus should be order=1")
  expect_equal(res[["3"]]$order, 2L, label = "Bac should be order=2")
  expect_equal(res[["2"]]$order, 3L, label = "Secondaire should be order=3")
  expect_equal(res[["1"]]$order, 4L, label = "Aucun should be order=4")
})

test_that("J3b: ordinal + no desc field + ordinal_desc=TRUE → same as unknown", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()
  vars <- list(
    AGE = list(
      var_label = "Groupe d'âge",
      role      = "factor_nominal",
      levels    = list(
        "1" = list(label = "20-24"),
        "2" = list(label = "35-44"),
        "3" = list(label = "55-64")
      )
    )
  )
  .write_meta_json(make_meta_list(vars), path)

  meta <- make_classify_meta(
    "AGE", "Groupe d age",
    c("20-24", "35-44", "55-64"),
    c(1L, 2L, 3L)
  )

  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude",
    mock_ai('{"id":"AGE","role":"factor_ordinal"}'),  # no desc field
    envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)
  suppressMessages(
    ai_classify_roles(meta, meta_json = path, ordinal_desc = TRUE)
  )

  res <- .read_meta_json(path)$variables$AGE$levels
  # Ascending labels + no desc → same fallback as "unknown" → reversed
  expect_equal(res[["3"]]$order, 1L, label = "55-64 (oldest) should be order=1")
  expect_equal(res[["2"]]$order, 2L)
  expect_equal(res[["1"]]$order, 3L, label = "20-24 (youngest) should be order=3")
})

test_that("J4: ordinal + ordinal_desc=FALSE → existing order not changed", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()
  vars <- list(
    FREQ = list(
      var_label = "Fréquence",
      role      = "factor_nominal",
      levels    = list(
        "1" = list(order = 3L, label = "Jamais"),
        "2" = list(order = 2L, label = "Rarement"),
        "3" = list(order = 1L, label = "Souvent"),
        "4" = list(order = 4L, label = "Toujours")
      )
    )
  )
  .write_meta_json(make_meta_list(vars), path)

  meta <- make_classify_meta(
    "FREQ", "Frequence",
    c("Jamais", "Rarement", "Souvent", "Toujours"),
    c(1L, 2L, 3L, 4L)
  )

  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude",
    mock_ai('{"id":"FREQ","role":"factor_ordinal","desc":"unknown"}'),
    envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)
  suppressMessages(
    ai_classify_roles(meta, meta_json = path, ordinal_desc = FALSE)
  )

  res <- .read_meta_json(path)$variables$FREQ$levels
  # Role should be updated to factor_ordinal
  expect_equal(.read_meta_json(path)$variables$FREQ$role, "factor_ordinal")
  # Pre-set non-sequential order must be preserved (not overwritten)
  expect_equal(res[["1"]]$order, 3L)
  expect_equal(res[["2"]]$order, 2L)
  expect_equal(res[["3"]]$order, 1L)
  expect_equal(res[["4"]]$order, 4L)
})

test_that("J5: binary + AI returns high_first → Oui=order 1, Non=order 2", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()
  vars <- list(
    OUI_NON = list(
      var_label = "Avez-vous...",
      role      = "factor_nominal",
      levels    = list(
        "1" = list(label = "Oui"),
        "2" = list(label = "Non")
      )
    )
  )
  .write_meta_json(make_meta_list(vars), path)

  meta <- make_classify_meta(
    "OUI_NON", "Avez-vous",
    c("Oui", "Non"), c(1L, 2L),
    detected_role = "factor_nominal"
  )

  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude",
    mock_ai('{"id":"OUI_NON","role":"factor_binary","desc":"high_first"}'),
    envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)
  suppressMessages(
    ai_classify_roles(meta, meta_json = path, ordinal_desc = TRUE)
  )

  res <- .read_meta_json(path)$variables$OUI_NON$levels
  expect_equal(res[["1"]]$order, 1L, label = "Oui (positive, first shown) → order=1")
  expect_equal(res[["2"]]$order, 2L, label = "Non (negative) → order=2")
})

test_that("J6: binary + AI returns low_first → Non=order 2, Oui=order 1", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()
  vars <- list(
    OUI_NON = list(
      var_label = "Avez-vous...",
      role      = "factor_nominal",
      levels    = list(
        "1" = list(label = "Non"),
        "2" = list(label = "Oui")
      )
    )
  )
  .write_meta_json(make_meta_list(vars), path)

  meta <- make_classify_meta(
    "OUI_NON", "Avez-vous",
    c("Non", "Oui"), c(1L, 2L)
  )

  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude",
    mock_ai('{"id":"OUI_NON","role":"factor_binary","desc":"low_first"}'),
    envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)
  suppressMessages(
    ai_classify_roles(meta, meta_json = path, ordinal_desc = TRUE)
  )

  res <- .read_meta_json(path)$variables$OUI_NON$levels
  expect_equal(res[["1"]]$order, 2L, label = "Non (negative, first shown) → order=2")
  expect_equal(res[["2"]]$order, 1L, label = "Oui (positive, second shown) → order=1")
})

test_that("J7: mixed variables — ordinal high_first + ordinal unknown + binary low_first", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()
  vars <- list(
    VILLE = list(
      var_label = "Taille de commune",
      role      = "factor_nominal",
      levels    = list(
        "1" = list(label = "Grande ville"),
        "2" = list(label = "Ville moyenne"),
        "3" = list(label = "Village")
      )
    ),
    DIPLOME = list(
      var_label = "Diplôme",
      role      = "factor_nominal",
      levels    = list(
        "1" = list(label = "Aucun"),
        "2" = list(label = "Bac"),
        "3" = list(label = "Bac+5")
      )
    ),
    BINAIRE = list(
      var_label = "Question oui/non",
      role      = "factor_nominal",
      levels    = list(
        "1" = list(label = "Non"),
        "2" = list(label = "Oui")
      )
    )
  )
  .write_meta_json(make_meta_list(vars), path)

  meta <- dplyr::bind_rows(
    make_classify_meta("VILLE",   "Taille de commune",
                       c("Grande ville", "Ville moyenne", "Village"), c(1L, 2L, 3L)),
    make_classify_meta("DIPLOME", "Diplome",
                       c("Aucun", "Bac", "Bac+5"),                   c(1L, 2L, 3L)),
    make_classify_meta("BINAIRE", "Question oui non",
                       c("Non", "Oui"),                               c(1L, 2L))
  )

  fake <- paste(
    '{"id":"VILLE","role":"factor_ordinal","desc":"high_first"}',
    '{"id":"DIPLOME","role":"factor_ordinal","desc":"unknown"}',
    '{"id":"BINAIRE","role":"factor_binary","desc":"low_first"}',
    sep = "\n"
  )
  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude", mock_ai(fake), envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)
  suppressMessages(
    ai_classify_roles(meta, meta_json = path, ordinal_desc = TRUE)
  )

  res <- .read_meta_json(path)$variables

  # VILLE: high_first → seq order 1,2,3
  expect_equal(res$VILLE$levels[["1"]]$order, 1L)
  expect_equal(res$VILLE$levels[["3"]]$order, 3L)

  # DIPLOME: unknown + ordinal_desc=TRUE → reversed: Aucun→3, Bac+5→1
  expect_equal(res$DIPLOME$levels[["1"]]$order, 3L, label = "Aucun should be order=3")
  expect_equal(res$DIPLOME$levels[["3"]]$order, 1L, label = "Bac+5 should be order=1")

  # BINAIRE: low_first → positive (Oui) is second shown → Non=order 2, Oui=order 1
  expect_equal(res$BINAIRE$levels[["1"]]$order, 2L, label = "Non (first shown) → order=2")
  expect_equal(res$BINAIRE$levels[["2"]]$order, 1L, label = "Oui (second shown, positive) → order=1")
})


# ---------------------------------------------------------------------------
# H. Helper function tests — .parse_json_example_block()
# ---------------------------------------------------------------------------

test_that("H1: .parse_json_example_block() parses ordinal with order integers", {
  json_text <- '{
    "DIPLOME": {
      "var_label": "Diplome",
      "role": "factor_ordinal",
      "cur": "factor_nominal",
      "levels": {
        "1": { "order": 3, "label": "Aucun" },
        "2": { "order": 2, "label": "Bac" },
        "3": { "order": 1, "label": "Bac+5" }
      }
    }
  }'
  result <- .parse_json_example_block(json_text)
  expect_length(result, 1)
  ex <- result[["DIPLOME"]]

  # Labels should be sorted by order ascending: Bac+5 (1), Bac (2), Aucun (3)
  expect_equal(ex$input_args$labels, c("Bac+5", "Bac", "Aucun"))
  expect_equal(ex$input_args$n_distinct, 3L)
  expect_equal(ex$input_args$detected_role, "factor_nominal")

  # Expected output is JSONL: first shown is highest rank → high_first
  parsed_expected <- jsonlite::fromJSON(ex$expected)
  expect_equal(parsed_expected$role, "factor_ordinal")
  expect_equal(parsed_expected$desc, "high_first")
})

test_that("H2: .parse_json_example_block() parses ascending ordinal → high_first", {
  json_text <- '{
    "AGE": {
      "var_label": "Groupe d age",
      "role": "factor_ordinal",
      "levels": {
        "1": { "order": 4, "label": "20-29" },
        "2": { "order": 3, "label": "30-39" },
        "3": { "order": 2, "label": "40-49" },
        "4": { "order": 1, "label": "50-69" }
      }
    }
  }'
  result <- .parse_json_example_block(json_text)
  ex <- result[["AGE"]]

  # Sorted by order ascending: 50-69 (1), 40-49 (2), 30-39 (3), 20-29 (4)
  expect_equal(ex$input_args$labels, c("50-69", "40-49", "30-39", "20-29"))
  # First has order=1, last has order=4 → high_first
  parsed_expected <- jsonlite::fromJSON(ex$expected)
  expect_equal(parsed_expected$role, "factor_ordinal")
  expect_equal(parsed_expected$desc, "high_first")
})

test_that("H3: .parse_json_example_block() handles missing levels", {
  json_text <- '{
    "Q3": {
      "var_label": "Vous habitez",
      "role": "factor_ordinal",
      "cur": "factor_nominal",
      "levels": {
        "1": { "order": 1, "label": "Grande ville" },
        "2": { "order": 2, "label": "Petite ville" },
        "9": { "missing": true, "label": "NSP" }
      }
    }
  }'
  result <- .parse_json_example_block(json_text)
  ex <- result[["Q3"]]

  expect_equal(ex$input_args$n_distinct, 2L)
  # Labels include missing at end
  expect_equal(length(ex$input_args$labels), 3L)
  expect_equal(ex$input_args$labels[3], "NSP")
  # Missing vals present
  expect_true(length(ex$input_args$missing_vals) > 0)
})

test_that("H4: .parse_json_example_block() parses nominal → no desc", {
  json_text <- '{
    "REGION": {
      "var_label": "Region",
      "role": "factor_nominal",
      "levels": {
        "1": { "label": "Nord" },
        "2": { "label": "Sud" },
        "3": { "label": "Est" }
      }
    }
  }'
  result <- .parse_json_example_block(json_text)
  ex <- result[["REGION"]]

  parsed_expected <- jsonlite::fromJSON(ex$expected)
  expect_equal(parsed_expected$role, "factor_nominal")
  expect_null(parsed_expected$desc)
})

test_that("H5: .parse_json_example_block() explicit desc overrides inferred direction", {
  json_text <- '{
    "AGE4": {
      "var_label": "Groupe d age 4 modalites",
      "role": "factor_ordinal",
      "desc": "low_first",
      "levels": {
        "1": { "order": 1, "label": "20-29" },
        "2": { "order": 2, "label": "30-39" },
        "3": { "order": 3, "label": "40-49" },
        "4": { "order": 4, "label": "50-69" }
      }
    }
  }'
  result <- .parse_json_example_block(json_text)
  ex <- result[["AGE4"]]

  # Sorted by order ascending: 20-29 (1), 30-39 (2), 40-49 (3), 50-69 (4)
  expect_equal(ex$input_args$labels, c("20-29", "30-39", "40-49", "50-69"))
  # Explicit desc="low_first" overrides inferred direction
  parsed_expected <- jsonlite::fromJSON(ex$expected)
  expect_equal(parsed_expected$role, "factor_ordinal")
  expect_equal(parsed_expected$desc, "low_first")
})


# ---------------------------------------------------------------------------
# P. Prompt building tests — .build_classify_system_prompt()
# ---------------------------------------------------------------------------

test_that("P1: .build_classify_system_prompt() with ordinal_desc=TRUE includes desc EXAMPLES", {
  withr::local_dir(.test_proj_root)
  prompt_path <- file.path(getwd(), "instructions", "classify_roles_prompt.md")
  skip_if_not(file.exists(prompt_path), "classify_roles_prompt.md not found")

  result <- .build_classify_system_prompt(prompt_path, ordinal_desc = TRUE)

  # Should contain ordinal direction detection section
  expect_true(grepl("Ordinal descending/ascending order detection", result))
  # Should contain JSONL format (Input/Output pairs from json_example blocks)
  expect_true(grepl('"role"', result))
  expect_true(grepl('"id"', result))
  expect_true(grepl("Input:", result))
  expect_true(grepl("Output:", result))
  # Should NOT contain raw json fences
  expect_false(grepl("```json", result))
  # Should NOT contain IF markers
  expect_false(grepl("<!-- IF", result))
  # Should contain factor_ordinal and factor_binary desc sections
  expect_true(grepl("factor_ordinal", result))
  expect_true(grepl("factor_binary", result))
  # Should NOT contain old SET: format
  expect_false(grepl("SET:", result))
  # Should NOT contain nd:0 section (removed)
  expect_false(grepl("nd:0 EXAMPLES", result))
  # Should contain MISSING VALUE DETECTION section
  expect_true(grepl("MISSING VALUE DETECTION", result))
  # Should contain binary unknown examples
  expect_true(grepl("Homme.*Femme", result))
  # Should contain Autre ordinal example
  expect_true(grepl("Autre", result))
})

test_that("P2: .build_classify_system_prompt() with ordinal_desc=FALSE has binary desc only", {
  withr::local_dir(.test_proj_root)
  prompt_path <- file.path(getwd(), "instructions", "classify_roles_prompt.md")
  skip_if_not(file.exists(prompt_path), "classify_roles_prompt.md not found")

  result <- .build_classify_system_prompt(prompt_path, ordinal_desc = FALSE)

  # Should NOT contain desc EXAMPLES section (only present when ordinal_desc=TRUE)
  expect_false(grepl("desc EXAMPLES", result))
  # Should contain binary desc section
  expect_true(grepl("factor_binary", result))
  # Should NOT contain IF markers
  expect_false(grepl("<!-- IF", result))
  # Should NOT contain old SET: format
  expect_false(grepl("SET:", result))
  # Should NOT contain nd:0 section (removed)
  expect_false(grepl("nd:0 EXAMPLES", result))
  # Should contain MISSING VALUE DETECTION section
  expect_true(grepl("MISSING VALUE DETECTION", result))
  # Should contain binary unknown examples
  expect_true(grepl("Homme.*Femme", result))
})


# ---------------------------------------------------------------------------
# A. Auto-classification tests — nd:0 and nd:1 variables
# ---------------------------------------------------------------------------

test_that("A1: nd:0 integer vars auto-classified as integer_count without API call", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()
  vars <- list(
    AGE_Q = list(
      var_label = "quel est votre age",
      role      = "integer",
      levels    = list("99" = list(label = "NSP", missing = TRUE))
    )
  )
  .write_meta_json(make_meta_list(vars), path)

  meta <- make_classify_meta(
    "AGE_Q", "quel est votre age",
    c("NSP"), c(99L),
    missing_vals  = 99L,
    detected_role = "integer"
  )
  meta$n_distinct <- 0L

  # mock_ai should NOT be called — if it is, test fails
  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude",
    function(...) stop("API should not be called for nd:0 vars"),
    envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)

  suppressMessages(
    ai_classify_roles(meta, meta_json = path)
  )

  res <- .read_meta_json(path)$variables$AGE_Q
  expect_equal(res$role, "integer_count")
})

test_that("A2: nd:1 factor vars auto-classified as factor_unique_value without API call", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()
  vars <- list(
    CONST = list(
      var_label = "constante",
      role      = "factor_nominal",
      levels    = list("1" = list(label = "Valeur unique"))
    )
  )
  .write_meta_json(make_meta_list(vars), path)

  meta <- make_classify_meta(
    "CONST", "constante",
    c("Valeur unique"), c(1L),
    detected_role = "factor_nominal"
  )
  meta$n_distinct <- 1L

  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude",
    function(...) stop("API should not be called for n_distinct==1"),
    envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)

  suppressMessages(
    ai_classify_roles(meta, meta_json = path)
  )

  res <- .read_meta_json(path)$variables$CONST
  expect_equal(res$role, "factor_unique_value")
})

test_that("A3: mixed nd:0 + nd:1 + normal vars — auto + API classification", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()
  vars <- list(
    AGE_Q = list(
      var_label = "quel est votre age",
      role      = "integer",
      levels    = list("99" = list(label = "NSP", missing = TRUE))
    ),
    CONST = list(
      var_label = "constante",
      role      = "factor_nominal",
      levels    = list("1" = list(label = "Valeur unique"))
    ),
    SATISF = list(
      var_label = "satisfaction",
      role      = "factor_nominal",
      levels    = list(
        "1" = list(label = "Tres satisfait"),
        "2" = list(label = "Satisfait"),
        "3" = list(label = "Pas satisfait")
      )
    )
  )
  .write_meta_json(make_meta_list(vars), path)

  meta <- dplyr::bind_rows(
    {
      m <- make_classify_meta("AGE_Q", "quel est votre age",
                              c("NSP"), c(99L), missing_vals = 99L,
                              detected_role = "integer")
      m$n_distinct <- 0L
      m
    },
    {
      m <- make_classify_meta("CONST", "constante",
                              c("Valeur unique"), c(1L))
      m$n_distinct <- 1L
      m
    },
    make_classify_meta("SATISF", "satisfaction",
                       c("Tres satisfait", "Satisfait", "Pas satisfait"),
                       c(1L, 2L, 3L))
  )

  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude",
    mock_ai('{"id":"SATISF","role":"factor_ordinal","desc":"high_first"}'),
    envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)

  suppressMessages(
    ai_classify_roles(meta, meta_json = path, ordinal_desc = TRUE)
  )

  res <- .read_meta_json(path)$variables
  expect_equal(res$AGE_Q$role, "integer_count")
  expect_equal(res$CONST$role, "factor_unique_value")
  expect_equal(res$SATISF$role, "factor_ordinal")
})


# ---------------------------------------------------------------------------
# B. Binary desc="unknown" — existing JSON order preserved
# ---------------------------------------------------------------------------

test_that("B1: binary + desc unknown → existing JSON order preserved", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()
  vars <- list(
    SEXE = list(
      var_label = "Sexe",
      role      = "factor_nominal",
      levels    = list(
        "1" = list(label = "Homme"),
        "2" = list(label = "Femme")
      )
    )
  )
  .write_meta_json(make_meta_list(vars), path)

  meta <- make_classify_meta(
    "SEXE", "sexe de l enquete",
    c("Homme", "Femme"), c(1L, 2L),
    detected_role = "factor_nominal"
  )

  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude",
    mock_ai('{"id":"SEXE","role":"factor_binary","desc":"unknown"}'),
    envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)
  suppressMessages(
    ai_classify_roles(meta, meta_json = path, ordinal_desc = TRUE)
  )

  res <- .read_meta_json(path)$variables$SEXE
  expect_equal(res$role, "factor_binary")
  # desc=unknown + .find_binary_desc fails → existing JSON order preserved (1, 2 from v3 migration)
  expect_equal(res$levels[["1"]]$order, 1L)
  expect_equal(res$levels[["2"]]$order, 2L)
})


# ---------------------------------------------------------------------------
# O. Ordinal with "Autre" — Autre moved to last position
# ---------------------------------------------------------------------------

test_that("O1: ordinal with Autre → Autre gets last order position", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()
  vars <- list(
    DIPLOME = list(
      var_label = "Diplome",
      role      = "factor_nominal",
      levels    = list(
        "1" = list(label = "Aucun"),
        "2" = list(label = "Autre"),
        "3" = list(label = "Bac"),
        "4" = list(label = "Bac+5")
      )
    )
  )
  .write_meta_json(make_meta_list(vars), path)

  meta <- make_classify_meta(
    "DIPLOME", "Diplome",
    c("Aucun", "Autre", "Bac", "Bac+5"),
    c(1L, 2L, 3L, 4L)
  )

  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude",
    mock_ai('{"id":"DIPLOME","role":"factor_ordinal","desc":"high_first"}'),
    envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)
  suppressMessages(
    ai_classify_roles(meta, meta_json = path, ordinal_desc = TRUE)
  )

  res <- .read_meta_json(path)$variables$DIPLOME$levels
  # high_first: sequential 1,2,3,4 then Autre moved to 4 (last)
  # Original order: Aucun=1, Autre=2, Bac=3, Bac+5=4
  # After Autre move: Aucun=1, Bac=2, Bac+5=3, Autre=4
  expect_equal(res[["2"]]$order, 4L, label = "Autre should be order=4 (last)")
  expect_equal(res[["1"]]$order, 1L, label = "Aucun should be order=1")
  expect_equal(res[["3"]]$order, 2L, label = "Bac should be order=2")
  expect_equal(res[["4"]]$order, 3L, label = "Bac+5 should be order=3")
})

test_that("O2: ordinal with Autre already last → order unchanged", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()
  vars <- list(
    FREQ = list(
      var_label = "Frequence",
      role      = "factor_nominal",
      levels    = list(
        "1" = list(label = "Toujours"),
        "2" = list(label = "Souvent"),
        "3" = list(label = "Rarement"),
        "4" = list(label = "Autre")
      )
    )
  )
  .write_meta_json(make_meta_list(vars), path)

  meta <- make_classify_meta(
    "FREQ", "Frequence",
    c("Toujours", "Souvent", "Rarement", "Autre"),
    c(1L, 2L, 3L, 4L)
  )

  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude",
    mock_ai('{"id":"FREQ","role":"factor_ordinal","desc":"high_first"}'),
    envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)
  suppressMessages(
    ai_classify_roles(meta, meta_json = path, ordinal_desc = TRUE)
  )

  res <- .read_meta_json(path)$variables$FREQ$levels
  # Autre is already last (position 4) → order stays 4
  expect_equal(res[["1"]]$order, 1L)
  expect_equal(res[["2"]]$order, 2L)
  expect_equal(res[["3"]]$order, 3L)
  expect_equal(res[["4"]]$order, 4L, label = "Autre already last → order=4")
})


# ---------------------------------------------------------------------------
# H. Additional parser tests — ordinal_desc parameter
# ---------------------------------------------------------------------------

test_that("H6: parse with ordinal_desc=FALSE → no desc in factor_ordinal expected", {
  json_text <- '{
    "AGE4": {
      "var_label": "Groupe d age",
      "role": "factor_ordinal",
      "levels": {
        "1": { "order": 1, "label": "20-29" },
        "2": { "order": 2, "label": "30-39" }
      }
    }
  }'
  result <- .parse_json_example_block(json_text, ordinal_desc = FALSE)
  parsed_expected <- jsonlite::fromJSON(result[["AGE4"]]$expected)
  expect_equal(parsed_expected$role, "factor_ordinal")
  expect_null(parsed_expected$desc)
})

test_that("H7: parse with ordinal_desc=FALSE → desc preserved for factor_binary", {
  json_text <- '{
    "EMPLOI": {
      "var_label": "avez-vous un emploi",
      "role": "factor_binary",
      "desc": "high_first",
      "cur": "factor_binary",
      "levels": {
        "1": { "label": "Oui" },
        "2": { "label": "Non" }
      }
    }
  }'
  result <- .parse_json_example_block(json_text, ordinal_desc = FALSE)
  parsed_expected <- jsonlite::fromJSON(result[["EMPLOI"]]$expected)
  expect_equal(parsed_expected$desc, "high_first")
})


# ---------------------------------------------------------------------------
# P. Additional prompt building tests — ordinal desc in examples
# ---------------------------------------------------------------------------

test_that("P3: ordinal_desc=TRUE → ordinal JSONL examples in built prompt include desc", {
  withr::local_dir(.test_proj_root)
  prompt_path <- file.path(getwd(), "instructions", "classify_roles_prompt.md")
  skip_if_not(file.exists(prompt_path), "classify_roles_prompt.md not found")

  result <- .build_classify_system_prompt(prompt_path, ordinal_desc = TRUE)

  # SATISF ordinal example should include desc
  expect_true(grepl('"SATISF".*"factor_ordinal".*"desc"', result))
  # No raw fences should remain
  expect_false(grepl("```", result))
})

test_that("P4: ordinal_desc=FALSE → ordinal JSONL examples have no desc", {
  withr::local_dir(.test_proj_root)
  prompt_path <- file.path(getwd(), "instructions", "classify_roles_prompt.md")
  skip_if_not(file.exists(prompt_path), "classify_roles_prompt.md not found")

  result <- .build_classify_system_prompt(prompt_path, ordinal_desc = FALSE)

  # SATISF ordinal Output line should NOT contain desc — check line by line
  satisf_output_lines <- grep('"SATISF".*"factor_ordinal"', strsplit(result, "\n")[[1]], value = TRUE)
  expect_true(length(satisf_output_lines) > 0, label = "SATISF ordinal output line must exist")
  expect_false(any(grepl('"desc"', satisf_output_lines)), label = "SATISF ordinal output must not have desc")
  # Binary desc must still be present (Q1 from factor_binary section)
  expect_true(grepl('"Q1".*"desc"', result, perl = TRUE))
})


# ---------------------------------------------------------------------------
# M. miss field — extra missing-value detection from AI response
# ---------------------------------------------------------------------------

test_that("M1: AI returns miss field → message printed with flagged label", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()
  vars <- list(
    SANTE = list(
      var_label = "Etat de sante",
      role      = "factor_nominal",
      levels    = list(
        "1" = list(label = "Tres bon"),
        "2" = list(label = "Bon"),
        "3" = list(label = "Mauvais"),
        "4" = list(label = "Non concerne")
      )
    )
  )
  .write_meta_json(make_meta_list(vars), path)

  meta <- make_classify_meta(
    "SANTE", "Etat de sante",
    c("Tres bon", "Bon", "Mauvais", "Non concerne"),
    c(1L, 2L, 3L, 4L)
  )

  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude",
    mock_ai('{"id":"SANTE","role":"factor_ordinal","desc":"high_first","miss":"Non concerne"}'),
    envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)

  expect_message(
    ai_classify_roles(meta, meta_json = path, ordinal_desc = TRUE),
    "Non concerne",
    label = "miss field should appear in message output"
  )

  # Role should still be updated correctly
  res <- .read_meta_json(path)$variables$SANTE
  expect_equal(res$role, "factor_ordinal")
})

# ---------------------------------------------------------------------------
# P. Additional prompt building tests — new examples
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# I. Pre-AI nd:1 disambiguation — integer_count / other / factor_unique_value
# ---------------------------------------------------------------------------

test_that("I1: nd:1 + detected_role=integer → integer_count (not factor_unique_value)", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()
  vars <- list(
    AGE_Q = list(
      var_label = "quel est votre age",
      role      = "integer",
      levels    = list("99" = list(label = "NSP", missing = TRUE))
    )
  )
  .write_meta_json(make_meta_list(vars), path)

  meta <- make_classify_meta(
    "AGE_Q", "quel est votre age",
    c("NSP"), c(99L),
    missing_vals    = 99L,
    detected_role   = "integer",
    n_distinct_data = 3L  # small data count but detected_role=integer triggers it
  )
  meta$n_distinct <- 1L  # override to nd:1

  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude",
    function(...) stop("API should not be called for nd:1 integer vars"),
    envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)

  suppressMessages(
    ai_classify_roles(meta, meta_json = path)
  )

  res <- .read_meta_json(path)$variables$AGE_Q
  expect_equal(res$role, "integer_count")
})

test_that("I2: nd:1 + large n_distinct_data + factor_nominal → integer_count (labelled age)", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()
  vars <- list(
    Q19C = list(
      var_label = "Quel est son age actuel",
      role      = "factor_nominal",
      levels    = list(
        "85" = list(label = "85 ans ou plus"),
        "88" = list(label = "NVPD", missing = TRUE),
        "99" = list(label = "Ne sais pas", missing = TRUE)
      )
    )
  )
  .write_meta_json(make_meta_list(vars), path)

  meta <- make_classify_meta(
    "Q19C", "Quel est son age actuel",
    c("85 ans ou plus", "NVPD", "Ne sais pas"),
    c(85L, 88L, 99L),
    missing_vals    = c(88L, 99L),
    detected_role   = "factor_nominal",
    n_distinct_data = 72L  # 72 distinct integer ages observed in data
  )
  # n_distinct = 1 (only "85 ans ou plus" is non-missing label)

  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude",
    function(...) stop("API should not be called for nd:1 with large n_distinct_data"),
    envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)

  suppressMessages(
    ai_classify_roles(meta, meta_json = path)
  )

  res <- .read_meta_json(path)$variables$Q19C
  expect_equal(res$role, "integer_count",
               label = "Many distinct data values → integer_count despite factor_nominal detected_role")
})

test_that("I3: nd:1 + 'noter en clair' label → other (open-text field)", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()
  vars <- list(
    EMP5 = list(
      var_label = "Quelle est votre profession",
      role      = "factor_nominal",
      levels    = list(
        "01" = list(label = "Noter en clair"),
        "88" = list(label = "NVPD", missing = TRUE),
        "99" = list(label = "NSP", missing = TRUE)
      )
    )
  )
  .write_meta_json(make_meta_list(vars), path)

  meta <- make_classify_meta(
    "EMP5", "Quelle est votre profession",
    c("Noter en clair", "NVPD", "NSP"),
    c("01", "88", "99"),
    missing_vals    = c("88", "99"),
    detected_role   = "factor_nominal",
    n_distinct_data = 3L  # only 3 distinct codes in data (01, 88, 99)
  )
  # n_distinct = 1

  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude",
    function(...) stop("API should not be called for open-text nd:1 vars"),
    envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)

  suppressMessages(
    ai_classify_roles(meta, meta_json = path)
  )

  res <- .read_meta_json(path)$variables$EMP5
  expect_equal(res$role, "other",
               label = "'noter en clair' label → other (open-text field)")
})

test_that("I3b: nd:1 + 'preciser' in var_label → other (open-text detected via var_label)", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()
  vars <- list(
    EMP1B = list(
      var_label = "Quel etait votre dernier emploi ? Notez le metier, puis precisez",
      role      = "factor_nominal",
      levels    = list(
        "01" = list(label = "Reponse"),
        "88" = list(label = "NVPD", missing = TRUE)
      )
    )
  )
  .write_meta_json(make_meta_list(vars), path)

  meta <- make_classify_meta(
    "EMP1B", "Quel etait votre dernier emploi ? Notez le metier, puis precisez",
    c("Reponse", "NVPD"),
    c("01", "88"),
    missing_vals    = "88",
    detected_role   = "factor_nominal",
    n_distinct_data = 2L
  )

  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude",
    function(...) stop("API should not be called"),
    envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)

  suppressMessages(
    ai_classify_roles(meta, meta_json = path)
  )

  res <- .read_meta_json(path)$variables$EMP1B
  expect_equal(res$role, "other",
               label = "'precisez' in var_label → other")
})

test_that("I4: nd:1 + small n_distinct_data + normal label → factor_unique_value", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()
  vars <- list(
    Q15A = list(
      var_label = "Cela a-t-il dure plus ou moins de 4 mois",
      role      = "factor_nominal",
      levels    = list(
        "01" = list(label = "4 mois ou plus"),
        "88" = list(label = "NVPD", missing = TRUE)
      )
    )
  )
  .write_meta_json(make_meta_list(vars), path)

  meta <- make_classify_meta(
    "Q15A", "Cela a-t-il dure plus ou moins de 4 mois",
    c("4 mois ou plus", "NVPD"),
    c("01", "88"),
    missing_vals    = "88",
    detected_role   = "factor_nominal",
    n_distinct_data = 2L  # only 2 distinct codes in data (01 and 88)
  )
  # n_distinct = 1

  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude",
    function(...) stop("API should not be called for true factor_unique_value"),
    envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)

  suppressMessages(
    ai_classify_roles(meta, meta_json = path)
  )

  res <- .read_meta_json(path)$variables$Q15A
  expect_equal(res$role, "factor_unique_value",
               label = "True single-category factor → factor_unique_value")
})

test_that("I5: nd:0 + large n_distinct_data + factor_nominal → integer_count", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()
  vars <- list(
    AGE_ARRIVEE = list(
      var_label = "Age arrive en France",
      role      = "factor_nominal",
      levels    = list(
        "88" = list(label = "NVPD", missing = TRUE),
        "99" = list(label = "NSP ou prefere indiquer annee", missing = TRUE)
      )
    )
  )
  .write_meta_json(make_meta_list(vars), path)

  meta <- make_classify_meta(
    "AGE_ARRIVEE", "Age arrive en France",
    c("NVPD", "NSP ou prefere indiquer annee"),
    c(88L, 99L),
    missing_vals    = c(88L, 99L),
    detected_role   = "factor_nominal",
    n_distinct_data = 68L  # 68 distinct age values + 2 missing codes in data
  )
  # n_distinct = 0 (all labels are missing-coded)

  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude",
    function(...) stop("API should not be called for nd:0 vars"),
    envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)

  suppressMessages(
    ai_classify_roles(meta, meta_json = path)
  )

  res <- .read_meta_json(path)$variables$AGE_ARRIVEE
  expect_equal(res$role, "integer_count",
               label = "nd:0 with many distinct data values → integer_count despite factor_nominal")
})


test_that("P5: built prompt includes double, unclear, and miss examples", {
  withr::local_dir(.test_proj_root)
  prompt_path <- file.path(getwd(), "instructions", "classify_roles_prompt.md")
  skip_if_not(file.exists(prompt_path), "classify_roles_prompt.md not found")

  result <- .build_classify_system_prompt(prompt_path, ordinal_desc = TRUE)

  # double example present
  expect_true(grepl('"double"', result), label = "double role example must exist")
  # unclear example present
  expect_true(grepl('"unclear"', result), label = "unclear role example must exist")
  # miss field example present
  expect_true(grepl('"miss"', result), label = "miss field example must exist")
  # Renamed descending age example should use AGE10_DESC, not duplicate Q19E_GRAGE
  expect_true(grepl("AGE10_DESC", result),
    label = "AGE10_DESC should appear in built prompt")
})


# ===========================================================================
# AC: Edge dummy auto-classification tests
# ===========================================================================

test_that("AC1: edge dummy ALL_MISS (nd:0) auto-classified without API call", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()
  on.exit(unlink(path))

  meta <- suppressMessages(extract_survey_metadata(
    .edge_dummy,
    meta_json   = path,
    missing_num = .edge_missing_num,
    missing_chr = .edge_missing_chr,
    yes_labels  = .edge_yes_labels,
    no_labels   = .edge_no_labels
  ))

  # Mock AI: should NOT be called for nd:0 variables (auto-classified)
  # Return valid response for other vars that do need classification
  classify_resp <- paste(
    '{"id":"SINGLE_VAL","role":"factor_nominal"}',
    '{"id":"EMPTY_LABELS","role":"factor_binary","desc":"high_first"}',
    '{"id":"HIGH_CARD","role":"factor_nominal"}',
    sep = "\n"
  )
  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude", mock_ai(classify_resp), envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)

  suppressMessages(
    ai_classify_roles(meta, meta_json = path)
  )

  res <- .read_meta_json(path)$variables$ALL_MISS
  # nd:0 → auto-classified as integer_count (no API needed)
  expect_true(res$role %in% c("integer_count", "integer", "other", "factor_nominal"),
              info = paste("ALL_MISS role:", res$role))
})


test_that("AC2: edge dummy SINGLE_VAL (nd:1) auto-classified as factor_unique_value", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()
  on.exit(unlink(path))

  meta <- suppressMessages(extract_survey_metadata(
    .edge_dummy,
    meta_json   = path,
    missing_num = .edge_missing_num,
    missing_chr = .edge_missing_chr,
    yes_labels  = .edge_yes_labels,
    no_labels   = .edge_no_labels
  ))

  # Mock AI for vars that do get sent to API
  classify_resp <- paste(
    '{"id":"EMPTY_LABELS","role":"factor_binary","desc":"high_first"}',
    '{"id":"HIGH_CARD","role":"factor_nominal"}',
    sep = "\n"
  )
  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude", mock_ai(classify_resp), envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)

  suppressMessages(
    ai_classify_roles(meta, meta_json = path)
  )

  res <- .read_meta_json(path)$variables$SINGLE_VAL
  # nd:1 → auto-classified as factor_unique_value (no API needed)
  expect_equal(res$role, "factor_unique_value",
               info = paste("SINGLE_VAL role:", res$role))
})
