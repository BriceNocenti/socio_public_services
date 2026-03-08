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

  # Should contain desc EXAMPLES section
  expect_true(grepl("desc EXAMPLES", result))
  # Should contain JSONL format (Input/Output pairs from json_example blocks)
  expect_true(grepl('"role"', result))
  expect_true(grepl('"id"', result))
  expect_true(grepl("Input:", result))
  expect_true(grepl("Output:", result))
  # Should NOT contain raw json_example fences
  expect_false(grepl("```json_example", result))
  # Should NOT contain IF markers
  expect_false(grepl("<!-- IF", result))
  # Should contain factor_ordinal and factor_binary desc sections
  expect_true(grepl("factor_ordinal", result))
  expect_true(grepl("factor_binary", result))
  # Should NOT contain old SET: format
  expect_false(grepl("SET:", result))
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
})
