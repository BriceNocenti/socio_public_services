# Tests for double-coded haven_labelled variables in ai_suggest_labels().
# Reproduces the bug where variables with double value codes (0,1) are
# silently dropped from the AI prompt.
#
# Uses dput() structures from the real dataset to exactly reproduce the bug.

# ---------------------------------------------------------------------------
# D1: haven_labelled<double> binary with codes 0/1 (like STERILE)
# ---------------------------------------------------------------------------

test_that("D1: haven_labelled<double> binary (codes 0/1) appears in ai_suggest_labels prompt", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()

  # Reproduce STERILE exactly from dput() output
  sterile_col <- structure(
    c(0, 0, 1, 0, 0, 1),
    label = "F: Stérile",
    format.spss = "F3.0",
    display_width = 3L,
    labels = c(Non = 0, Oui = 1),
    class = c("haven_labelled", "vctrs_vctr", "double")
  )

  df_test <- tibble::tibble(STERILE = sterile_col)

  meta <- suppressMessages(extract_survey_metadata(
    df_test,
    meta_json   = path,
    missing_num = c(-1L, 96L, 99L)
  ))

  # Check role was detected correctly
  row <- meta[meta$var_name == "STERILE", ]
  expect_true(nrow(row) == 1L)
  expect_true(row$detected_role %in% c("factor_binary", "factor_nominal"))

  # Check new_labels is NOT empty and NOT all "NULL"
  expect_true(length(row$new_labels[[1]]) > 0,
              info = "new_labels should not be empty for STERILE")
  expect_false(all(row$new_labels[[1]] == "NULL"),
               info = "new_labels should not be all NULL for STERILE non-missing levels")

  # Check values are stored correctly (as character or numeric, but not empty)
  expect_true(length(row$values[[1]]) > 0,
              info = "values should not be empty for STERILE")

  # Check that dry_run prompt includes STERILE
  fake_resp <- '{"STERILE": {"0": "Stérile", "1": "Pas stérile"}}'
  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude", mock_ai(fake_resp), envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)

  prompts <- suppressMessages(
    ai_suggest_labels(meta, meta_json = path, dry_run = TRUE,
                      replace_existing_new_labels = TRUE)
  )

  expect_true(length(prompts) > 0,
              info = "ai_suggest_labels dry_run should produce at least one prompt")
  prompt_text <- paste(prompts, collapse = "\n")
  expect_true(grepl("STERILE", prompt_text),
              info = "STERILE should appear in the AI prompt")
})

# ---------------------------------------------------------------------------
# D2: haven_labelled<double> nominal with codes 1/2/3 (like ANNEAUVIE)
# ---------------------------------------------------------------------------

test_that("D2: haven_labelled<double> nominal (codes 1/2/3) appears in ai_suggest_labels prompt", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()

  anneauvie_col <- structure(
    c(2, 2, 2, NA, 2, 1, 2, 3),
    label = "A déjà utilisé l'anneau vaginal (vie)",
    format.spss = "F3.0",
    display_width = 3L,
    labels = c(Oui = 1, Non = 2,
               `N'a jamais utilisé de méthode contraceptive` = 3),
    class = c("haven_labelled", "vctrs_vctr", "double")
  )

  df_test <- tibble::tibble(ANNEAUVIE = anneauvie_col)

  meta <- suppressMessages(extract_survey_metadata(
    df_test,
    meta_json   = path,
    missing_num = c(-1L, 96L, 99L)
  ))

  row <- meta[meta$var_name == "ANNEAUVIE", ]
  expect_true(nrow(row) == 1L)
  expect_true(row$detected_role %in% c("factor_nominal", "factor_ordinal", "factor_binary"))
  expect_true(length(row$new_labels[[1]]) > 0,
              info = "new_labels should not be empty for ANNEAUVIE")
  expect_false(all(row$new_labels[[1]] == "NULL"),
               info = "new_labels should not be all NULL for ANNEAUVIE non-missing levels")

  fake_resp <- '{"ANNEAUVIE": {"1": "Oui", "2": "Non", "3": "Jamais"}}'
  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude", mock_ai(fake_resp), envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)

  prompts <- suppressMessages(
    ai_suggest_labels(meta, meta_json = path, dry_run = TRUE,
                      replace_existing_new_labels = TRUE)
  )

  expect_true(length(prompts) > 0,
              info = "ai_suggest_labels dry_run should produce at least one prompt")
  expect_true(grepl("ANNEAUVIE", paste(prompts, collapse = "\n")),
              info = "ANNEAUVIE should appear in the AI prompt")
})

# ---------------------------------------------------------------------------
# D3: haven_labelled<double> binary codes 1/2 (like CONTRA_R)
# ---------------------------------------------------------------------------


test_that("D3: haven_labelled<double> binary (codes 1/2) appears in ai_suggest_labels prompt", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()

  contra_col <- structure(
    c(1, 1, 1, NA, 1, 2, 1, 1),
    label = "A déjà utilisé au moins une méthode contraceptive dans sa vie",
    format.spss = "F3.0",
    display_width = 3L,
    labels = c(Oui = 1, Non = 2),
    class = c("haven_labelled", "vctrs_vctr", "double")
  )

  df_test <- tibble::tibble(CONTRA_R = contra_col)

  meta <- suppressMessages(extract_survey_metadata(
    df_test,
    meta_json   = path,
    missing_num = c(-1L, 96L, 99L)
  ))

  row <- meta[meta$var_name == "CONTRA_R", ]
  expect_true(nrow(row) == 1L)
  expect_true(row$detected_role %in% c("factor_binary", "factor_nominal"))
  expect_true(length(row$new_labels[[1]]) > 0)
  expect_false(all(row$new_labels[[1]] == "NULL"))

  fake_resp <- '{"CONTRA_R": {"1": "A utilisé contraception", "2": "Pas contraception"}}'
  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude", mock_ai(fake_resp), envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)

  prompts <- suppressMessages(
    ai_suggest_labels(meta, meta_json = path, dry_run = TRUE,
                      replace_existing_new_labels = TRUE)
  )

  expect_true(length(prompts) > 0)
  expect_true(grepl("CONTRA_R", paste(prompts, collapse = "\n")),
              info = "CONTRA_R should appear in the AI prompt")
})

# ---------------------------------------------------------------------------
# D4: metadata_apply_meta_json() must NOT clear new_labels when JSON has
#     only "order"/"n" (no new_label, no missing:true) for a variable,
#     even when another variable in the same JSON has new_label set.
#
# Regression test for the bind_rows() padding bug:
#   VAR_A: JSON has new_label → update_rows row has new_labels column
#   VAR_B: JSON has only order → update_rows row has NO new_labels column
#   bind_rows() padded VAR_B's new_labels with list(NULL), clearing it.
# ---------------------------------------------------------------------------

test_that("D4: metadata_apply_meta_json preserves new_labels when JSON has only order (no new_label)", {
  # Build a minimal json_vars list simulating a partially-processed JSON:
  #   VAR_A: fully labeled (new_label present)
  #   VAR_B: has order+n but no new_label (like STERILE before ai_suggest_labels)
  json_vars <- list(
    VAR_A = list(
      var_label = "Variable A",
      role      = "factor_binary",
      new_name  = "VAR_A",
      levels    = list(
        "1" = list(label = "Oui", new_label = "A voté",  order = 1L, n = 80L),
        "2" = list(label = "Non", new_label = "Pas voté", order = 2L, n = 20L)
      )
    ),
    VAR_B = list(
      var_label = "Variable B",
      role      = "factor_binary",
      new_name  = "VAR_B",
      levels    = list(
        "1" = list(label = "Oui", order = 1L, n = 449L),
        "2" = list(label = "Non", order = 2L, n = 8196L)
      )
    )
  )

  # Build minimal metadata with both variables (new_labels = original labels)
  metadata <- tibble::tibble(
    var_name      = c("VAR_A", "VAR_B"),
    var_label     = c("Variable A", "Variable B"),
    r_class       = c("integer", "double"),
    n_distinct    = c(2L, 2L),
    n_distinct_data = c(2L, 2L),
    detected_role = c("factor_binary", "factor_binary"),
    values        = list(c("1", "2"), c("1", "2")),
    labels        = list(c("Oui", "Non"), c("Oui", "Non")),
    missing_vals  = list(character(0), character(0)),
    new_labels    = list(c("Oui", "Non"), c("Oui", "Non")),
    new_name      = c("VAR_A", "VAR_B")
  )

  result <- suppressMessages(metadata_apply_meta_json(metadata, json_vars))

  # VAR_A: new_labels updated from JSON
  res_a <- result[result$var_name == "VAR_A", ]$new_labels[[1]]
  expect_equal(res_a, c("A voté", "Pas voté"))

  # VAR_B: new_labels must NOT be cleared — must still be original labels
  res_b <- result[result$var_name == "VAR_B", ]$new_labels[[1]]
  expect_true(length(res_b) > 0,
              info = "VAR_B new_labels must not be cleared by bind_rows() padding")
  expect_equal(res_b, c("Oui", "Non"),
               info = "VAR_B new_labels must stay as original labels when JSON has no new_label")
})

