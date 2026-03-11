# Tests for extract_survey_metadata() role detection using shared dummy datasets.
#
# Uses the three shared dummies from testthat.R:
#   .virage_dummy  — haven_labelled columns (labels embedded in R object)
#   .emploi_dummy  — plain character/numeric (labels from SAS format file)
#   .edge_dummy    — synthetic edge cases
#
# Replaces: I1-I3 from test-sas-format-parser.R, D1-D3 from test-double-coded-vars.R


# ===========================================================================
# E1: Virage dummy — detected roles match expected
# ===========================================================================
test_that("E1: Virage dummy detected roles are correct (haven_labelled path)", {
  meta <- extract_dummy_meta(
    .virage_dummy,
    missing_num = .virage_missing_num,
    missing_chr = .virage_missing_chr,
    yes_labels  = .virage_yes_labels,
    no_labels   = .virage_no_labels
  )

  expect_s3_class(meta, "tbl_df")
  expect_equal(nrow(meta), ncol(.virage_dummy))

  for (vname in names(.virage_expected_roles)) {
    row <- meta[meta$var_name == vname, ]
    expect_equal(nrow(row), 1L, info = paste("variable", vname, "found in metadata"))

    expected <- .virage_expected_roles[[vname]]
    if (vname == "MIGBIS_E") {
      # MIGBIS_E: 3 non-null after excluding Inclassable, could be nominal or binary
      expect_true(row$detected_role %in% c("factor_nominal", "factor_binary"),
                  info = paste(vname, "role:", row$detected_role))
    } else {
      expect_equal(row$detected_role, expected,
                   info = paste(vname, "expected", expected, "got", row$detected_role))
    }
  }
})


# ===========================================================================
# E2: Emploi dummy — detected roles correct via sas_format_file
# ===========================================================================
test_that("E2: Emploi dummy detected roles are correct (SAS format file path)", {
  f <- tempfile(fileext = ".sas")
  writeLines(.sas_emploi_inline, f, useBytes = TRUE)
  on.exit(unlink(f))

  meta <- extract_dummy_meta(
    .emploi_dummy,
    missing_num     = .emploi_missing_num,
    missing_chr     = .emploi_missing_chr,
    yes_labels      = .emploi_yes_labels,
    no_labels       = .emploi_no_labels,
    sas_format_file = f
  )

  expect_s3_class(meta, "tbl_df")
  expect_equal(nrow(meta), ncol(.emploi_dummy))

  for (vname in names(.emploi_expected_roles)) {
    row <- meta[meta$var_name == vname, ]
    expect_equal(nrow(row), 1L, info = paste("variable", vname, "found"))
    expect_equal(row$detected_role, .emploi_expected_roles[[vname]],
                 info = paste(vname, "expected", .emploi_expected_roles[[vname]],
                              "got", row$detected_role))
  }
})


# ===========================================================================
# E3: Edge dummy — detected roles match expected
# ===========================================================================
test_that("E3: Edge dummy detected roles are correct (edge cases)", {
  meta <- extract_dummy_meta(
    .edge_dummy,
    missing_num = .edge_missing_num,
    missing_chr = .edge_missing_chr,
    yes_labels  = .edge_yes_labels,
    no_labels   = .edge_no_labels
  )

  expect_s3_class(meta, "tbl_df")
  expect_equal(nrow(meta), ncol(.edge_dummy))

  for (vname in names(.edge_expected_roles)) {
    row <- meta[meta$var_name == vname, ]
    expect_equal(nrow(row), 1L, info = paste("variable", vname, "found"))

    expected <- .edge_expected_roles[[vname]]
    actual   <- row$detected_role

    expect_equal(actual, expected,
                 info = paste(vname, "expected", expected, "got", actual))
  }
})


# ===========================================================================
# E4: Emploi JSON roundtrip preserves SAS labels
# ===========================================================================
test_that("E4: Emploi dummy JSON roundtrip preserves SAS labels", {
  withr::local_dir(.test_proj_root)
  f <- tempfile(fileext = ".sas")
  writeLines(.sas_emploi_inline, f, useBytes = TRUE)
  json_path <- tmp_json()
  on.exit(unlink(c(f, json_path)))

  meta <- suppressMessages(extract_survey_metadata(
    .emploi_dummy,
    sas_format_file = f,
    meta_json       = json_path,
    missing_num     = .emploi_missing_num,
    missing_chr     = .emploi_missing_chr,
    yes_labels      = .emploi_yes_labels,
    no_labels       = .emploi_no_labels
  ))

  expect_true(file.exists(json_path))

  json_data <- .read_meta_json(json_path)
  expect_true("variables" %in% names(json_data))

  vars <- json_data$variables

  # METRODOM should have levels in the JSON
  expect_true("METRODOM" %in% names(vars))
  metro_levels <- vars[["METRODOM"]]$levels
  expect_true(length(metro_levels) > 0)

  # PCS1 should have 7 levels
  expect_true("PCS1" %in% names(vars))
  pcs_levels <- vars[["PCS1"]]$levels
  expect_true(length(pcs_levels) >= 6)

  # HCONT should exist but have no levels (double)
  expect_true("HCONT" %in% names(vars))
  expect_equal(vars[["HCONT"]]$role, "double")
})


# ===========================================================================
# E5: haven_labelled<double> binary with codes 0/1 detected correctly
# ===========================================================================
test_that("E5: haven_labelled<double> binary (codes 0/1) detected as factor_binary", {
  path <- tmp_json()
  on.exit(unlink(path))

  # STERILE-like column (from real data, reproduces D1 regression)
  sterile_col <- structure(
    c(0, 0, 1, 0, 0, 1),
    label = "F: St\u00e9rile",
    format.spss = "F3.0",
    display_width = 3L,
    labels = c(Non = 0, Oui = 1),
    class = c("haven_labelled", "vctrs_vctr", "double")
  )
  df_test <- tibble::tibble(STERILE = sterile_col)

  meta <- suppressMessages(extract_survey_metadata(
    df_test, meta_json = path, missing_num = c(-1L, 96L, 99L)
  ))

  row <- meta[meta$var_name == "STERILE", ]
  expect_equal(nrow(row), 1L)
  expect_true(row$detected_role %in% c("factor_binary", "factor_nominal"))
  expect_true(length(row$values[[1]]) > 0, info = "values should not be empty")
  expect_true(length(row$new_labels[[1]]) > 0, info = "new_labels should not be empty")
  expect_false(all(row$new_labels[[1]] == "NULL"),
               info = "new_labels should not be all NULL for non-missing levels")
})


# ===========================================================================
# E6: haven_labelled<double> nominal with codes 1/2/3 detected correctly
# ===========================================================================
test_that("E6: haven_labelled<double> nominal (codes 1/2/3) detected correctly", {
  path <- tmp_json()
  on.exit(unlink(path))

  # ANNEAUVIE-like column (from real data, reproduces D2 regression)
  anneauvie_col <- structure(
    c(2, 2, 2, NA, 2, 1, 2, 3),
    label = "A d\u00e9j\u00e0 utilis\u00e9 l'anneau vaginal (vie)",
    format.spss = "F3.0",
    display_width = 3L,
    labels = c(Oui = 1, Non = 2,
               "N'a jamais utilis\u00e9 de m\u00e9thode contraceptive" = 3),
    class = c("haven_labelled", "vctrs_vctr", "double")
  )
  df_test <- tibble::tibble(ANNEAUVIE = anneauvie_col)

  meta <- suppressMessages(extract_survey_metadata(
    df_test, meta_json = path, missing_num = c(-1L, 96L, 99L)
  ))

  row <- meta[meta$var_name == "ANNEAUVIE", ]
  expect_equal(nrow(row), 1L)
  expect_true(row$detected_role %in% c("factor_nominal", "factor_ordinal", "factor_binary"))
  expect_true(length(row$new_labels[[1]]) > 0, info = "new_labels should not be empty")
  expect_false(all(row$new_labels[[1]] == "NULL"),
               info = "new_labels should not be all NULL for non-missing levels")
})


# ===========================================================================
# E7: haven_labelled<double> binary codes 1/2 detected correctly
# ===========================================================================
test_that("E7: haven_labelled<double> binary (codes 1/2) detected correctly", {
  path <- tmp_json()
  on.exit(unlink(path))

  # CONTRA_R-like column (from real data, reproduces D3 regression)
  contra_col <- structure(
    c(1, 1, 1, NA, 1, 2, 1, 1),
    label = "A d\u00e9j\u00e0 utilis\u00e9 au moins une m\u00e9thode contraceptive dans sa vie",
    format.spss = "F3.0",
    display_width = 3L,
    labels = c(Oui = 1, Non = 2),
    class = c("haven_labelled", "vctrs_vctr", "double")
  )
  df_test <- tibble::tibble(CONTRA_R = contra_col)

  meta <- suppressMessages(extract_survey_metadata(
    df_test, meta_json = path, missing_num = c(-1L, 96L, 99L)
  ))

  row <- meta[meta$var_name == "CONTRA_R", ]
  expect_equal(nrow(row), 1L)
  expect_true(row$detected_role %in% c("factor_binary", "factor_nominal"))
  expect_true(length(row$new_labels[[1]]) > 0)
  expect_false(all(row$new_labels[[1]] == "NULL"))
})


# ===========================================================================
# E8: ALL_MISS has n_distinct == 0 after missing exclusion
# ===========================================================================
test_that("E8: all-missing variable has n_distinct == 0", {
  meta <- extract_dummy_meta(
    .edge_dummy,
    missing_num = .edge_missing_num,
    missing_chr = .edge_missing_chr,
    yes_labels  = .edge_yes_labels,
    no_labels   = .edge_no_labels
  )

  row <- meta[meta$var_name == "ALL_MISS", ]
  expect_equal(row$n_distinct, 0L)
})


# ===========================================================================
# E9: CONST_NUM detected as double (single fractional value)
# ===========================================================================
test_that("E9: constant fractional numeric detected as double", {
  meta <- extract_dummy_meta(
    .edge_dummy,
    missing_num = .edge_missing_num,
    missing_chr = .edge_missing_chr,
    yes_labels  = .edge_yes_labels,
    no_labels   = .edge_no_labels
  )

  row <- meta[meta$var_name == "CONST_NUM", ]
  expect_equal(row$detected_role, "double")
})


# ===========================================================================
# E10: DBL_BINARY in edge dummy appears in ai_suggest_labels prompt
# ===========================================================================
test_that("E10: haven_labelled<double> binary appears in ai_suggest_labels prompt", {
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

  fake_resp <- '{"DBL_BINARY": {"0": "Non", "1": "Oui"}}'
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
  expect_true(grepl("DBL_BINARY", prompt_text),
              info = "DBL_BINARY should appear in the AI prompt")
})
