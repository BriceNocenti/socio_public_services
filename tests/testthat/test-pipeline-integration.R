# End-to-end pipeline integration tests using shared dummy datasets.
#
# Walks through the full pipeline sequence on real-data dummies:
#   1. extract_survey_metadata()  -- role detection + initial JSON
#   2. ai_classify_roles()        -- AI disambiguates ordinal/nominal (mocked)
#   3. reload metadata from JSON  -- skeleton reconstruction (df=NULL)
#   4. ai_suggest_labels()        -- AI suggests short labels (mocked)
#   5. ai_suggest_varnames()      -- AI suggests variable names (mocked)
#   6. generate_format_script()   -- generate formatting R script
#
# All AI calls are mocked via assign()/on.exit() pattern.


# ===========================================================================
# INT1: Virage dummy -- full pipeline
# ===========================================================================
test_that("INT1: Virage dummy full pipeline (extract -> classify -> labels -> varnames -> script)", {
  withr::local_dir(.test_proj_root)
  json_path <- tmp_json()
  script_path <- tempfile(fileext = ".R")
  on.exit(unlink(c(json_path, script_path)))

  # Save original ai_call_claude for restoration
  .orig_ai <- get("ai_call_claude", envir = globalenv())
  on.exit(assign("ai_call_claude", .orig_ai, envir = globalenv()), add = TRUE)

  # --- Step 1: extract_survey_metadata ---
  meta <- suppressMessages(extract_survey_metadata(
    .virage_dummy,
    meta_json   = json_path,
    missing_num = .virage_missing_num,
    missing_chr = .virage_missing_chr,
    yes_labels  = .virage_yes_labels,
    no_labels   = .virage_no_labels
  ))

  expect_true(file.exists(json_path))
  expect_s3_class(meta, "tbl_df")
  expect_equal(nrow(meta), ncol(.virage_dummy))

  # Verify initial roles
  expect_equal(meta[meta$var_name == "Q25E1", ]$detected_role, "factor_binary")
  expect_equal(meta[meta$var_name == "Q19E_GRAGEBIS", ]$detected_role, "factor_nominal")
  expect_equal(meta[meta$var_name == "POIDS_CAL", ]$detected_role, "double")

  # --- Step 2: ai_classify_roles (mock) ---
  classify_response <- paste(
    '{"id":"Q19E_GRAGEBIS","role":"factor_ordinal","desc":"high_first"}',
    '{"id":"Q25E1","role":"factor_binary","desc":"high_first"}',
    '{"id":"MIGBIS_E","role":"factor_nominal"}',
    '{"id":"Q19E_AGE","role":"integer_count"}',
    sep = "\n"
  )
  assign("ai_call_claude", mock_ai(classify_response), envir = globalenv())

  suppressMessages(
    ai_classify_roles(meta, meta_json = json_path, ordinal_desc = TRUE)
  )

  # Verify JSON updated with ordinal role
  json_data <- .read_meta_json(json_path)
  expect_equal(json_data$variables$Q19E_GRAGEBIS$role, "factor_ordinal")

  # Verify order integers written for ordinal variable
  gragebis_levels <- json_data$variables$Q19E_GRAGEBIS$levels
  orders <- purrr::map_int(gragebis_levels, ~ .x$order %||% NA_integer_)
  non_miss_orders <- orders[!is.na(orders)]
  expect_true(length(non_miss_orders) > 0, info = "ordinal should have order integers")

  # --- Step 3: reload metadata from JSON (with df for full level info) ---
  meta2 <- suppressMessages(extract_survey_metadata(
    .virage_dummy, meta_json = json_path,
    missing_num = .virage_missing_num, missing_chr = .virage_missing_chr,
    yes_labels = .virage_yes_labels, no_labels = .virage_no_labels
  ))
  expect_s3_class(meta2, "tbl_df")
  expect_equal(nrow(meta2), ncol(.virage_dummy))
  expect_equal(meta2[meta2$var_name == "Q19E_GRAGEBIS", ]$detected_role, "factor_ordinal")

  # --- Step 3b: add level stats (required before ai_suggest_labels) ---
  meta2 <- suppressMessages(
    metadata_add_level_stats(meta2, .virage_dummy, meta_json = json_path)
  )

  # --- Step 4: ai_suggest_labels (mock) ---
  labels_response <- paste0(
    '{"Q25E1": {"00": "Non", "01": "Oui"},',
    ' "MIGBIS_E": {"01": "Majoritaire", "02": "DOM", "03": "Immigre"},',
    ' "Q19E_GRAGEBIS": {"01": "20-29", "02": "30-39", "03": "40-49", "04": "50-69"}}'
  )
  assign("ai_call_claude", mock_ai(labels_response), envir = globalenv())

  suppressMessages(
    ai_suggest_labels(meta2, meta_json = json_path,
                      replace_existing_new_labels = TRUE)
  )

  # Verify new_label written to JSON
  json_data <- .read_meta_json(json_path)
  q25_levels <- json_data$variables$Q25E1$levels
  expect_equal(q25_levels[["00"]]$new_label, "Non")
  expect_equal(q25_levels[["01"]]$new_label, "Oui")

  # Verify order integers preserved after label suggestion
  gragebis_levels <- json_data$variables$Q19E_GRAGEBIS$levels
  orders_after <- purrr::map_int(gragebis_levels, ~ .x$order %||% NA_integer_)
  expect_equal(orders_after[!is.na(orders_after)], non_miss_orders)

  # --- Step 5: ai_suggest_varnames (mock) ---
  varnames_response <- paste0(
    '{"Q25E1": "REPRISE_ETUD",',
    ' "MIGBIS_E": "STAT_MIGR",',
    ' "Q19E_GRAGEBIS": "GRP_AGE",',
    ' "POIDS_CAL": "POIDS",',
    ' "Q19E_AGE": "AGE",',
    ' "ID": "ID_RESP"}'
  )
  assign("ai_call_claude", mock_ai(varnames_response), envir = globalenv())

  suppressMessages(
    ai_suggest_varnames(meta2, meta_json = json_path)
  )

  # Verify new_name in JSON
  json_data <- .read_meta_json(json_path)
  expect_equal(json_data$variables$Q25E1$new_name, "REPRISE_ETUD")
  expect_equal(json_data$variables$POIDS_CAL$new_name, "POIDS")

  # --- Step 6: generate_format_script ---
  meta3 <- suppressMessages(extract_survey_metadata(
    .virage_dummy, meta_json = json_path,
    missing_num = .virage_missing_num, missing_chr = .virage_missing_chr,
    yes_labels = .virage_yes_labels, no_labels = .virage_no_labels
  ))

  suppressMessages(
    generate_format_script(meta3, json_path, df = .virage_dummy,
                           output_path = script_path)
  )

  expect_true(file.exists(script_path))
  script_text <- paste(readLines(script_path, encoding = "UTF-8"), collapse = "\n")

  # Script should be parseable R code
  expect_no_error(parse(text = script_text))

  # Script should contain expected patterns
  expect_true(grepl("REPRISE_ETUD", script_text), info = "should contain new variable name")
  expect_true(grepl("as\\.ordered", script_text), info = "should contain as.ordered for ordinal")
  expect_true(grepl("fct_recode", script_text), info = "should contain fct_recode calls")
})


# ===========================================================================
# INT2: Emploi dummy -- full pipeline with SAS format file
# ===========================================================================
test_that("INT2: Emploi dummy full pipeline with SAS format file", {
  withr::local_dir(.test_proj_root)
  json_path <- tmp_json()
  sas_file <- tempfile(fileext = ".sas")
  script_path <- tempfile(fileext = ".R")
  writeLines(.sas_emploi_inline, sas_file, useBytes = TRUE)
  on.exit(unlink(c(json_path, sas_file, script_path)))

  # Save original ai_call_claude for restoration
  .orig_ai <- get("ai_call_claude", envir = globalenv())
  on.exit(assign("ai_call_claude", .orig_ai, envir = globalenv()), add = TRUE)

  # --- Step 1: extract_survey_metadata with SAS format file ---
  meta <- suppressMessages(extract_survey_metadata(
    .emploi_dummy,
    sas_format_file = sas_file,
    meta_json       = json_path,
    missing_num     = .emploi_missing_num,
    missing_chr     = .emploi_missing_chr,
    yes_labels      = .emploi_yes_labels,
    no_labels       = .emploi_no_labels
  ))

  expect_true(file.exists(json_path))
  expect_equal(meta[meta$var_name == "METRODOM", ]$detected_role, "factor_binary")
  expect_equal(meta[meta$var_name == "AGED", ]$detected_role, "factor_nominal")
  expect_equal(meta[meta$var_name == "PCS1", ]$detected_role, "factor_nominal")
  expect_equal(meta[meta$var_name == "HCONT", ]$detected_role, "double")
  expect_equal(meta[meta$var_name == "NAIA", ]$detected_role, "integer")

  # --- Step 2: ai_classify_roles (mock) ---
  classify_response <- paste(
    '{"id":"AGED","role":"factor_ordinal","desc":"low_first"}',
    '{"id":"PCS1","role":"factor_nominal"}',
    '{"id":"METRODOM","role":"factor_binary","desc":"high_first"}',
    '{"id":"NAIA","role":"integer_count"}',
    sep = "\n"
  )
  assign("ai_call_claude", mock_ai(classify_response), envir = globalenv())

  suppressMessages(
    ai_classify_roles(meta, meta_json = json_path, ordinal_desc = TRUE)
  )

  json_data <- .read_meta_json(json_path)
  expect_equal(json_data$variables$AGED$role, "factor_ordinal")

  # --- Step 3: reload from JSON (with df for full level info) ---
  meta2 <- suppressMessages(extract_survey_metadata(
    .emploi_dummy, sas_format_file = sas_file, meta_json = json_path,
    missing_num = .emploi_missing_num, missing_chr = .emploi_missing_chr,
    yes_labels = .emploi_yes_labels, no_labels = .emploi_no_labels
  ))
  expect_equal(meta2[meta2$var_name == "AGED", ]$detected_role, "factor_ordinal")

  # --- Step 3b: add level stats ---
  meta2 <- suppressMessages(
    metadata_add_level_stats(meta2, .emploi_dummy, meta_json = json_path)
  )

  # --- Step 4: ai_suggest_labels (mock) ---
  labels_response <- paste0(
    '{"METRODOM": {"1": "Metropole", "2": "DOM"},',
    ' "AGED": {"00": "0-9", "10": "10-19", "20": "20-29", "30": "30-39",',
    '          "40": "40-49", "50": "50-59", "60": "60-69", "70": "70-79",',
    '          "80": "80-89", "90": "90+"},',
    ' "PCS1": {"0": "Non code", "1": "Agric.", "2": "Artisans",',
    '          "3": "Cadres", "4": "Prof. interm.", "5": "Employes", "6": "Ouvriers"}}'
  )
  assign("ai_call_claude", mock_ai(labels_response), envir = globalenv())

  suppressMessages(
    ai_suggest_labels(meta2, meta_json = json_path,
                      replace_existing_new_labels = TRUE)
  )

  json_data <- .read_meta_json(json_path)
  expect_equal(json_data$variables$METRODOM$levels[["1"]]$new_label, "Metropole")

  # --- Step 5: ai_suggest_varnames (mock) ---
  varnames_response <- paste0(
    '{"METRODOM": "METRO_DOM",',
    ' "AGED": "AGE_DEC",',
    ' "PCS1": "PCS_NIV1",',
    ' "HCONT": "H_CONTRAT",',
    ' "NAIA": "ANNEE_NAISS"}'
  )
  assign("ai_call_claude", mock_ai(varnames_response), envir = globalenv())

  suppressMessages(
    ai_suggest_varnames(meta2, meta_json = json_path)
  )

  json_data <- .read_meta_json(json_path)
  expect_equal(json_data$variables$METRODOM$new_name, "METRO_DOM")
  expect_equal(json_data$variables$HCONT$new_name, "H_CONTRAT")

  # --- Step 6: generate_format_script ---
  meta3 <- suppressMessages(extract_survey_metadata(
    .emploi_dummy, sas_format_file = sas_file, meta_json = json_path,
    missing_num = .emploi_missing_num, missing_chr = .emploi_missing_chr,
    yes_labels = .emploi_yes_labels, no_labels = .emploi_no_labels
  ))

  suppressMessages(
    generate_format_script(meta3, json_path, df = .emploi_dummy,
                           output_path = script_path)
  )

  expect_true(file.exists(script_path))
  script_text <- paste(readLines(script_path, encoding = "UTF-8"), collapse = "\n")

  expect_no_error(parse(text = script_text))
  expect_true(grepl("METRO_DOM", script_text), info = "should contain new variable name")
  expect_true(grepl("as\\.ordered", script_text), info = "should contain as.ordered for AGED ordinal")
})
