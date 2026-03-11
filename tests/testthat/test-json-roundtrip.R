# Tests for tibble ↔ JSON round-trip soundness in the survey_meta pipeline.
# Functions under test: .write_meta_json, .read_meta_json,
#                       .backup_meta_json, .migrate_meta_json_v3_to_v4
#

# ---------------------------------------------------------------------------
# A. Basic write → read round-trip
# ---------------------------------------------------------------------------

test_that("basic round-trip preserves config and variable fields", {
  vars <- list(
    Q_SATIS = list(
      var_label = "Satisfaction dans la vie",
      role      = "factor_ordinal",
      new_name  = "satis_vie",
      levels = list(
        "1" = list(order = 1L, label = "Pas du tout",  n = 43L,  pct = 3L),
        "2" = list(order = 2L, label = "Un peu",       n = 200L, pct = 15L),
        "3" = list(order = 3L, label = "Satisfait",    n = 580L, pct = 43L),
        "9" = list(missing = TRUE, label = "NSP")
      )
    ),
    Q_SEXE = list(
      var_label = "Sexe de la personne",
      role      = "factor_binary",
      new_name  = "sexe",
      levels = list(
        "1" = list(order = 1L, label = "Homme", new_label = "Homme", n = 600L, pct = 50L),
        "2" = list(order = 2L, label = "Femme", new_label = "Femme", n = 600L, pct = 50L)
      )
    )
  )
  path <- tmp_json()
  .write_meta_json(make_meta_list(vars), path)
  result <- .read_meta_json(path)

  # Config preserved
  expect_equal(result$config$dataset, "test.dta")
  expect_equal(unlist(result$config$missing_num), c(-1L, 99L))
  expect_equal(unlist(result$config$yes_labels),  "Oui")

  # Variable names preserved
  expect_setequal(names(result$variables), c("Q_SATIS", "Q_SEXE"))

  # Ordinal variable: order integers round-trip
  q <- result$variables[["Q_SATIS"]]
  expect_equal(q$role, "factor_ordinal")
  expect_equal(q$new_name, "satis_vie")
  expect_equal(q$levels[["1"]]$order, 1L)
  expect_equal(q$levels[["3"]]$order, 3L)
  expect_null(q$levels[["9"]]$order)
  expect_true(isTRUE(q$levels[["9"]]$missing))
  expect_equal(q$levels[["1"]]$n, 43L)
  expect_equal(q$levels[["1"]]$pct, 3L)

  # Binary variable: new_label round-trips
  s <- result$variables[["Q_SEXE"]]
  expect_equal(s$levels[["1"]]$new_label, "Homme")
  expect_equal(s$levels[["2"]]$new_label, "Femme")
})

# ---------------------------------------------------------------------------
# B. Non-consecutive value codes — the crash scenario
# ---------------------------------------------------------------------------

test_that("non-consecutive value codes do not crash .write_meta_json", {
  vars <- list(
    Q_OUI = list(
      var_label = "A voté ?",
      role      = "factor_binary",
      new_name  = "vote",
      levels = list(
        "1" = list(order = 1L, label = "Oui"),
        "9" = list(missing = TRUE, label = "NSP")
      )
    )
  )
  path <- tmp_json()
  # Must not throw
  expect_no_error(.write_meta_json(make_meta_list(vars), path))

  result <- .read_meta_json(path)
  q <- result$variables[["Q_OUI"]]
  expect_equal(q$levels[["1"]]$order, 1L)
  expect_null(q$levels[["9"]]$order)
  expect_true(isTRUE(q$levels[["9"]]$missing))
})

test_that("many non-consecutive codes (sparse) round-trip without error", {
  # Codes: 1, 2, 10, 88, 99 — only 88 and 99 are missing
  vars <- list(
    Q_PAYS = list(
      var_label = "Pays de naissance",
      role      = "factor_nominal",
      new_name  = "pays",
      levels = list(
        "1"  = list(order = 1L, label = "France"),
        "2"  = list(order = 2L, label = "Algérie"),
        "10" = list(order = 3L, label = "Autre"),
        "88" = list(missing = TRUE, label = "NVPD"),
        "99" = list(missing = TRUE, label = "NSP")
      )
    )
  )
  path <- tmp_json()
  expect_no_error(.write_meta_json(make_meta_list(vars), path))
  result <- .read_meta_json(path)
  q <- result$variables[["Q_PAYS"]]
  expect_equal(q$levels[["10"]]$order, 3L)
  expect_true(isTRUE(q$levels[["88"]]$missing))
  expect_true(isTRUE(q$levels[["99"]]$missing))
  expect_null(q$levels[["99"]]$order)
})

# ---------------------------------------------------------------------------
# C. Ordinal with merged levels (identical order integer)
# ---------------------------------------------------------------------------

test_that("merged ordinal levels (same order integer) round-trip correctly", {
  vars <- list(
    Q_SATIS = list(
      var_label = "Satisfaction vie",
      role      = "factor_ordinal",
      new_name  = "satis",
      levels = list(
        "1" = list(order = 5L, label = "Pas du tout", n = 43L,  pct = 3L),
        "2" = list(order = 5L, label = "Très peu",    n = 67L,  pct = 5L),
        "3" = list(order = 4L, label = "Peu",         n = 120L, pct = 9L),
        "4" = list(order = 3L, label = "Satisfait",   n = 580L, pct = 43L),
        "5" = list(order = 2L, label = "Très",        n = 540L, pct = 40L),
        "6" = list(order = 1L, label = "Tout à fait", n = 100L, pct = 7L),
        "9" = list(missing = TRUE, label = "NSP")
      )
    )
  )
  path <- tmp_json()
  expect_no_error(.write_meta_json(make_meta_list(vars), path))
  result <- .read_meta_json(path)
  q <- result$variables[["Q_SATIS"]]
  expect_equal(q$levels[["1"]]$order, 5L)
  expect_equal(q$levels[["2"]]$order, 5L)  # same group
  expect_equal(q$levels[["6"]]$order, 1L)
  expect_null(q$levels[["9"]]$order)
})

# ---------------------------------------------------------------------------
# D. JSON write/read round-trip preserves order and level data
# ---------------------------------------------------------------------------

test_that("JSON write/read round-trip preserves order and level structure", {
  vars <- list(
    Q1 = list(
      var_label = "Satisfaction dans la vie", role = "factor_ordinal",
      r_class = "double", new_name = "Q1",
      levels = list(
        "1" = list(label = "Pas du tout", order = 1L),
        "2" = list(label = "Un peu",      order = 2L),
        "3" = list(label = "Très",        order = 3L),
        "9" = list(label = "NSP",         missing = TRUE)
      )
    ),
    Q2 = list(
      var_label = "Sexe", role = "factor_binary",
      r_class = "double", new_name = "Q2",
      levels = list(
        "1" = list(label = "Homme", order = 1L),
        "2" = list(label = "Femme", order = 2L),
        "9" = list(label = "NSP",   missing = TRUE)
      )
    )
  )

  path <- tmp_json()
  on.exit(unlink(path))
  .write_meta_json(make_meta_list(vars), path)

  # File must exist and be valid JSON
  expect_true(file.exists(path))
  raw <- .read_meta_json(path)
  expect_equal(length(raw$variables), 2L)

  # Reload via .load_meta
  meta2 <- .load_meta(path)$meta

  # Order preserved in levels list
  q1_idx <- which(meta2$var_name == "Q1")
  lvls_q1 <- meta2$levels[[q1_idx]]
  expect_equal(lvls_q1[["1"]]$order, 1L)
  expect_equal(lvls_q1[["2"]]$order, 2L)
  expect_equal(lvls_q1[["3"]]$order, 3L)
  expect_true(isTRUE(lvls_q1[["9"]]$missing))
  expect_null(lvls_q1[["9"]]$order)

  q2_idx <- which(meta2$var_name == "Q2")
  lvls_q2 <- meta2$levels[[q2_idx]]
  expect_equal(lvls_q2[["1"]]$order, 1L)
  expect_equal(lvls_q2[["2"]]$order, 2L)
  expect_true(isTRUE(lvls_q2[["9"]]$missing))
})

# ---------------------------------------------------------------------------
# E. Edge cases
# ---------------------------------------------------------------------------

test_that("variable with zero levels writes empty levels block", {
  vars <- list(
    Q_ID = list(
      var_label = "Identifiant",
      role      = "identifier",
      new_name  = "id",
      levels    = list()
    )
  )
  path <- tmp_json()
  expect_no_error(.write_meta_json(make_meta_list(vars), path))
  result <- .read_meta_json(path)
  expect_equal(length(result$variables[["Q_ID"]]$levels), 0L)
})

test_that("variable with ALL levels missing still writes correctly", {
  vars <- list(
    Q_MISS = list(
      var_label = "Variable entièrement manquante",
      role      = "other",
      new_name  = "q_miss",
      levels = list(
        "98" = list(missing = TRUE, label = "NVPD"),
        "99" = list(missing = TRUE, label = "NSP")
      )
    )
  )
  path <- tmp_json()
  expect_no_error(.write_meta_json(make_meta_list(vars), path))
  result <- .read_meta_json(path)
  q <- result$variables[["Q_MISS"]]
  expect_true(isTRUE(q$levels[["98"]]$missing))
  expect_true(isTRUE(q$levels[["99"]]$missing))
  expect_null(q$levels[["98"]]$order)
})

test_that("new_label on some non-missing levels: has_new_label detected correctly", {
  vars <- list(
    Q_LBL = list(
      var_label = "Question avec new_label partiel",
      role      = "factor_nominal",
      new_name  = "q_lbl",
      levels = list(
        "1" = list(order = 1L, label = "Oui",  new_label = "Oui",  n = 60L, pct = 60L),
        "2" = list(order = 2L, label = "Non",  new_label = "Non",  n = 40L, pct = 40L),
        "9" = list(missing = TRUE, label = "NSP")
      )
    )
  )
  path <- tmp_json()
  expect_no_error(.write_meta_json(make_meta_list(vars), path))
  result <- .read_meta_json(path)
  q <- result$variables[["Q_LBL"]]
  expect_equal(q$levels[["1"]]$new_label, "Oui")
  expect_equal(q$levels[["2"]]$new_label, "Non")
  # missing level must not have new_label
  expect_null(q$levels[["9"]]$new_label)
})

test_that("new_label only on first level (partial): second level gets empty string in JSON", {
  vars <- list(
    Q_PART_LBL = list(
      var_label = "Partiel new_label sans n/pct",
      role      = "factor_nominal",
      new_name  = "q_part_lbl",
      levels = list(
        "1" = list(order = 1L, label = "Oui", new_label = "Oui"),
        "2" = list(order = 2L, label = "Non"),   # no new_label
        "9" = list(missing = TRUE, label = "NSP")
      )
    )
  )
  path <- tmp_json()
  expect_no_error(.write_meta_json(make_meta_list(vars), path))
  result <- .read_meta_json(path)
  q <- result$variables[["Q_PART_LBL"]]
  expect_equal(q$levels[["1"]]$new_label, "Oui")
  # level "2" had no new_label; JSON stores "" which jsonlite reads back as ""
  expect_true(is.null(q$levels[["2"]]$new_label) || q$levels[["2"]]$new_label == "")
  expect_null(q$levels[["9"]]$new_label)
})

test_that("n/pct only on some levels: column alignment does not crash", {
  vars <- list(
    Q_PART = list(
      var_label = "Variable avec n/pct partiel",
      role      = "factor_ordinal",
      new_name  = "q_part",
      levels = list(
        "1" = list(order = 1L, label = "A", n = 100L, pct = 50L),
        "2" = list(order = 2L, label = "B"),   # no n/pct
        "9" = list(missing = TRUE, label = "NSP")
      )
    )
  )
  path <- tmp_json()
  expect_no_error(.write_meta_json(make_meta_list(vars), path))
  result <- .read_meta_json(path)
  expect_equal(result$variables[["Q_PART"]]$levels[["1"]]$n, 100L)
  expect_null(result$variables[["Q_PART"]]$levels[["2"]]$n)
})

test_that(".backup_meta_json creates file in .survey_meta subdirectory", {
  path <- tmp_json()
  # Create dummy JSON file to back up
  writeLines('{"config":{}}', path)
  dest <- .backup_meta_json(path, "test_step")
  expect_true(file.exists(dest))
  backup_dir <- file.path(dirname(path), ".survey_meta")
  expect_true(dir.exists(backup_dir))
  expect_true(startsWith(dirname(dest), backup_dir))
  expect_true(grepl("test_step", basename(dest)))
})

test_that(".backup_meta_json returns NULL invisibly when file does not exist", {
  result <- .backup_meta_json(tmp_json(), "step")
  expect_null(result)
})

# ---------------------------------------------------------------------------
# F. Auto-migration: old v3 JSON (null_coded / desc) → v4 (missing / order)
# ---------------------------------------------------------------------------

test_that("auto-migration: null_coded:true → missing:true, order assigned", {
  # Write a v3-style JSON manually (bypassing .write_meta_json to avoid it
  # writing v4 format automatically)
  path <- tmp_json()
  v3_json <- '{
  "config": { "dataset": "old.dta" },
  "variables": {
    "Q_OLD": {
      "var_label": "Ancienne variable",
      "role": "factor_ordinal",
      "new_name": "q_old",
      "levels": {
        "1": { "label": "Faible" },
        "2": { "label": "Moyen" },
        "3": { "label": "Fort" },
        "9": { "null_coded": true, "label": "NSP" }
      }
    }
  }
}'
  writeLines(v3_json, path)

  result <- .read_meta_json(path)  # triggers migration

  q <- result$variables[["Q_OLD"]]
  # null_coded must become missing:true
  expect_true(isTRUE(q$levels[["9"]]$missing))
  expect_null(q$levels[["9"]]$null_coded)
  # Non-missing levels must have sequential order
  expect_equal(q$levels[["1"]]$order, 1L)
  expect_equal(q$levels[["2"]]$order, 2L)
  expect_equal(q$levels[["3"]]$order, 3L)
  expect_null(q$levels[["9"]]$order)
  # desc field removed
  expect_null(q$desc)
})

test_that("auto-migration: desc:true on ordinal → reversed order integers", {
  path <- tmp_json()
  v3_json <- '{
  "config": {},
  "variables": {
    "Q_DESC": {
      "var_label": "Satisfaction",
      "role": "factor_ordinal",
      "desc": true,
      "new_name": "q_desc",
      "levels": {
        "1": { "label": "Pas du tout" },
        "2": { "label": "Peu" },
        "3": { "label": "Beaucoup" },
        "9": { "null_coded": true, "label": "NSP" }
      }
    }
  }
}'
  writeLines(v3_json, path)
  result <- .read_meta_json(path)
  q <- result$variables[["Q_DESC"]]
  # desc:true on ordinal → reversed: level "1" gets order 3, "3" gets order 1
  expect_equal(q$levels[["1"]]$order, 3L)
  expect_equal(q$levels[["2"]]$order, 2L)
  expect_equal(q$levels[["3"]]$order, 1L)
  expect_null(q$desc)
})

test_that("auto-migration is idempotent: re-reading migrated JSON changes nothing", {
  path <- tmp_json()
  v3_json <- '{
  "config": {},
  "variables": {
    "Q_IDEM": {
      "var_label": "Test idempotent",
      "role": "factor_nominal",
      "new_name": "q_idem",
      "levels": {
        "1": { "label": "A" },
        "9": { "null_coded": true, "label": "NSP" }
      }
    }
  }
}'
  writeLines(v3_json, path)
  r1 <- .read_meta_json(path)  # migrates & rewrites
  r2 <- .read_meta_json(path)  # reads already-migrated file

  expect_equal(r1$variables[["Q_IDEM"]]$levels[["1"]]$order,
               r2$variables[["Q_IDEM"]]$levels[["1"]]$order)
  expect_equal(r1$variables[["Q_IDEM"]]$levels[["9"]]$missing,
               r2$variables[["Q_IDEM"]]$levels[["9"]]$missing)
})

test_that("auto-migration: v4 JSON (already has order) is left unchanged", {
  path <- tmp_json()
  v4_json <- '{
  "config": {},
  "variables": {
    "Q_V4": {
      "var_label": "Déjà migré",
      "role": "factor_binary",
      "new_name": "q_v4",
      "levels": {
        "1": { "order": 1, "label": "Oui" },
        "2": { "order": 2, "label": "Non" },
        "9": { "missing": true, "label": "NSP" }
      }
    }
  }
}'
  writeLines(v4_json, path)
  result <- .read_meta_json(path)
  q <- result$variables[["Q_V4"]]
  expect_equal(q$levels[["1"]]$order, 1L)
  expect_equal(q$levels[["2"]]$order, 2L)
  expect_null(q$desc)
})

# ---------------------------------------------------------------------------
# G. Special characters in labels and variable names
# ---------------------------------------------------------------------------

test_that("special characters in labels survive JSON round-trip", {
  vars <- list(
    `Q_ÉTÉ` = list(
      var_label = "Êtes-vous d'accord ?",
      role      = "factor_binary",
      new_name  = "q_ete",
      levels = list(
        "1" = list(order = 1L, label = 'Oui, "tout à fait"',
                   new_label = "Oui"),
        "2" = list(order = 2L, label = "Non \\(pas du tout\\)",
                   new_label = "Non"),
        "9" = list(missing = TRUE, label = "NSP")
      )
    )
  )
  path <- tmp_json()
  expect_no_error(.write_meta_json(make_meta_list(vars), path))
  result <- .read_meta_json(path)
  q <- result$variables[["Q_ÉTÉ"]]
  expect_equal(q$levels[["1"]]$label,     'Oui, "tout à fait"')
  expect_equal(q$levels[["2"]]$label,     "Non \\(pas du tout\\)")
  expect_equal(q$levels[["1"]]$new_label, "Oui")
})

# ---------------------------------------------------------------------------
# H. Key/join soundness: val_labs inner-join against observed unique values
# ---------------------------------------------------------------------------

# Helper: build a minimal haven_labelled column with given values and label map
make_labelled_col <- function(values, label_map) {
  # label_map: named numeric vector, names = label text, values = numeric code
  labelled::labelled(values, labels = label_map)
}

# Helper: build a one-variable survey df suitable for extract_survey_metadata()
make_survey_df <- function(col, var_label = "Test variable") {
  df <- tibble::tibble(Q1 = col)
  labelled::var_label(df$Q1) <- var_label
  df
}

test_that("H1: val_labs codes absent from data are dropped — no n=0 keys in JSON", {
  # Codes 1,2,3,9 defined in labels, but only 1 and 2 appear in the data
  col <- make_labelled_col(
    c(1, 2, 1, 2, 1),
    c("Oui" = 1, "Non" = 2, "Autre" = 3, "NSP" = 9)
  )
  df   <- make_survey_df(col)
  path <- tmp_json()
  on.exit(unlink(path))
  suppressMessages(
    extract_survey_metadata(df, path, missing_num = c(9), missing_chr = character(0))
  )
  lvls <- .read_meta_json(path)$variables$Q1$levels
  # Only codes actually in data (1, 2) should appear; 3 and 9 are absent
  expect_false("3" %in% names(lvls))
  expect_false("9" %in% names(lvls))
  expect_true("1" %in% names(lvls))
  expect_true("2" %in% names(lvls))
})

test_that("H2: numeric-numeric match works for double column with integer val_labs codes", {
  # Column stored as double (1.0, 2.0), val_labs use integer codes 1L, 2L
  col <- make_labelled_col(
    as.double(c(1, 2, 1, 2)),
    c("Oui" = 1L, "Non" = 2L, "NSP" = 9L)
  )
  df   <- make_survey_df(col)
  path <- tmp_json()
  on.exit(unlink(path))
  suppressMessages(
    extract_survey_metadata(df, path, missing_num = c(9), missing_chr = character(0))
  )
  lvls <- .read_meta_json(path)$variables$Q1$levels
  # Codes 1 and 2 must match despite double vs integer storage
  expect_true("1" %in% names(lvls))
  expect_true("2" %in% names(lvls))
  # Code 9 not in data — must be absent
  expect_false("9" %in% names(lvls))
})

test_that("H3: string fallback works for character column with text value labels", {
  # Character column — numeric conversion of observed values and codes would fail
  col <- labelled::labelled(
    c("A", "B", "A", "B"),
    labels = c("Category A" = "A", "Category B" = "B", "Unknown" = "Z")
  )
  df   <- make_survey_df(col)
  path <- tmp_json()
  on.exit(unlink(path))
  suppressMessages(
    extract_survey_metadata(df, path, missing_num = numeric(0), missing_chr = character(0))
  )
  lvls <- .read_meta_json(path)$variables$Q1$levels
  expect_true("A" %in% names(lvls))
  expect_true("B" %in% names(lvls))
  # "Z" is in labels but not in data — must be dropped
  expect_false("Z" %in% names(lvls))
})

test_that("H4: empty string in data is excluded from unique values after import_survey() na_if step", {
  # Simulate the na_if transformation that import_survey() applies:
  # column has values c("1","","2",NA) — after na_if("") the "" becomes NA
  col_raw <- labelled::labelled(
    c("1", "", "2", NA_character_),
    labels = c("Oui" = "1", "Non" = "2")
  )
  # Apply the same transformation as import_survey():
  col_clean <- dplyr::na_if(col_raw, "")

  # "" must be gone; only "1" and "2" are valid non-NA values
  vals_present <- unique(col_clean[!is.na(col_clean)])
  expect_false("" %in% as.character(vals_present))
  expect_true("1" %in% as.character(vals_present))
  expect_true("2" %in% as.character(vals_present))

  # val_labels must still be intact after na_if
  labs <- labelled::val_labels(col_clean)
  expect_equal(length(labs), 2L)
})

test_that("H5: dropped val_labs codes do not appear as keys in the JSON", {
  # codes 1, 2 observed; code 99 in labels but absent from data
  col <- make_labelled_col(
    c(1, 2, 1, 2),
    c("Oui" = 1, "Non" = 2, "NSP" = 99)
  )
  df   <- make_survey_df(col)
  path <- tmp_json()
  on.exit(unlink(path))
  suppressMessages(
    extract_survey_metadata(df, path, missing_num = c(99), missing_chr = character(0))
  )
  q <- .read_meta_json(path)$variables[["Q1"]]
  expect_false("99" %in% names(q$levels))
  expect_true("1"  %in% names(q$levels))
  expect_true("2"  %in% names(q$levels))
})

test_that("H6: metadata_add_level_stats never produces n=0 for any key when join is sound", {
  col <- make_labelled_col(
    c(1, 2, 1, 1, 2),
    c("Oui" = 1, "Non" = 2, "NSP" = 9)
  )
  df   <- make_survey_df(col)
  path <- tmp_json()
  on.exit(unlink(path))
  suppressMessages(
    extract_survey_metadata(df, path, missing_num = c(9), missing_chr = character(0))
  )
  suppressMessages(metadata_add_level_stats(path, df))

  raw <- .read_meta_json(path)
  q   <- raw$variables[["Q1"]]
  # All non-missing levels must have n > 0
  non_miss_lvls <- Filter(function(l) !isTRUE(l$missing), q$levels)
  for (lv in non_miss_lvls) {
    if (!is.null(lv$n)) expect_true(lv$n > 0L)
  }
  # "1" and "2" should NOT be missing
  expect_false(isTRUE(q$levels[["1"]]$missing))
  expect_false(isTRUE(q$levels[["2"]]$missing))
})

# ---------------------------------------------------------------------------
# I. Empty string NA conversion in import_survey()
# ---------------------------------------------------------------------------

test_that("I1: na_if transformation preserves val_labels and var_label on haven_labelled", {
  # Simulate what import_survey() does: apply dplyr::na_if("") to the column
  col <- labelled::labelled(
    c("1", "", "2", "1", NA_character_),
    labels = c("Oui" = "1", "Non" = "2")
  )
  labelled::var_label(col) <- "Ma variable"

  col_clean <- dplyr::na_if(col, "")

  # "" converted to NA
  expect_equal(sum(is.na(col_clean)), 2L)  # original NA + empty string

  # val_labels preserved
  labs <- labelled::val_labels(col_clean)
  expect_equal(unname(labs), c("1", "2"))
  expect_equal(names(labs),  c("Oui", "Non"))

  # var_label preserved
  expect_equal(labelled::var_label(col_clean), "Ma variable")
})

test_that("I2: import_survey() na_if is applied via dplyr::across to all char/factor cols", {
  # Test the actual transformation logic used in import_survey() on a tibble
  df_raw <- tibble::tibble(
    Q1 = c("1", "", "2", NA_character_),
    Q2 = c(1L, 2L, 1L, NA_integer_)   # integer — must be untouched
  )
  # Apply the same across() call as import_survey():
  df_clean <- dplyr::mutate(df_raw, dplyr::across(
    dplyr::where(~ is.character(.) || is.factor(.)),
    ~ dplyr::na_if(., "")
  ))
  # Q1: "" → NA
  expect_equal(sum(is.na(df_clean$Q1)), 2L)
  expect_false("" %in% df_clean$Q1[!is.na(df_clean$Q1)])
  # Q2: unchanged (integer)
  expect_equal(df_clean$Q2, df_raw$Q2)
})

# ---------------------------------------------------------------------------
# J. JSON ↔ .load_meta() roundtrip completeness (labels, role, r_class)
# ---------------------------------------------------------------------------

# Shared fixture: JSON vars list for J-section tests
.j_vars <- list(
  Q1 = list(
    var_label = "Satisfaction dans la vie", role = "factor_ordinal",
    r_class = "double", new_name = "Q1",
    levels = list(
      "1" = list(label = "Pas du tout", order = 1L),
      "2" = list(label = "Un peu",      order = 2L),
      "3" = list(label = "Très",        order = 3L),
      "9" = list(label = "NSP",         missing = TRUE)
    )
  ),
  Q2 = list(
    var_label = "Sexe", role = "factor_binary",
    r_class = "double", new_name = "Q2",
    levels = list(
      "1" = list(label = "Homme", order = 1L),
      "2" = list(label = "Femme", order = 2L),
      "9" = list(label = "NSP",   missing = TRUE)
    )
  )
)

test_that("J1: labels survive JSON write → .load_meta() roundtrip", {
  path <- tmp_json()
  on.exit(unlink(path))
  .write_meta_json(make_meta_list(.j_vars), path)

  meta <- .load_meta(path)$meta

  q1_idx <- which(meta$var_name == "Q1")
  expect_equal(meta$levels[[q1_idx]][["1"]]$label, "Pas du tout")
  expect_equal(meta$levels[[q1_idx]][["2"]]$label, "Un peu")
  expect_equal(meta$levels[[q1_idx]][["3"]]$label, "Très")
  expect_true(isTRUE(meta$levels[[q1_idx]][["9"]]$missing))

  q2_idx <- which(meta$var_name == "Q2")
  expect_equal(meta$levels[[q2_idx]][["1"]]$label, "Homme")
  expect_equal(meta$levels[[q2_idx]][["2"]]$label, "Femme")
})

test_that("J2: detected_role stored and restored via JSON", {
  path <- tmp_json()
  on.exit(unlink(path))
  .write_meta_json(make_meta_list(.j_vars), path)

  meta <- .load_meta(path)$meta

  expect_equal(meta$detected_role[meta$var_name == "Q1"], "factor_ordinal")
  expect_equal(meta$detected_role[meta$var_name == "Q2"], "factor_binary")
})

test_that("J3: r_class roundtrip + backward compat with old JSON lacking r_class", {
  # r_class survives write → read
  path <- tmp_json()
  on.exit(unlink(path))
  .write_meta_json(make_meta_list(.j_vars), path)
  raw <- .read_meta_json(path)
  expect_equal(raw$variables[["Q1"]]$r_class, "double")

  meta <- .load_meta(path)$meta
  expect_equal(meta$r_class[meta$var_name == "Q1"], "double")

  # Backward compat: old JSON without r_class → empty string, no crash
  path2 <- tmp_json()
  on.exit(unlink(path2), add = TRUE)
  old_json <- '{"config": {}, "variables": {
    "Q_OLD": {
      "var_label": "Old var", "role": "factor_nominal", "new_name": "q_old",
      "levels": {"1": {"order": 1, "label": "A"}, "2": {"order": 2, "label": "B"}}
    }
  }}'
  writeLines(old_json, path2)

  meta2 <- .load_meta(path2)$meta
  expect_true(meta2$r_class[meta2$var_name == "Q_OLD"] %in% c("", NA_character_))
})

test_that("J4: .load_meta() reconstructs meta from JSON without df", {
  # Write a JSON with known structure
  vars <- list(
    Q1 = list(
      var_label = "Satisfaction dans la vie",
      role      = "factor_ordinal",
      r_class   = "double",
      new_name  = "satis",
      levels    = list(
        "1" = list(order = 1L, label = "Pas du tout"),
        "2" = list(order = 2L, label = "Un peu"),
        "3" = list(order = 3L, label = "Très"),
        "9" = list(missing = TRUE, label = "NSP")
      )
    ),
    Q2 = list(
      var_label = "Sexe",
      role      = "factor_binary",
      r_class   = "integer",
      new_name  = "sexe",
      levels    = list(
        "1" = list(order = 1L, label = "Homme"),
        "2" = list(order = 2L, label = "Femme")
      )
    )
  )
  path <- tmp_json()
  on.exit(unlink(path))
  .write_meta_json(make_meta_list(vars), path)

  loaded <- .load_meta(path)
  meta   <- loaded$meta

  # New-style columns present
  expected_cols <- c("var_name", "var_label", "r_class", "detected_role",
                     "new_name", "n_distinct", "n_distinct_data", "levels")
  for (col in expected_cols) {
    expect_true(col %in% names(meta), info = paste("Missing column:", col))
  }

  q1_idx <- which(meta$var_name == "Q1")
  expect_equal(meta$detected_role[q1_idx], "factor_ordinal")
  expect_equal(meta$var_label[q1_idx],    "Satisfaction dans la vie")
  expect_equal(meta$r_class[q1_idx],      "double")

  # n_distinct: 3 non-missing levels
  expect_equal(meta$n_distinct[q1_idx], 3L)

  # levels preserved
  lvls <- meta$levels[[q1_idx]]
  expect_equal(names(lvls), c("1", "2", "3", "9"))
  expect_true(isTRUE(lvls[["9"]]$missing))
  expect_equal(lvls[["1"]]$label, "Pas du tout")
})

test_that("J5: extract_survey_metadata requires both df and meta_json", {
  # meta_json is now required — missing it raises an error
  df_test <- tibble::tibble(X = c(1, 2))
  expect_error(
    extract_survey_metadata(df_test),
    "meta_json"
  )
})

test_that("J6: JSON vars not in df are ignored on re-extract", {
  # df has only Q1, JSON has Q1 + Q_EXTRA (from previous run)
  col <- make_labelled_col(c(1, 2, 1, 2), c("Oui" = 1, "Non" = 2))
  df  <- make_survey_df(col, "Test Q1")

  path <- tmp_json()
  on.exit(unlink(path))
  extra_json <- '{"config": {}, "variables": {
    "Q1": {
      "var_label": "Test Q1", "role": "factor_binary", "new_name": "q1",
      "levels": {"1": {"order": 1, "label": "Oui"}, "2": {"order": 2, "label": "Non"}}
    },
    "Q_EXTRA": {
      "var_label": "Extra var", "role": "factor_nominal", "new_name": "q_extra",
      "levels": {"1": {"order": 1, "label": "A"}}
    }
  }}'
  writeLines(extra_json, path)

  suppressMessages(extract_survey_metadata(df, path))
  # JSON should now only contain Q1 (Q_EXTRA dropped because not in df)
  json_vars <- .read_meta_json(path)$variables
  expect_true("Q1" %in% names(json_vars))
  expect_false("Q_EXTRA" %in% names(json_vars))
})

test_that("J7: df vars not in JSON keep df-extracted levels in JSON", {
  # df has Q1, JSON is empty (first run)
  col <- make_labelled_col(c(1, 2, 1), c("Oui" = 1, "Non" = 2))
  df  <- make_survey_df(col, "My variable")

  path <- tmp_json()
  on.exit(unlink(path))
  writeLines('{"config": {}, "variables": {}}', path)

  suppressMessages(extract_survey_metadata(df, path))
  json_vars <- .read_meta_json(path)$variables
  expect_true("Q1" %in% names(json_vars))
  expect_true("1" %in% names(json_vars$Q1$levels))
  expect_true("2" %in% names(json_vars$Q1$levels))
  # var_label from df preserved
  expect_equal(json_vars$Q1$var_label, "My variable")
})

test_that("J8: n_distinct_data absent from JSON does not crash ai_classify_roles", {
  # Write a JSON without n_distinct_data field (simulates old JSON)
  path <- tmp_json()
  on.exit(unlink(path))
  vars <- list(Q_AGE = list(
    var_label = "Age", role = "factor_nominal", new_name = "Q_AGE",
    levels = list("99" = list(missing = TRUE, label = "NSP"))
  ))
  .write_meta_json(make_meta_list(vars), path)

  # Mock ai_call_claude to avoid real API call (nd=0 → auto-classified)
  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude", mock_ai("{}"), envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)

  # Should not error when n_distinct_data is absent from JSON
  expect_no_error(
    suppressMessages(ai_classify_roles(path))
  )
})

test_that("J9: n/pct accessible via $levels after .load_meta()", {
  vars <- list(
    Q1 = list(
      var_label = "Test", role = "factor_nominal", new_name = "Q1",
      levels = list(
        "1" = list(order = 1L, label = "A", n = 60L, pct = 60L),
        "2" = list(order = 2L, label = "B", n = 40L, pct = 40L)
      )
    )
  )
  path <- tmp_json()
  on.exit(unlink(path))
  .write_meta_json(make_meta_list(vars), path)

  meta <- .load_meta(path)$meta

  lvls <- meta$levels[[1]]
  expect_equal(lvls[["1"]]$n,   60L)
  expect_equal(lvls[["2"]]$n,   40L)
  expect_equal(lvls[["1"]]$pct, 60L)
  expect_equal(lvls[["2"]]$pct, 40L)
})


# ===========================================================================
# K1: .load_meta() preserves new_label when only some levels have it
# ===========================================================================
# Regression: VAR_A has new_label on all levels, VAR_B has none.
# Ensures VAR_B's label is preserved (not overwritten) by .load_meta().

test_that("K1: .load_meta() preserves labels when only some vars have new_label", {
  vars <- list(
    VAR_A = list(
      var_label = "Variable A", role = "factor_binary",
      r_class = "integer", new_name = "VAR_A",
      levels = list(
        "1" = list(label = "Oui", new_label = "A vot\u00e9",   order = 1L, n = 80L),
        "2" = list(label = "Non", new_label = "Pas vot\u00e9", order = 2L, n = 20L)
      )
    ),
    VAR_B = list(
      var_label = "Variable B", role = "factor_binary",
      r_class = "double", new_name = "VAR_B",
      levels = list(
        "1" = list(label = "Oui", order = 1L, n = 449L),
        "2" = list(label = "Non", order = 2L, n = 8196L)
      )
    )
  )

  path <- tmp_json()
  on.exit(unlink(path))
  .write_meta_json(make_meta_list(vars), path)

  meta <- .load_meta(path)$meta

  # VAR_A: new_label present
  va_idx <- which(meta$var_name == "VAR_A")
  expect_equal(meta$levels[[va_idx]][["1"]]$new_label, "A vot\u00e9")
  expect_equal(meta$levels[[va_idx]][["2"]]$new_label, "Pas vot\u00e9")

  # VAR_B: no new_label — original label preserved
  vb_idx <- which(meta$var_name == "VAR_B")
  expect_equal(meta$levels[[vb_idx]][["1"]]$label, "Oui")
  expect_equal(meta$levels[[vb_idx]][["2"]]$label, "Non")
  expect_null(meta$levels[[vb_idx]][["1"]]$new_label)
  expect_null(meta$levels[[vb_idx]][["2"]]$new_label)
})
