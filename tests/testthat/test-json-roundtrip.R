# Tests for tibble ↔ JSON round-trip soundness in the survey_meta pipeline.
# Functions under test: .write_meta_json, .read_meta_json, .write_initial_meta_json,
#                       metadata_apply_meta_json, .backup_meta_json, .migrate_meta_json_v3_to_v4
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
# D. .write_initial_meta_json → metadata_apply_meta_json round-trip
# ---------------------------------------------------------------------------

test_that(".write_initial_meta_json + metadata_apply_meta_json round-trip order correctly", {
  meta_tbl <- tibble::tibble(
    var_name      = c("Q1", "Q2"),
    var_label     = c("Satisfaction dans la vie", "Sexe"),
    detected_role = c("factor_ordinal", "factor_binary"),
    new_name      = c("Q1", "Q2"),
    values        = list(c("1", "2", "3", "9"), c("1", "2", "9")),
    labels        = list(
      c("Pas du tout", "Un peu", "Très", "NSP"),
      c("Homme", "Femme", "NSP")
    ),
    missing_vals  = list(c("9"), c("9")),
    new_labels    = list(
      c(NA_character_, NA_character_, NA_character_, "NULL"),
      c(NA_character_, NA_character_, "NULL")
    ),
    order         = list(
      c(1L, 2L, 3L, NA_integer_),
      c(1L, 2L, NA_integer_)
    )
  )

  path <- tmp_json()
  .write_initial_meta_json(meta_tbl, path)

  # File must exist and be valid JSON
  expect_true(file.exists(path))
  raw <- .read_meta_json(path)
  expect_true(length(raw$variables) == 2L)

  # Apply JSON back to the tibble
  meta2 <- metadata_apply_meta_json(meta_tbl, raw$variables)

  # order list-col: non-missing levels have integers, missing levels have NA_integer_
  ord_q1 <- meta2$order[[which(meta2$var_name == "Q1")]]
  expect_equal(ord_q1[1:3], c(1L, 2L, 3L))
  expect_true(is.na(ord_q1[4]))

  ord_q2 <- meta2$order[[which(meta2$var_name == "Q2")]]
  expect_equal(ord_q2[1:2], c(1L, 2L))
  expect_true(is.na(ord_q2[3]))
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

test_that("H1: val_labs codes absent from data are dropped — no n=0 keys in metadata", {
  # Codes 1,2,3,9 defined in labels, but only 1 and 2 appear in the data
  col <- make_labelled_col(
    c(1, 2, 1, 2, 1),
    c("Oui" = 1, "Non" = 2, "Autre" = 3, "NSP" = 9)
  )
  df  <- make_survey_df(col)
  meta <- extract_survey_metadata(df, missing_num = c(9), missing_chr = character(0))

  vals <- meta$values[[which(meta$var_name == "Q1")]]
  # Only codes actually in data (1, 2) should appear; 3 and 9 are absent
  expect_false("3" %in% as.character(vals))
  expect_false("9" %in% as.character(vals))
  expect_true("1" %in% as.character(vals))
  expect_true("2" %in% as.character(vals))
})

test_that("H2: numeric-numeric match works for double column with integer val_labs codes", {
  # Column stored as double (1.0, 2.0), val_labs use integer codes 1L, 2L
  col <- make_labelled_col(
    as.double(c(1, 2, 1, 2)),
    c("Oui" = 1L, "Non" = 2L, "NSP" = 9L)
  )
  df  <- make_survey_df(col)
  meta <- extract_survey_metadata(df, missing_num = c(9), missing_chr = character(0))

  vals <- meta$values[[which(meta$var_name == "Q1")]]
  # Codes 1 and 2 must match despite double vs integer storage
  expect_true("1" %in% as.character(vals))
  expect_true("2" %in% as.character(vals))
  # Code 9 not in data — must be absent
  expect_false("9" %in% as.character(vals))
})

test_that("H3: string fallback works for character column with text value labels", {
  # Character column — numeric conversion of observed values and codes would fail
  col <- labelled::labelled(
    c("A", "B", "A", "B"),
    labels = c("Category A" = "A", "Category B" = "B", "Unknown" = "Z")
  )
  df  <- make_survey_df(col)
  meta <- extract_survey_metadata(df, missing_num = numeric(0), missing_chr = character(0))

  vals <- meta$values[[which(meta$var_name == "Q1")]]
  expect_true("A" %in% as.character(vals))
  expect_true("B" %in% as.character(vals))
  # "Z" is in labels but not in data — must be dropped
  expect_false("Z" %in% as.character(vals))
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

test_that("H5: dropped val_labs codes do not appear as keys in the initial JSON", {
  # codes 1, 2 observed; code 99 in labels but absent from data
  col <- make_labelled_col(
    c(1, 2, 1, 2),
    c("Oui" = 1, "Non" = 2, "NSP" = 99)
  )
  df  <- make_survey_df(col)
  meta <- extract_survey_metadata(df, missing_num = c(99), missing_chr = character(0))

  # Write initial JSON and check it has no "99" key
  path <- tmp_json()
  .write_initial_meta_json(meta, path)
  raw <- .read_meta_json(path)
  q   <- raw$variables[["Q1"]]
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
  meta <- extract_survey_metadata(df, missing_num = c(9), missing_chr = character(0))

  path <- tmp_json()
  .write_initial_meta_json(meta, path)
  meta2 <- metadata_add_level_stats(meta, df, meta_json = path)

  # All level_counts entries must be > 0 (no phantom zero-count keys)
  for (cnts in meta2$level_counts) {
    if (length(cnts) > 0) expect_true(all(cnts > 0L))
  }

  # JSON must not have any key marked missing that was not already missing in meta
  raw <- .read_meta_json(path)
  q   <- raw$variables[["Q1"]]
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
