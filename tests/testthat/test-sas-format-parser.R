# Tests for SAS PROC FORMAT parser: parse_sas_formats() and apply_sas_labels().
#
# Covers:
#   - parse_sas_formats(): parsing format definitions, mapping, duplicates,
#     non-numeric codes, fallback (no mapping section), real SAS file
#   - apply_sas_labels(): applying labels to plain tibbles, skipping
#     already-labelled columns, leaving unmatched columns untouched,
#     variable labels
#
# Uses .sas_emploi_inline from testthat.R for basic tests.
# Integration tests (extract_survey_metadata + SAS) are in test-extract-metadata.R.


# ===========================================================================
# P1: parse_sas_formats — basic parsing
# ===========================================================================
test_that("P1: parse_sas_formats parses basic format definitions and mapping", {
  f <- tempfile(fileext = ".sas")
  writeLines(.sas_emploi_inline, f, useBytes = TRUE)
  on.exit(unlink(f))

  result <- parse_sas_formats(f)
  expect_type(result, "list")
  expect_named(result, c("value_labels", "var_labels"))
  expect_type(result$value_labels, "list")

  # METRODOM: 2 labels
  expect_true("METRODOM" %in% names(result$value_labels))
  metro <- result$value_labels[["METRODOM"]]
  expect_length(metro, 2L)
  expect_equal(unname(metro), c("1", "2"))

  # AGED: 10 labels
  expect_true("AGED" %in% names(result$value_labels))
  expect_length(result$value_labels[["AGED"]], 10L)

  # PCS1: 7 labels
  expect_true("PCS1" %in% names(result$value_labels))
  expect_length(result$value_labels[["PCS1"]], 7L)
})


# ===========================================================================
# P2: Duplicate format names — last definition wins
# ===========================================================================
test_that("P2: parse_sas_formats keeps last definition for duplicate formats", {
  sas_dup <- '
proc format library=;
\t;value $ PARENTSf
\t\t"1"="1"
\t\t"2"="2"
\t\t"3"="3"
\t\t"4"="4"

\t;value $ PARENTSf
\t\t"1"="M\u00e8re seule"
\t\t"2"="P\u00e8re seul"
\t\t"3"="Deux parents pr\u00e9sents"
\t\t"4"="Aucun parent pr\u00e9sent"
;
run;

data;
set;
format
\tPARENTS $PARENTSf
;
run;
'
  f <- tempfile(fileext = ".sas")
  writeLines(sas_dup, f, useBytes = TRUE)
  on.exit(unlink(f))

  result <- parse_sas_formats(f)
  labs <- result$value_labels[["PARENTS"]]
  # Last definition should win — labels should be descriptive, not "1","2","3","4"
  expect_true(any(grepl("seule?", names(labs), ignore.case = TRUE)))
})


# ===========================================================================
# P3: Non-numeric codes
# ===========================================================================
test_that("P3: parse_sas_formats handles non-numeric codes (I-A, 1a, B)", {
  sas_codes <- '
proc format library=;
\t;value $ PCSLf
\t\t"I-A"="Cadre avec cadre"
\t\t"I-B"="Cadre avec profession interm\u00e9diaire"
\t\t"II-A"="Cadre avec employ\u00e9 ou ouvrier"

\t;value $ CARf
\t\t"0"="Permanente"
\t\t"1a"="Personne vivant chez parents, rattach\u00e9e"
\t\t"1b"="Personne vivant chez parents, non rattach\u00e9e"
\t\t"5c"="Etudiant mineur"

\t;value $ STCOMM2020f
\t\t"B"="Banlieue"
\t\t"C"="Centre"
\t\t"H"="Hors AU"
\t\t"I"="Isol\u00e9e"
;
run;

data;
set;
format
\tPCSL $PCSLf
\tCAR $CARf
\tSTCOMM2020 $STCOMM2020f
;
run;
'
  f <- tempfile(fileext = ".sas")
  writeLines(sas_codes, f, useBytes = TRUE)
  on.exit(unlink(f))

  result <- parse_sas_formats(f)

  # PCSL: codes "I-A", "I-B", "II-A"
  pcsl <- result$value_labels[["PCSL"]]
  expect_true("I-A" %in% pcsl)
  expect_true("II-A" %in% pcsl)
  expect_length(pcsl, 3L)

  # CAR: codes "0", "1a", "1b", "5c"
  car_labs <- result$value_labels[["CAR"]]
  expect_true("1a" %in% car_labs)
  expect_true("5c" %in% car_labs)
  expect_length(car_labs, 4L)

  # STCOMM2020: codes "B", "C", "H", "I"
  st <- result$value_labels[["STCOMM2020"]]
  expect_true("B" %in% st)
  expect_length(st, 4L)
})


# ===========================================================================
# P4: No mapping section — fallback strips trailing f
# ===========================================================================
test_that("P4: parse_sas_formats falls back to stripping trailing f", {
  sas_no_map <- '
proc format library=;
\t;value $ SEXEf
\t\t"1"="Homme"
\t\t"2"="Femme"

\t;value $ TRIMf
\t\t"1"="T1"
\t\t"2"="T2"
\t\t"3"="T3"
\t\t"4"="T4"
;
run;
'
  f <- tempfile(fileext = ".sas")
  writeLines(sas_no_map, f, useBytes = TRUE)
  on.exit(unlink(f))

  result <- parse_sas_formats(f)

  # Should derive SEXE from SEXEf, TRIM from TRIMf
  expect_true("SEXE" %in% names(result$value_labels))
  expect_true("TRIM" %in% names(result$value_labels))
  expect_length(result$value_labels[["SEXE"]], 2L)
  expect_length(result$value_labels[["TRIM"]], 4L)
})


# ===========================================================================
# P5: Real SAS format file
# ===========================================================================
test_that("P5: parse_sas_formats parses the real Enquete emploi SAS file", {
  sas_path <- file.path(.test_proj_root, "tests", "labels_sas_lil-1734b.txt")
  skip_if_not(file.exists(sas_path), "Real SAS format file not found")

  result <- parse_sas_formats(sas_path)

  # METRODOM: 2 labels
  expect_true("METRODOM" %in% names(result$value_labels))
  expect_length(result$value_labels[["METRODOM"]], 2L)

  # AGED: 10 labels (00 through 90)
  expect_true("AGED" %in% names(result$value_labels))
  expect_length(result$value_labels[["AGED"]], 10L)

  # PCS1: 7 labels (0 through 6)
  expect_true("PCS1" %in% names(result$value_labels))
  expect_length(result$value_labels[["PCS1"]], 7L)

  # SEXE: 2 labels
  expect_true("SEXE" %in% names(result$value_labels))
  expect_length(result$value_labels[["SEXE"]], 2L)

  # Total mapped variables should be substantial
  expect_true(length(result$value_labels) > 50)
})


# ===========================================================================
# P6: apply_sas_labels — character column gets haven_labelled
# ===========================================================================
test_that("P6: apply_sas_labels applies labels to plain character column", {
  df <- tibble::tibble(
    METRODOM = c("1", "2", "1", NA, "2")
  )
  sas_parsed <- list(
    value_labels = list(
      METRODOM = c("France m\u00e9tropolitaine" = "1",
                   "D\u00e9partements d'outre-mer" = "2")
    ),
    var_labels = character(0)
  )

  result <- apply_sas_labels(df, sas_parsed)
  expect_true(inherits(result$METRODOM, "haven_labelled"))

  labs <- labelled::val_labels(result$METRODOM)
  expect_length(labs, 2L)
  expect_equal(unname(labs[1]), "1")
})


# ===========================================================================
# P7: apply_sas_labels — already-labelled column not overwritten
# ===========================================================================
test_that("P7: apply_sas_labels skips already haven_labelled columns", {
  labelled_col <- structure(
    c("01", "02", "01"),
    class = c("haven_labelled", "vctrs_vctr", "character"),
    labels = c(Non = "01", Oui = "02")
  )
  df <- tibble::tibble(MYVAR = labelled_col)

  sas_parsed <- list(
    value_labels = list(
      MYVAR = c("Diff\u00e9rent" = "01", "Autre" = "02")
    ),
    var_labels = character(0)
  )

  result <- apply_sas_labels(df, sas_parsed)
  # Should keep original labels, not SAS labels
  labs <- labelled::val_labels(result$MYVAR)
  expect_equal(names(labs)[1], "Non")
})


# ===========================================================================
# P8: apply_sas_labels — unmatched columns left untouched
# ===========================================================================
test_that("P8: apply_sas_labels leaves unmatched columns untouched", {
  df <- tibble::tibble(
    HCONT = c(35.5, 42.0, 18.5),
    NAIA  = c(1990, 1975, 2001)
  )
  sas_parsed <- list(
    value_labels = list(
      METRODOM = c("France" = "1", "DOM" = "2")
    ),
    var_labels = character(0)
  )

  result <- apply_sas_labels(df, sas_parsed)
  expect_false(inherits(result$HCONT, "haven_labelled"))
  expect_false(inherits(result$NAIA, "haven_labelled"))
  expect_equal(result$HCONT, df$HCONT)
  expect_equal(result$NAIA, df$NAIA)
})


# ===========================================================================
# P9: apply_sas_labels — variable labels applied when missing from R
# ===========================================================================
test_that("P9: apply_sas_labels applies variable labels when R column has none", {
  df <- tibble::tibble(
    MYVAR = c("1", "2", "1")
  )
  sas_parsed <- list(
    value_labels = list(),
    var_labels = c(MYVAR = "Description from SAS")
  )

  result <- apply_sas_labels(df, sas_parsed)
  expect_equal(attr(result$MYVAR, "label"), "Description from SAS")
})


test_that("P9b: apply_sas_labels does NOT overwrite existing R variable label", {
  df <- tibble::tibble(
    MYVAR = structure(c("1", "2"), label = "Existing R label")
  )
  sas_parsed <- list(
    value_labels = list(),
    var_labels = c(MYVAR = "SAS label would replace")
  )

  result <- apply_sas_labels(df, sas_parsed)
  expect_equal(attr(result$MYVAR, "label"), "Existing R label")
})
