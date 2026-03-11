# Tests for SAS PROC FORMAT parser and pipeline integration.
#
# Covers:
#   - parse_sas_formats(): parsing format definitions, mapping, duplicates,
#     non-numeric codes, fallback (no mapping section), real SAS file
#   - apply_sas_labels(): applying labels to plain tibbles, skipping
#     already-labelled columns, leaving unmatched columns untouched
#   - Integration: extract_survey_metadata() with sas_format_file parameter,
#     using both the Emploi dummy (plain character/numeric) and the Virage
#     dummy (haven_labelled) datasets

# ===========================================================================
# Dummy datasets
# ===========================================================================

# --- Virage dummy (haven_labelled columns — existing use case) -------------
.virage_dummy <- structure(list(
  Q25E1 = structure(
    c("01", NA, NA, "00", "00", "01", "00", "01", "01", "00",
      "00", "01", "00", NA, "01", NA, NA, "00", "00", "01",
      NA, NA, "01", "00", "00", NA, "00", "01", "01", NA),
    label = "Q25E1. \u00cates-vous en reprise d'\u00e9tudes ?",
    format.sas = "$Q25E1_F",
    class = c("haven_labelled", "vctrs_vctr", "character"),
    labels = c(Non = "00", Oui = "01", NVPD = "88", NSP = "99")),
  MIGBIS_E = structure(
    c(NA, "03", NA, "03", "01", "03", NA, NA, "03", "02",
      NA, "03", "99", "01", "02", NA, "03", "99", "02", "99",
      "02", "03", NA, "03", "03", NA, "03", "02", "99", "99"),
    label = "Statut migratoire Ego-3 mod",
    format.sas = "$MIGBIS_E_F",
    class = c("haven_labelled", "vctrs_vctr", "character"),
    labels = setNames(
      c("01", "02", "03", "99"),
      c("Majoritaire ou descendant-e", "N\u00e9-e dans un DOM",
        "Immigr\u00e9-e", "Inclassable"))),
  Q19E_GRAGEBIS = structure(
    c("02", "04", "02", NA, "01", "03", "04", "02", "02", "04",
      "04", NA, "04", "01", NA, "02", "01", "03", "04", "03",
      "02", "04", NA, NA, "01", "03", NA, NA, "03", NA),
    label = "Groupe d'\u00e2ge 4 modalit\u00e9s",
    format.sas = "$GRAGEBIS",
    class = c("haven_labelled", "vctrs_vctr", "character"),
    labels = c(`20-29` = "01", `30-39` = "02",
               `40-49` = "03", `50-69` = "04")),
  POIDS_CAL = c(
    1599.22, 1567.41, 2545.13, 2008.31, 1913.36,
    3307.16, 2577.83, 1096.67, 388.26, 1931.75,
    678.89, 1883.88, 5549.99, 612.48, 1997.52,
    927.00, 1802.24, 3971.87, 1599.22, 936.81,
    1635.68, 723.73, 1844.04, 1250.46, 1886.94,
    1502.32, 1573.74, 704.89, 1268.80, 1567.41),
  Q19E_AGE = structure(
    c(30, 50, 23, 31, 58, 33, 30, 51, 52, 59,
      30, 21, 64, 36, 30, 66, 29, 40, 34, 25,
      39, 47, 20, 65, 27, 26, 30, 38, 57, 30),
    label = "Q19E_age. Quel est votre \u00e2ge ? (Synth\u00e8se des variables Q19e & Q19ea)",
    format.sas = "Q19E_AGE_F",
    class = c("haven_labelled", "vctrs_vctr", "double"),
    labels = setNames(c(99, 88),
      c("NSP ou pr\u00e9f\u00e8re indiquer l'ann\u00e9e", "NVPD"))),
  ID = structure(
    c("1001845964", "1000001885", "1000106943", "1002365859",
      "1000440541", "1001004836", "1000231317", "1002540107",
      "1002601432", "1002360133", "1000455390", "1002536082",
      "1001616571", "1002974895", "1002724063", "1002268156",
      "1000440620", "1002646486", "1000750205", "1003266901",
      "1002532488", "1003548887", "1001051419", "1003581981",
      "1004074851", "1001234137", "1001423985", "1001345462",
      "1003814284", "1000100071"),
    label = "ID. Identifiant")
), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -30L))


# --- Emploi dummy (plain character/numeric — SAS format file use case) -----
.emploi_dummy <- structure(list(
  METRODOM = structure(
    c(NA, "2", "2", NA, "2", NA, "1", "1", "1", NA,
      "2", "1", "1", "2", NA, "1", "1", NA, "2", "1",
      "1", "1", "2", "2", NA, NA, "1", "2", NA, "2"),
    label = "R\u00e9gion du logement de r\u00e9sidence en 2 modalit\u00e9s (M\u00e9tropole vs DOM)"),
  AGED = structure(
    c(NA, "90", "40", "20", "40", "10", "70", NA, "70", "80",
      "50", "90", "90", NA, "60", "10", "40", "50", "70", "50",
      "90", "70", NA, "10", NA, "30", "60", NA, "20", NA),
    label = "Age en tranche d\u00e9cennale"),
  PCS1 = structure(
    c("6", NA, NA, "6", "1", "5", "5", "4", "0", "6",
      "1", "3", NA, "1", "3", NA, NA, "2", "5", "3",
      "6", "4", "2", "4", "4", "4", NA, "2", NA, "0"),
    label = "Cat\u00e9gorie socio-professionnelle (PCS) de l'emploi principal - Niveau 1"),
  HCONT = structure(
    c(35.0, 56, 43, 35.0, 41.5, 35.0, 32, 58, 35.0, 47.5,
      23, 18.5, 37, 35.0, 8.5, 44.5, 31, 14, 6, 35.0,
      11.5, 72, 37.5, 21.5, 20.5, 10.5, 43.5, 19, 42, 35.0),
    label = "Nombre d'heures pr\u00e9vu par semaine dans le contrat de travail"),
  NAIA = structure(
    c(1990, 1977, 1973, 1990, 1964, 1990, 1998, 1990, 1957, 1939,
      1990, 1963, 1940, 1984, 1990, 1993, 1962, 1956, 1958, 1949,
      1990, 1938, 1990, 1979, 1987, 1961, 1960, 1942, 1990, 1975),
    label = "Ann\u00e9e de naissance")
), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -30L))


# --- Inline SAS format string for Emploi tests ----------------------------
.sas_emploi_inline <- '
proc format library=;

\t;value $ METRODOMf
\t\t"1"="France m\u00e9tropolitaine"
\t\t"2"="D\u00e9partements d\'outre-mer"

\t;value $ AGEDf
\t\t"00"="9 ans ou moins"
\t\t"10"="10-19 ans"
\t\t"20"="20-29 ans"
\t\t"30"="30-39 ans"
\t\t"40"="40-49 ans"
\t\t"50"="50-59 ans"
\t\t"60"="60-69 ans"
\t\t"70"="70-79 ans"
\t\t"80"="80-89 ans"
\t\t"90"="90 ans ou plus"

\t;value $ PCS1f
\t\t"0"="Non cod\u00e9"
\t\t"1"="Agriculteurs exploitants"
\t\t"2"="Artisans, commer\u00e7ants et chefs d\'entreprise"
\t\t"3"="Cadres et professions intellectuelles sup\u00e9rieures"
\t\t"4"="Professions interm\u00e9diaires"
\t\t"5"="Employ\u00e9s"
\t\t"6"="Ouvriers"

;
run;

data;
set;
format
\tMETRODOM $METRODOMf
\tAGED $AGEDf
\tPCS1 $PCS1f
;
run;
'


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


# --- Missing values for Emploi 2024 ----------------------------------------
.emploi_missing_num <- c(-1, 9999, 99999999)
.emploi_missing_chr <- c(
  "Non enqu\u00eat\u00e9", "N'a pas r\u00e9pondu", "Valeur manquante",
  "Non concern\u00e9", "Ne sait pas",
  "Vous ne savez pas", "Non r\u00e9ponse",
  "Non r\u00e9ponse ou un seul parent",
  "  Not applicable", "Not stated",
  "Refus", "Refuse de r\u00e9pondre", "Refus / Ne sait pas",
  "Je ne sais pas",
  "[Non concern\u00e9]", "Non concern\u00e9-e",
  "[Non enqu\u00eat\u00e9]", "[Aucune]",
  "NVPD", "NSP", "NVPD/NSP", "Niveau ind\u00e9termin\u00e9", "NSP/NVPD", "[NR]",
  "Ne souhaite pas r\u00e9pondre", "Ne sais pas", "Ne veut pas dire",
  "incoh\u00e9rence",
  "NSP ou pr\u00e9f\u00e8re indiquer l'ann\u00e9e",
  "NSP, non renseign\u00e9",
  "[Ne sait pas]", "[Refus]", "[NSP]"
)
.emploi_yes_labels <- c("Oui", "Choisi", "D\u00e9clare")
.emploi_no_labels  <- c("Non", "Non choisi", "Ne d\u00e9clare pas")

# --- Missing values for Virage ---------------------------------------------
.virage_missing_num <- c(-1, 88, 99, 888, 999, 8888, 9999)
.virage_missing_chr <- c(
  "Non enqu\u00eat\u00e9", "N'a pas r\u00e9pondu", "Valeur manquante",
  "Non concern\u00e9", "Ne sait pas", "",
  "Vous ne savez pas", "Refuse de r\u00e9pondre", "Je ne sais pas",
  "[Non concern\u00e9]", "Non concern\u00e9-e",
  "[Non enqu\u00eat\u00e9]",
  "NVPD", "NSP", "NVPD/NSP", "Niveau ind\u00e9termin\u00e9", "NSP/NVPD",
  "Ne souhaite pas r\u00e9pondre", "Ne sais pas", "Ne veut pas dire",
  "incoh\u00e9rence",
  "NSP ou pr\u00e9f\u00e8re indiquer l'ann\u00e9e",
  "NSP ou pr\u00e9f\u00e8re indiquer l'ann\u00e9e de naissance",
  "NSP ou pr\u00e9f\u00e8re dire depuis quelle ann\u00e9e",
  "NSP ou pr\u00e9f\u00e8re dire \u00e0 quel \u00e2ge",
  "Inclassable"
)
.virage_yes_labels <- c("Oui", "Choisi", "D\u00e9clare")
.virage_no_labels  <- c("Non", "Non choisi", "Ne d\u00e9clare pas")


# ===========================================================================
# I1: Emploi dummy — extract_survey_metadata with SAS format file
# ===========================================================================
test_that("I1: Emploi dummy detected roles are correct via sas_format_file", {
  f <- tempfile(fileext = ".sas")
  writeLines(.sas_emploi_inline, f, useBytes = TRUE)
  on.exit(unlink(f))

  meta <- suppressMessages(extract_survey_metadata(
    .emploi_dummy,
    sas_format_file = f,
    missing_num     = .emploi_missing_num,
    missing_chr     = .emploi_missing_chr,
    yes_labels      = .emploi_yes_labels,
    no_labels       = .emploi_no_labels
  ))

  expect_s3_class(meta, "tbl_df")

  # METRODOM: factor_binary (2 non-missing levels after SAS labels applied)
  row_metro <- meta[meta$var_name == "METRODOM", ]
  expect_equal(row_metro$detected_role, "factor_binary")
  expect_equal(row_metro$n_distinct, 2L)

  # PCS1: factor_nominal (7 levels)
  row_pcs <- meta[meta$var_name == "PCS1", ]
  expect_equal(row_pcs$detected_role, "factor_nominal")

  # AGED: factor_nominal (10 levels, ordinal would need AI)
  row_aged <- meta[meta$var_name == "AGED", ]
  expect_equal(row_aged$detected_role, "factor_nominal")

  # HCONT: double (continuous, no SAS format)
  row_hcont <- meta[meta$var_name == "HCONT", ]
  expect_equal(row_hcont$detected_role, "double")

  # NAIA: integer (year of birth, no SAS format)
  row_naia <- meta[meta$var_name == "NAIA", ]
  expect_equal(row_naia$detected_role, "integer")
})


# ===========================================================================
# I2: Virage dummy — haven_labelled path unchanged (no SAS file)
# ===========================================================================
test_that("I2: Virage dummy detected roles are correct without sas_format_file", {
  meta <- suppressMessages(extract_survey_metadata(
    .virage_dummy,
    missing_num = .virage_missing_num,
    missing_chr = .virage_missing_chr,
    yes_labels  = .virage_yes_labels,
    no_labels   = .virage_no_labels
  ))

  expect_s3_class(meta, "tbl_df")

  # Q25E1: factor_binary (2 non-missing levels: Non, Oui)
  row_q25 <- meta[meta$var_name == "Q25E1", ]
  expect_equal(row_q25$detected_role, "factor_binary")

  # MIGBIS_E: factor_nominal (3+ levels after excluding Inclassable)
  row_mig <- meta[meta$var_name == "MIGBIS_E", ]
  expect_true(row_mig$detected_role %in% c("factor_nominal", "factor_binary"))

  # Q19E_GRAGEBIS: factor_nominal (4 levels, ordinal would need AI)
  row_age_gr <- meta[meta$var_name == "Q19E_GRAGEBIS", ]
  expect_equal(row_age_gr$detected_role, "factor_nominal")

  # POIDS_CAL: double (continuous weight)
  row_poids <- meta[meta$var_name == "POIDS_CAL", ]
  expect_equal(row_poids$detected_role, "double")

  # Q19E_AGE: haven_labelled<double> with only missing labels (88=NVPD, 99=NSP).
  # Both labels are missing → n_clean=0 → should be detected as integer.
  row_age <- meta[meta$var_name == "Q19E_AGE", ]
  expect_equal(row_age$detected_role, "integer")

  # ID: identifier (unique character IDs)
  row_id <- meta[meta$var_name == "ID", ]
  expect_equal(row_id$detected_role, "identifier")
})


# ===========================================================================
# I3: JSON roundtrip with SAS-labelled Emploi data
# ===========================================================================
test_that("I3: Emploi dummy JSON roundtrip preserves SAS labels", {
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

  # JSON should have been written
  expect_true(file.exists(json_path))

  # Read back and check structure
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
