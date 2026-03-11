library(testthat)

# Detect current project root from the location of this testthat.R file
.proj_root <- normalizePath(file.path(dirname(testthat::test_path()), ".."), winslash = "/")
#.proj_root <- "~/github/socio_public_services"

source(file.path(.proj_root, "R", "data_formatting_pipeline.R"), encoding = "UTF-8")


# ===========================================================================
# Shared helpers (available to all test files)
# ===========================================================================

# Project root for tests that need instructions/*.md files
.test_proj_root <- .proj_root

tmp_json <- function() tempfile(fileext = ".json")

# Minimal meta_list suitable for .write_meta_json() / .read_meta_json()
make_meta_list <- function(vars) {
  list(
    config = list(
      dataset     = "test.dta",
      missing_num = list(-1L, 99L),
      missing_chr = list("NSP"),
      yes_labels  = list("Oui"),
      no_labels   = list("Non")
    ),
    variables = vars
  )
}

# Minimal metadata tibble row for ai_classify_roles().
# detected_role = "factor_nominal" so the variable enters the target filter.
make_classify_meta <- function(var_name, var_label,
                               labels_vec, values_vec,
                               missing_vals    = integer(0),
                               detected_role   = "factor_nominal",
                               n_distinct_data = as.integer(length(labels_vec))) {
  tibble::tibble(
    var_name        = var_name,
    var_label       = var_label,
    r_class         = "integer",
    n_distinct      = as.integer(length(labels_vec) - length(missing_vals)),
    n_distinct_data = n_distinct_data,
    labels          = list(labels_vec),
    values          = list(values_vec),
    missing_vals    = list(missing_vals),
    detected_role   = detected_role
  )
}

# Wrap a fake Haiku reply text into the list structure ai_call_claude() returns.
mock_ai <- function(text) {
  function(...) list(content = list(list(text = text)))
}

# Helper: extract metadata from a dummy dataset, returning tibble + JSON path
extract_dummy_meta <- function(dummy, missing_num, missing_chr,
                               yes_labels = NULL, no_labels = NULL,
                               sas_format_file = NULL, meta_json = NULL) {
  if (is.null(meta_json)) meta_json <- tmp_json()
  suppressMessages(extract_survey_metadata(
    dummy,
    missing_num     = missing_num,
    missing_chr     = missing_chr,
    yes_labels      = yes_labels,
    no_labels       = no_labels,
    sas_format_file = sas_format_file,
    meta_json       = meta_json
  ))
}


# ===========================================================================
# Shared dummy datasets — real data extracted via make_dummy_tibble()
# ===========================================================================

# ---------------------------------------------------------------------------
# Virage dummy (haven_labelled columns — existing use case)
# ---------------------------------------------------------------------------
# Source: Enquete Virage (INED)
# Labels are embedded as haven_labelled value labels.

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

# Virage missing values config
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

# Expected detected_role after extract_survey_metadata() (before AI)
.virage_expected_roles <- c(
  Q25E1         = "factor_binary",   # 2 non-null levels (Non/Oui) + missing (NVPD, NSP)
  MIGBIS_E      = "factor_nominal",  # 3 non-null levels (Inclassable is missing)
  Q19E_GRAGEBIS = "factor_nominal",  # 4 non-null age groups (ordinal via AI later)
  POIDS_CAL     = "double",          # continuous numeric weight
  Q19E_AGE      = "integer",         # haven_labelled<double> but all labels are missing -> integer
  ID            = "identifier"       # all unique character IDs
)


# ---------------------------------------------------------------------------
# Emploi dummy (plain character/numeric — SAS format file use case)
# ---------------------------------------------------------------------------
# Source: Enquete Emploi en continu 2024 (INSEE)
# Labels come from an external SAS format file (.sas_emploi_inline below).

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

# Emploi missing values config
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

# Expected detected_role after extract_survey_metadata() with SAS file (before AI)
.emploi_expected_roles <- c(
  METRODOM = "factor_binary",   # 2 non-null levels after SAS labels applied
  AGED     = "factor_nominal",  # 10 age decade levels (ordinal via AI later)
  PCS1     = "factor_nominal",  # 7 PCS levels
  HCONT    = "double",          # continuous hours (fractional values)
  NAIA     = "integer"          # year of birth (whole numbers)
)


# ---------------------------------------------------------------------------
# Inline SAS format string for Emploi tests
# ---------------------------------------------------------------------------
# Simplified extract from tests/labels_sas_lil-1734b.txt, keeping only the
# variables present in .emploi_dummy. Syntax is identical to real file.

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


# ---------------------------------------------------------------------------
# Edge-case dummy (synthetic — covers edge cases)
# ---------------------------------------------------------------------------
# Synthetic dataset covering: all-missing variable, single-value variable,
# haven_labelled<double> binary, missing code without label, constant numeric,
# high-cardinality unlabelled character.

.edge_dummy <- tibble::tibble(
  ALL_MISS = structure(
    c("88", "99", "88", "99", "88", "99", "88", "99", "88", "99"),
    label = "Variable tout manquant",
    class = c("haven_labelled", "vctrs_vctr", "character"),
    labels = c(NVPD = "88", NSP = "99")),
  SINGLE_VAL = structure(
    c("01", "88", "01", "88", "01", "01", "88", "01", "88", "01"),
    label = "Variable une seule valeur non-manquante",
    class = c("haven_labelled", "vctrs_vctr", "character"),
    labels = c(Reponse = "01", NVPD = "88")),
  DBL_BINARY = structure(
    c(0, 1, 0, 1, 0, 1, 0, 0, 1, 0),
    label = "Variable binaire double",
    class = c("haven_labelled", "vctrs_vctr", "double"),
    labels = c(Non = 0, Oui = 1)),
  EMPTY_LABELS = structure(
    c("1", "2", "1", "88", "2", "1", "2", "88", "1", "2"),
    label = "Variable avec code manquant sans label",
    class = c("haven_labelled", "vctrs_vctr", "character"),
    labels = c(Oui = "1", Non = "2")),
  CONST_NUM = c(42.5, 42.5, 42.5, 42.5, 42.5, 42.5, 42.5, 42.5, 42.5, 42.5),
  HIGH_CARD = c("Paris", "Lyon", "Marseille", "Toulouse", "Nice",
                "Nantes", "Strasbourg", "Bordeaux", "Lille", "Rennes")
)

# Edge-case missing values config
.edge_missing_num <- c(-1, 88, 99)
.edge_missing_chr <- c("NVPD", "NSP")
.edge_yes_labels  <- c("Oui")
.edge_no_labels   <- c("Non")

# Expected detected_role after extract_survey_metadata() (before AI)
.edge_expected_roles <- c(
  ALL_MISS     = "factor_nominal", # nd:0, but has haven_labelled labels -> factor path
  SINGLE_VAL   = "factor_nominal", # nd:1, one non-missing label
  DBL_BINARY   = "factor_binary",  # haven_labelled<double> with 2 non-missing levels Non/Oui
  EMPTY_LABELS = "factor_binary",  # 2 non-missing labels (Oui/Non), 88 is missing_num
  CONST_NUM    = "double",         # single fractional value -> double
  HIGH_CARD    = "identifier"      # 10 unique strings in 10 rows -> all unique -> identifier
)


# ===========================================================================
# Run test scripts
# ===========================================================================
test_dir(file.path(.proj_root, "tests", "testthat"))
# test_check("package_name")
