# Tests for INSEE nomenclature helpers:
#   create_nomenclatures_json(), add_nomenclature_to_json(),
#   detect_nomenclature_vars(), apply_nomenclatures()
#
# Fixtures: in-memory minimal nomenclature JSON (via tempfile).
# The real Excel files are NOT used here (external path dependency).

# ===========================================================================
# Dummy tibble from Enquête Emploi 2024 (60 rows, 6 variables)
# ===========================================================================

.ee_dummy <- structure(list(
  FAP_PROFA = structure(
    c("A0X43", "L4X81", "V3X80g", "A3X90", "A0X41f", "G0B41d", "F1X31a",
      "F0X33", "V2X92", "R3X83", "V2X93a", "B2Xxxx", "A0X41e", "S0X40",
      "R4X93a", "E2X40", "B6X71d", "L0X60d", "W0X92", "P2X90g", "V4X80",
      "V0X60b", "G0A40b", "A0X42a", "G0A40a", "QDX01", "A0X41b", "L5X91",
      "B6X74", "V4X85c", "W1X80b", "F1X31b", "V1X80a", "P2X90x", "J6X90b",
      "G1X77b", "M2X92a", "D2X31", "G0B41c", "G1X71a", "B2X36a", "J6X90a",
      "B6X71x", "F1X30", "R1X68", "S2X81", "A3X40a", "V3X71a", "A0X41d",
      "T4X60", "J3X44", "V3X80d", "G1X72b", "E2X20", "G0A41", "G1X77a",
      "U1X93", "U1X82a", "R0X61", "J4Xxxx"),
    label = "Famille professionnelle FAP-2021 (emploi principal ou dernier emploi)"),
  NAFG038N = structure(
    c("CC", "BZ", NA, "QA", "JC", "CK", "OZ", "LZ", "OZ", "FZ", "CJ",
      "GZ", "CM", NA, "TZ", NA, "CL", "FZ", "00", "JA", "SZ", NA, "CG",
      "QA", "EZ", "IZ", "DZ", "MC", "QB", "KZ", "EZ", "BZ", "MA", "UZ",
      "CB", "CF", "CE", "PZ", "SZ", "00", "CG", "JB", NA, "CL", "CH",
      "CA", "MB", "AZ", "CE", "FZ", NA, "CD", "RZ", "CI", "MB", NA,
      "CA", "TZ", "NZ", "HZ"),
    label = "Activite economique (NA 38 postes)"),
  NAFG129N = structure(
    c("I56Z", "2000", "C17A", "R91Z", "C29B", "J60Z", "C24A", "N79Z",
      "C22A", "2500", "C20A", "J58Z", "3200", "H49B", "U99Z", "H49A",
      "C10G", "4900", "C22B", "C26D", "A03Z", "C32B", "H53Z", "L68A",
      "C26G", "2300", "F41A", "R92Z", "C29A", "1700", "C25D", "C30E",
      "2200", "C15Z", "A02Z", "Q87Z", "E39Z", "C16Z", "C32C", "R90Z",
      "C27B", "C20C", "J61Z", "B09Z", "B06Z", "G45Z", "C10K", "R93Z",
      "C27A", "C21Z", "C23A", "K66Z", "6800", "N81Z", "D35B", "S96Z",
      "G47Z", "C10B", "C26B", "N80Z"),
    label = "Activite economique (NA 129 postes)"),
  NAFN = structure(
    c("3600Z", "23000", "2229B", "4782Z", "4332C", "6511Z", "3900Z",
      "4778C", "1431Z", "50000", "8710B", "9104Z", "7430Z", "7219Z",
      "4799B", "8622B", "2732Z", "8520Z", "8899A", "62000", "4772A",
      "9001Z", "5223Z", "4399D", "4312B", "2849Z", "3319Z", "6910Z",
      "24000", "7810Z", "4646Z", "0142Z", "3213Z", "7820Z", "9512Z",
      "4665Z", "4632B", "42000", "7211Z", "4759A", "4669C", "1310Z",
      "5590Z", "68000", "2599A", "5122Z", "2740Z", "8299Z", "2434Z",
      "2530Z", "81000", "2015Z", "1082Z", "3312Z", "3315Z", "2364Z",
      "51000", "7021Z", "60000", "2011Z"),
    label = "Activite economique (NAF rev 2, 732 postes)"),
  PCS3 = structure(
    c("38A", "35C", "43B", "43A", "65C", "68A", "55B", "370", "10A",
      "10B", "67D", "56B", "47D", "55D", "62E", "680", "630", "48D",
      "38G", "37E", "64B", "520", "470", "33A", "22B", "400", "64A",
      "52C", "67C", "300", "54A", "53B", "65B", "21D", "31A", "600",
      "42C", "48A", "63D", "53D", "43D", "56C", "46E", "63F", "23A",
      "47A", "37D", "56A", "43C", "47B", "33E", "37C", "380", "46G",
      "530", "48C", "37F", "220", "21E", "63A"),
    label = "PCS niveau 3"),
  PCS4 = structure(
    c("3500", "54B1", "63C0", "46F0", "34A2", "10A6", "38C3", "63C1",
      "35A1", "21E3", "48C1", "38E1", "43D7", "35B2", "54D2", "62A2",
      "65C1", "21E4", "21B2", "62E3", "43C3", "54B2", "10A3", "48C2",
      "38F4", "33A2", "43D4", "65A1", "37D2", "46D1", "47D3", "53A3",
      "34B0", "33A1", "37B5", "68D0", "31A1", "54A3", "33C0", "42A3",
      "34C2", "47C2", "46E0", "54A1", "34C4", "68A0", "44A1", "4800",
      "62C0", "69A0", "63B6", "22B0", "22A1", "43A1", "4500", "46E2",
      "67D2", "62A0", "62E1", "65B1"),
    label = "PCS niveau 4")),
  class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -60L))

# ===========================================================================
# Fixture helpers
# ===========================================================================

# Minimal nomenclature list for testing (no real Excel files needed)
.make_min_nom_list <- function() {
  list(
    `_schema` = list(
      description = "Test nomenclatures",
      fields = list(
        `nomenclatures.ID.var_label` = "Label de la nomenclature",
        `nomenclatures.ID.levels.CODE.label` = "Libelle du code"
      )
    ),
    nomenclatures = list(
      NAF_rev2 = list(
        var_label = "NAF Rev.2",
        source    = "INSEE",
        version   = "Rev.2",
        levels    = list(
          `3900Z` = list(label = "Depollution et autres services de gestion des dechets"),
          `3600Z` = list(label = "Captage, traitement et distribution d'eau"),
          `6910Z` = list(label = "Activites juridiques")
        )
      ),
      PCS2020_N3 = list(
        var_label = "PCS 2020 N3",
        source    = "INSEE",
        version   = "PCS 2020",
        levels    = list(
          `38A` = list(label = "Ingenieurs et cadres d'etude, R et D en informatique"),
          `65B` = list(label = "Conducteurs de vehicules"),
          `370` = list(label = "Cadres des services administratifs et commerciaux des entreprises"),
          `380` = list(label = "Ingenieurs et cadres techniques d'entreprise")
        )
      ),
      FAP2021_341 = list(
        var_label = "FAP 2021 niveau 341",
        source    = "DARES",
        version   = "FAP 2021",
        levels    = list(
          A0X43  = list(label = "Conducteurs d'engins agricoles"),
          A0X41f = list(label = "Aides d'eleveurs"),
          L4X81  = list(label = "Cadres de la banque et assurance")
        )
      )
    )
  )
}

# ===========================================================================
# Tests: .write_nomenclatures_json / .read_nomenclatures_json
# ===========================================================================

test_that(".write_nomenclatures_json produces valid JSON readable by .read_nomenclatures_json", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))

  nom <- .make_min_nom_list()
  .write_nomenclatures_json(nom, tmp)
  expect_true(file.exists(tmp))

  roundtrip <- .read_nomenclatures_json(tmp)
  expect_named(roundtrip, c("_schema", "nomenclatures"))
  expect_true("NAF_rev2" %in% names(roundtrip$nomenclatures))
  expect_equal(
    roundtrip$nomenclatures$NAF_rev2$levels[["3900Z"]]$label,
    "Depollution et autres services de gestion des dechets"
  )
})

test_that(".read_nomenclatures_json returns empty list if file absent", {
  result <- .read_nomenclatures_json("/non/existent/file.json")
  expect_equal(result, list(nomenclatures = list()))
})

# ===========================================================================
# Tests: create_nomenclatures_json
# ===========================================================================

test_that("create_nomenclatures_json does not overwrite existing file", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))

  # Pre-create with sentinel content
  writeLines('{"sentinel": true}', tmp)
  original_mtime <- file.info(tmp)$mtime

  # Should NOT overwrite
  expect_message(
    create_nomenclatures_json(
      naf_path = "does_not_exist.xls",
      fap_path = "does_not_exist.xlsx",
      pcs_path = "does_not_exist.xlsx",
      path = tmp
    ),
    "d\u00e9j\u00e0 existant"
  )
  expect_equal(file.info(tmp)$mtime, original_mtime)
})

# ===========================================================================
# Tests: add_nomenclature_to_json
# ===========================================================================

test_that("add_nomenclature_to_json adds a nomenclature to existing JSON", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(c(tmp, paste0(dirname(tmp), "/.survey_meta"))))

  # Start with a minimal valid file
  .write_nomenclatures_json(
    list(`_schema` = list(description = "test"), nomenclatures = list()),
    tmp
  )

  df_codes <- data.frame(
    code  = c("01A", "02B", "03C"),
    label = c("Secteur un", "Secteur deux", "Secteur trois"),
    stringsAsFactors = FALSE
  )

  expect_message(
    add_nomenclature_to_json("TEST_NOM", df_codes, path = tmp,
                             var_label = "Nomenclature test",
                             source = "Test", version = "2024"),
    "TEST_NOM"
  )

  result <- .read_nomenclatures_json(tmp)
  expect_true("TEST_NOM" %in% names(result$nomenclatures))
  expect_equal(result$nomenclatures$TEST_NOM$levels[["01A"]]$label, "Secteur un")
  expect_equal(length(result$nomenclatures$TEST_NOM$levels), 3L)
})

test_that("add_nomenclature_to_json warns when replacing existing nomenclature", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))

  .write_nomenclatures_json(.make_min_nom_list(), tmp)

  df_new <- data.frame(code = "ZZZ", label = "New code", stringsAsFactors = FALSE)
  expect_warning(
    add_nomenclature_to_json("NAF_rev2", df_new, path = tmp),
    "d\u00e9j\u00e0 pr\u00e9sente"
  )
  result <- .read_nomenclatures_json(tmp)
  expect_equal(length(result$nomenclatures$NAF_rev2$levels), 1L)
  expect_equal(result$nomenclatures$NAF_rev2$levels[["ZZZ"]]$label, "New code")
})

test_that("add_nomenclature_to_json errors on invalid df_codes", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  .write_nomenclatures_json(.make_min_nom_list(), tmp)
  expect_error(
    add_nomenclature_to_json("X", data.frame(a = 1, b = 2), path = tmp),
    "columns 'code' and 'label'"
  )
})

# ===========================================================================
# Tests: detect_nomenclature_vars
# ===========================================================================

# Write a minimal survey_meta JSON using actual .ee_dummy values as level codes.
# detect_nomenclature_vars() identifies nomenclature type by matching level codes.
.make_nom_detect_json <- function() {
  vars_list <- c("FAP_PROFA", "NAFG038N", "NAFG129N", "NAFN", "PCS3", "PCS4")
  variables <- purrr::set_names(
    lapply(vars_list, function(vn) {
      codes <- unique(na.omit(.ee_dummy[[vn]]))
      list(
        var_label = vn,
        role      = "factor_nominal",
        new_name  = vn,
        levels    = purrr::set_names(
          lapply(codes, function(code) list(label = code)),
          codes
        )
      )
    }),
    vars_list
  )
  path <- tmp_json()
  .write_meta_json(list(config = list(), variables = variables), path)
  path
}

test_that("detect_nomenclature_vars detects FAP_PROFA as FAP2021_341", {
  path <- .make_nom_detect_json()
  on.exit(unlink(path))
  mapping <- suppressMessages(detect_nomenclature_vars(path))
  expect_equal(mapping[["FAP_PROFA"]], "FAP2021_341")
})

test_that("detect_nomenclature_vars detects NAFN as NAF_rev2", {
  path <- .make_nom_detect_json()
  on.exit(unlink(path))
  mapping <- suppressMessages(detect_nomenclature_vars(path))
  expect_equal(mapping[["NAFN"]], "NAF_rev2")
})

test_that("detect_nomenclature_vars detects PCS3 as PCS2020_N3", {
  path <- .make_nom_detect_json()
  on.exit(unlink(path))
  mapping <- suppressMessages(detect_nomenclature_vars(path))
  expect_equal(mapping[["PCS3"]], "PCS2020_N3")
})

test_that("detect_nomenclature_vars detects PCS4 as PCS2020_N4", {
  path <- .make_nom_detect_json()
  on.exit(unlink(path))
  mapping <- suppressMessages(detect_nomenclature_vars(path))
  expect_equal(mapping[["PCS4"]], "PCS2020_N4")
})

test_that("detect_nomenclature_vars detects NAFG038N as NAF_38N", {
  path <- .make_nom_detect_json()
  on.exit(unlink(path))
  mapping <- suppressMessages(detect_nomenclature_vars(path))
  expect_equal(mapping[["NAFG038N"]], "NAF_38N")
})

test_that("detect_nomenclature_vars detects NAFG129N as NAF_129N", {
  path <- .make_nom_detect_json()
  on.exit(unlink(path))
  mapping <- suppressMessages(detect_nomenclature_vars(path))
  expect_equal(mapping[["NAFG129N"]], "NAF_129N")
})

# ===========================================================================
# Tests: apply_nomenclatures
# ===========================================================================

# Helper: write a minimal survey_meta.json with given variables/levels
.make_nom_meta_json <- function(vars_levels, path) {
  variables <- purrr::imap(vars_levels, function(lvls, vname) {
    list(
      var_label = vname,
      role      = "factor_nominal",
      new_name  = vname,
      levels    = purrr::set_names(
        lapply(lvls, function(lbl) list(label = lbl)),
        lvls
      )
    )
  })
  .write_meta_json(list(config = list(), variables = variables), path)
}

test_that("apply_nomenclatures enriches new_label from nomenclature JSON", {
  tmp_nom  <- tempfile(fileext = ".json")
  tmp_meta <- tempfile(fileext = ".json")
  on.exit(unlink(c(tmp_nom, tmp_meta)))
  .write_nomenclatures_json(.make_min_nom_list(), tmp_nom)

  # NAFN has codes "3900Z", "6910Z"; PCS3 has code "38A"
  .make_nom_meta_json(
    list(NAFN = c("3900Z", "6910Z", "XXXXZ"), PCS3 = c("38A", "65B")),
    tmp_meta
  )

  mapping <- list(NAFN = "NAF_rev2", PCS3 = "PCS2020_N3")
  suppressWarnings(apply_nomenclatures(tmp_meta, mapping, nom_json = tmp_nom))

  written <- .read_meta_json(tmp_meta)
  expect_equal(
    written$variables$NAFN$levels[["3900Z"]]$new_label,
    "Depollution et autres services de gestion des dechets"
  )
  expect_equal(
    written$variables$PCS3$levels[["38A"]]$new_label,
    "Ingenieurs et cadres d'etude, R et D en informatique"
  )
})

test_that("apply_nomenclatures warns but does not error on missing code", {
  tmp_nom  <- tempfile(fileext = ".json")
  tmp_meta <- tempfile(fileext = ".json")
  on.exit(unlink(c(tmp_nom, tmp_meta)))
  .write_nomenclatures_json(.make_min_nom_list(), tmp_nom)

  # Map NAFN to NAF_rev2 which only has 3 codes → many unmatched
  .make_nom_meta_json(list(NAFN = c("3900Z", "XXXXZ", "9999Z")), tmp_meta)

  expect_warning(
    apply_nomenclatures(tmp_meta, list(NAFN = "NAF_rev2"), nom_json = tmp_nom),
    "absents de 'NAF_rev2'"
  )
})

test_that("apply_nomenclatures warns on unknown variable name", {
  tmp_nom  <- tempfile(fileext = ".json")
  tmp_meta <- tempfile(fileext = ".json")
  on.exit(unlink(c(tmp_nom, tmp_meta)))
  .write_nomenclatures_json(.make_min_nom_list(), tmp_nom)

  .make_nom_meta_json(list(NAFN = c("3900Z")), tmp_meta)

  expect_warning(
    apply_nomenclatures(tmp_meta, list(UNKNOWN_VAR = "NAF_rev2"), nom_json = tmp_nom),
    "absente du JSON"
  )
})

test_that("apply_nomenclatures warns on unknown nomenclature key", {
  tmp_nom  <- tempfile(fileext = ".json")
  tmp_meta <- tempfile(fileext = ".json")
  on.exit(unlink(c(tmp_nom, tmp_meta)))
  .write_nomenclatures_json(.make_min_nom_list(), tmp_nom)

  .make_nom_meta_json(list(NAFN = c("3900Z")), tmp_meta)

  expect_warning(
    apply_nomenclatures(tmp_meta, list(NAFN = "UNKNOWN_NOM"), nom_json = tmp_nom),
    "absente du JSON"
  )
})

test_that("apply_nomenclatures with dry_run does not write to JSON", {
  tmp_nom  <- tempfile(fileext = ".json")
  tmp_meta <- tempfile(fileext = ".json")
  on.exit(unlink(c(tmp_nom, tmp_meta)))
  .write_nomenclatures_json(.make_min_nom_list(), tmp_nom)

  .make_nom_meta_json(list(NAFN = c("3900Z", "6910Z")), tmp_meta)

  suppressWarnings(
    apply_nomenclatures(tmp_meta, list(NAFN = "NAF_rev2"),
                        nom_json = tmp_nom, dry_run = TRUE)
  )

  # dry_run: JSON must not have new_label written
  written <- .read_meta_json(tmp_meta)
  expect_null(written$variables$NAFN$levels[["3900Z"]]$new_label)
})

test_that("apply_nomenclatures writes new_label to JSON", {
  tmp_nom  <- tempfile(fileext = ".json")
  tmp_meta <- tempfile(fileext = ".json")
  on.exit(unlink(c(tmp_nom, tmp_meta)))
  .write_nomenclatures_json(.make_min_nom_list(), tmp_nom)

  .make_nom_meta_json(list(NAFN = c("3900Z", "3600Z")), tmp_meta)

  suppressWarnings(
    apply_nomenclatures(tmp_meta, list(NAFN = "NAF_rev2"), nom_json = tmp_nom)
  )

  written <- .read_meta_json(tmp_meta)
  expect_equal(
    written$variables$NAFN$levels[["3900Z"]]$new_label,
    "Depollution et autres services de gestion des dechets"
  )
})

test_that("apply_nomenclatures replaces existing new_label in JSON", {
  tmp_nom  <- tempfile(fileext = ".json")
  tmp_meta <- tempfile(fileext = ".json")
  on.exit(unlink(c(tmp_nom, tmp_meta)))
  .write_nomenclatures_json(.make_min_nom_list(), tmp_nom)

  # Pre-existing new_label
  .write_meta_json(list(
    config    = list(),
    variables = list(
      NAFN = list(
        var_label = "NAF", role = "factor_nominal", new_name = "NAFN",
        levels = list(
          `3900Z` = list(label = "Code brut 3900Z", new_label = "Ancien libelle")
        )
      )
    )
  ), tmp_meta)

  suppressWarnings(
    apply_nomenclatures(tmp_meta, list(NAFN = "NAF_rev2"), nom_json = tmp_nom)
  )

  written <- .read_meta_json(tmp_meta)
  expect_equal(
    written$variables$NAFN$levels[["3900Z"]]$new_label,
    "Depollution et autres services de gestion des dechets"
  )
})
