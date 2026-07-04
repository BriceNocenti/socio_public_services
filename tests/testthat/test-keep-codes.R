# Tests for the keep_codes feature: keeping original level codes as the final
# numbers instead of clean sequential numbering.
# Functions under test: .gfs_build_entries (keep_codes branch), set_keep_codes,
#   suggest_keep_codes, .write_meta_json / .read_meta_json roundtrip, and the
#   extract_survey_metadata(keep_codes=) argument + preserve.
# Prefix: KC

# Helper: write vars to a temp JSON, return the path.
kc_json <- function(vars) {
  path <- tmp_json()
  .write_meta_json(make_meta_list(vars), path)
  path
}

# A region-like nominal with non-contiguous, zero-padded codes.
kc_region_vars <- function(keep = TRUE) list(
  REGION = list(
    var_label = "Region", role = "factor_nominal", new_name = "REGION",
    keep_codes = keep,
    levels = list(
      "01" = list(order = 1L, label = "Guadeloupe", new_label = "Guadeloupe", n = 5L,  pct = 5L),
      "06" = list(order = 2L, label = "Mayotte",    new_label = "Mayotte",    n = 4L,  pct = 4L),
      "11" = list(order = 3L, label = "IDF",        new_label = "Île de France", n = 20L, pct = 20L),
      "94" = list(order = 4L, label = "Corse",      new_label = "Corse",      n = 2L,  pct = 2L)))
)


# ---------------------------------------------------------------------------
# KC1. .gfs_build_entries — keep_codes uses the original code as prefix
# ---------------------------------------------------------------------------

test_that("KC1: keep_codes prefixes with the original code, sorted by code", {
  e <- .gfs_build_entries(kc_region_vars(keep = TRUE))[[1]]
  labels <- vapply(e$levels_sorted, function(lv) .gfs_level_label(lv, e$max_order), character(1))
  expect_equal(labels, c("01-Guadeloupe", "06-Mayotte", "11-Île de France", "94-Corse"))
})

test_that("KC1b: without keep_codes, the same variable is renumbered 1..4", {
  e <- .gfs_build_entries(kc_region_vars(keep = FALSE))[[1]]
  labels <- vapply(e$levels_sorted, function(lv) .gfs_level_label(lv, e$max_order), character(1))
  expect_equal(labels, c("1-Guadeloupe", "2-Mayotte", "3-Île de France", "4-Corse"))
})

test_that("KC1c: keep_codes preserves zero padding to the widest code", {
  vars <- list(MOIS = list(
    var_label = "Mois", role = "factor_ordinal", new_name = "MOIS", keep_codes = TRUE,
    levels = list(
      "08" = list(order = 1L, label = "Août",    new_label = "Août",    n = 8L, pct = 1L),
      "07" = list(order = 2L, label = "Juillet", new_label = "Juillet", n = 6L, pct = 6L),
      "01" = list(order = 3L, label = "Janvier", new_label = "Janvier", n = 7L, pct = 7L))))
  e <- .gfs_build_entries(vars)[[1]]
  labels <- vapply(e$levels_sorted, function(lv) .gfs_level_label(lv, e$max_order), character(1))
  expect_equal(labels, c("01-Janvier", "07-Juillet", "08-Août"))   # sorted by code
})

test_that("KC1d: keep_codes extracts the leading integer of a compound code", {
  vars <- list(REG = list(
    var_label = "Region", role = "factor_nominal", new_name = "REG", keep_codes = TRUE,
    levels = list(
      "01 - GUADELOUPE"    = list(order = 1L, label = "x", new_label = "Guadeloupe", n = 1L, pct = 1L),
      "11 - ILE DE FRANCE" = list(order = 2L, label = "x", new_label = "IDF",        n = 1L, pct = 1L))))
  e <- .gfs_build_entries(vars)[[1]]
  labels <- vapply(e$levels_sorted, function(lv) .gfs_level_label(lv, e$max_order), character(1))
  expect_equal(labels, c("01-Guadeloupe", "11-IDF"))
})

test_that("KC1e: codes with no leading number fall back to normal numbering", {
  vars <- list(X = list(
    var_label = "X", role = "factor_nominal", new_name = "X", keep_codes = TRUE,
    levels = list(
      "A" = list(order = 1L, label = "Aa", new_label = "Aa", n = 1L, pct = 1L),
      "B" = list(order = 2L, label = "Bb", new_label = "Bb", n = 1L, pct = 1L),
      "C" = list(order = 3L, label = "Cc", new_label = "Cc", n = 1L, pct = 1L))))
  expect_message(e <- .gfs_build_entries(vars)[[1]], "leading number")
  labels <- vapply(e$levels_sorted, function(lv) .gfs_level_label(lv, e$max_order), character(1))
  expect_equal(labels, c("1-Aa", "2-Bb", "3-Cc"))
})

test_that("KC1f: range codes use their leading number (age / year classes)", {
  vars <- list(CL = list(
    var_label = "Age", role = "factor_ordinal", new_name = "CL", keep_codes = TRUE,
    levels = list(
      "80-84"      = list(order = 1L, label = "80 à 84 ans",  new_label = "80 à 84 ans",  n = 1L, pct = 1L),
      "85-89"      = list(order = 2L, label = "85 à 89 ans",  new_label = "85 à 89 ans",  n = 1L, pct = 1L),
      "90 et plus" = list(order = 3L, label = "90 ans et plus", new_label = "90 ans et plus", n = 1L, pct = 1L))))
  e <- .gfs_build_entries(vars)[[1]]
  labels <- vapply(e$levels_sorted, function(lv) .gfs_level_label(lv, e$max_order), character(1))
  expect_equal(labels, c("80-80 à 84 ans", "85-85 à 89 ans", "90-90 ans et plus"))
})

test_that("KC1g: a leading-text code (e.g. 'Avant 1930') makes the variable fall back", {
  vars <- list(AN = list(
    var_label = "Annee", role = "factor_ordinal", new_name = "AN", keep_codes = TRUE,
    levels = list(
      "Avant 1930" = list(order = 1L, label = "Avant 1930",  new_label = "Avant 1930",  n = 1L, pct = 1L),
      "1930-1934"  = list(order = 2L, label = "1930 à 1934", new_label = "1930 à 1934", n = 1L, pct = 1L),
      "1935-1939"  = list(order = 3L, label = "1935 à 1939", new_label = "1935 à 1939", n = 1L, pct = 1L))))
  expect_message(e <- .gfs_build_entries(vars)[[1]], "Avant 1930")
  labels <- vapply(e$levels_sorted, function(lv) .gfs_level_label(lv, e$max_order), character(1))
  expect_equal(labels, c("1-Avant 1930", "2-1930 à 1934", "3-1935 à 1939"))
})


# ---------------------------------------------------------------------------
# KC2. Serialization / roundtrip / extract preserve + argument
# ---------------------------------------------------------------------------

test_that("KC2: keep_codes survives write -> read roundtrip", {
  path <- kc_json(kc_region_vars(keep = TRUE))
  rd <- .read_meta_json(path)
  expect_true(isTRUE(rd$variables$REGION$keep_codes))
})

test_that("KC2b: keep_codes is only emitted when TRUE", {
  path <- kc_json(kc_region_vars(keep = FALSE))
  expect_false(any(grepl('"keep_codes"[[:space:]]*:', readLines(path, warn = FALSE))))
})

test_that("KC2c: extract keep_codes= sets the flag and it is preserved on re-extract", {
  df <- data.frame(V1 = factor(c("01", "06", "11", "01", "06")),
                   V2 = c(1.5, 2.5, 3.5, 4.5, 5.5))
  path <- tmp_json()
  on.exit(unlink(path), add = TRUE)
  suppressMessages(extract_survey_metadata(df, meta_json = path, keep_codes = "V1"))
  expect_true(isTRUE(.read_meta_json(path)$variables$V1$keep_codes))
  suppressMessages(extract_survey_metadata(df, meta_json = path))   # re-extract, no arg
  expect_true(isTRUE(.read_meta_json(path)$variables$V1$keep_codes))
})


# ---------------------------------------------------------------------------
# KC3. set_keep_codes()
# ---------------------------------------------------------------------------

test_that("KC3: set_keep_codes sets and removes the flag", {
  path <- kc_json(kc_region_vars(keep = FALSE))
  suppressMessages(set_keep_codes(path, "REGION"))
  expect_true(isTRUE(.read_meta_json(path)$variables$REGION$keep_codes))
  suppressMessages(set_keep_codes(path, "REGION", value = FALSE))
  expect_false(isTRUE(.read_meta_json(path)$variables$REGION$keep_codes))
})


# ---------------------------------------------------------------------------
# KC4. suggest_keep_codes() heuristic — content vocabularies, no code-structure
# ---------------------------------------------------------------------------

# Build a 3-level factor with the given labels + codes (codes default 1,2,3).
kc_factor <- function(name, labels, codes = as.character(seq_along(labels)),
                      role = "factor_nominal") {
  lv <- stats::setNames(lapply(seq_along(labels), function(i)
    list(order = i, label = labels[i], new_label = labels[i], n = 10L, pct = 10L)), codes)
  stats::setNames(list(list(var_label = name, role = role, new_name = name, levels = lv)), name)
}

test_that("KC4: flags region / PCS / month / age / decile content, not plain labels", {
  vars <- c(
    kc_factor("PLAIN",  c("A", "B", "C")),
    kc_factor("REG",    c("Bretagne", "Occitanie", "Corse", "Normandie")),
    kc_factor("PROF",   c("Agriculteurs exploitants", "Ouvriers", "Employés")),
    kc_factor("PERIODE", c("Janvier", "Février", "Mars", "Avril")),
    kc_factor("TRANCHE_AGE", c("Moins de 30 ans", "30 à 44 ans", "45 ans et plus")),
    kc_factor("REV", c("Non classé", "1er décile", "2ème décile", "3ème décile"))
  )
  cand <- suppressMessages(suggest_keep_codes(kc_json(vars)))
  expect_false("PLAIN" %in% cand)
  expect_true(all(c("REG", "PROF", "PERIODE", "TRANCHE_AGE", "REV") %in% cand))
})

test_that("KC4a: 'codes non entiers' flags range/zero-padded codes on neutral labels", {
  vars <- c(
    # range codes, ordinary labels, no name match -> flagged only by 'codes non entiers'
    kc_factor("Q", c("Faible", "Moyen", "Fort"), codes = c("00-33", "34-66", "67-99")),
    # PCS niveau-2 phrase from CS_DETAIL (+ one more) -> PCS content
    kc_factor("SOCPRO", c("Professeurs, professions scientifiques", "Chauffeurs", "Techniciens"))
  )
  cand <- suppressMessages(suggest_keep_codes(kc_json(vars)))
  expect_true("Q" %in% cand)
  expect_true("SOCPRO" %in% cand)
})

test_that("KC4b: subjective ordinals and clean 1..n scales are NOT flagged", {
  vars <- c(
    # subjective standard-of-living (was a false positive via the old NIVVIE name)
    kc_factor("NIVVIE_PERCU", c("À l'aise", "Ça va", "C'est juste", "Difficilement"),
              role = "factor_ordinal"),
    # a Likert/frequency battery with a non-contiguous code and scrambled order:
    # must NOT be flagged anymore (removed 'codes non contigus' / 'ordre' rules)
    list(FREIN_COUT = list(var_label = "Frein coût", role = "factor_ordinal",
      new_name = "FREIN_COUT", levels = list(
        "1" = list(order = 3L, label = "Pas du tout", new_label = "Pas du tout", n = 10L, pct = 10L),
        "2" = list(order = 2L, label = "Plutôt",      new_label = "Plutôt",      n = 10L, pct = 10L),
        "4" = list(order = 1L, label = "Tout à fait", new_label = "Tout à fait", n = 10L, pct = 10L))))
  )
  cand <- suppressMessages(suggest_keep_codes(kc_json(vars)))
  expect_false("NIVVIE_PERCU" %in% cand)
  expect_false("FREIN_COUT" %in% cand)
})

test_that("KC4c: name pattern flags PCS/GS/REGION/MOIS; zero-padded codes flagged", {
  vars <- c(
    kc_factor("GS_DETAIL", c("Cadres, prof. sup.", "Employés", "Ouvriers")),
    kc_factor("MOIS_REP", c("Aout", "Juillet", "Juin"), codes = c("08", "07", "06"),
              role = "factor_ordinal")
  )
  cand <- suppressMessages(suggest_keep_codes(kc_json(vars)))
  expect_true("GS_DETAIL" %in% cand)   # name + PCS content
  expect_true("MOIS_REP" %in% cand)    # name + month + zero-padded codes
})
