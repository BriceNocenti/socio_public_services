# Tests for apply_sas_value_labels() and the df-aware SAS resolver helpers.
#
# Prefix "V". Covers case-insensitive matching (lowercase SAS names vs upper-case
# df columns), reliable trailing-"f" handling (as-is match wins, strip is a
# fallback), value-only application (codes unchanged), variable-label preservation,
# numeric columns, unmatched reporting, and overwrite behaviour.
#
# Uses .sas_emploi_inline and .emploi_dummy from testthat.R for the V10 parity test.
# French accents via \uXXXX only. Temp files via tempfile() + on.exit(unlink()).

# ---------------------------------------------------------------------------
# Local SAS fixtures (inspired by the real INSEE pps20 format script:
# mixed/lower-case format+variable names, a variable genuinely ending in "f",
# a numeric-coded format, a non-coercible-on-numeric format, an absent variable)
# ---------------------------------------------------------------------------

v_sas_mapped <- '
proc format library=;

\t;value $ pap_HipHopf
\t\t"0"="Non"
\t\t"1"="Oui"

\t;value $ pap_tir_sptff
\t\t"0"="Non"
\t\t"1"="Oui"

\t;value $ SEXEf
\t\t"h"="Homme"
\t\t"f"="Femme"

\t;value $ AGE_Df
\t\t"20"="20-29 ans"
\t\t"30"="30-39 ans"
\t\t"50"="50-59 ans"

\t;value $ WEIRDf
\t\t"h"="Bizarre h"
\t\t"f"="Bizarre f"
\t\t"x"="Bizarre x"

\t;value $ FOOf
\t\t"0"="Non"
\t\t"1"="Oui"

;
run;

\tlabel sexe="Sexe de l\'enqu\u00eat\u00e9-e";
\tlabel pap_HipHop="Danse hip-hop";

data;
set;
format
\tpap_HipHop $pap_HipHopf
\tpap_tir_sptf $pap_tir_sptff
\tSEXE $SEXEf
\tAGE_D $AGE_Df
\tWEIRD $WEIRDf
\tFOO $FOOf
;
run;
'

# No "data;" mapping block -> resolution falls back to the format names.
v_sas_nomap <- '
proc format library=;

\t;value $ SEXEf
\t\t"h"="Homme"
\t\t"f"="Femme"

\t;value $ CL_AGEf
\t\t"20-24"="20 \u00e0 24 ans"
\t\t"25-29"="25 \u00e0 29 ans"

\t;value $ CHOMFf
\t\t"0"="Non"
\t\t"1"="Oui"

;
run;
'

# A df mirroring the live pps20 shape: all-upper-case columns, character codes,
# plus numeric columns and a decoy column absent from the SAS script.
v_df_sport <- function() {
  tibble::tibble(
    PAP_HIPHOP   = c("0", "1", "0"),
    PAP_TIR_SPTF = c("0", "1", "1"),   # variable genuinely ending in "f"
    PAP_TIR_SPT  = c("0", "1", "0"),   # decoy, absent from the SAS script
    SEXE         = c("h", "f", "h"),
    AGE_D        = c(20, 30, 50),       # numeric, codes coercible
    WEIRD        = c(1, 2, 3)           # numeric, codes NOT coercible -> skipped
  )
}


# ===========================================================================
# V0: resolver helpers - case-insensitive match + reliable trailing-"f"
# ===========================================================================
test_that("V0: .match_df_col / .resolve_sas_name_to_col resolve df-aware and case-insensitively", {
  df_names <- c("PAP_HIPHOP", "SEXE", "AGE_D")

  # Case-insensitive exact match
  expect_equal(.match_df_col("pap_HipHop", df_names), "PAP_HIPHOP")
  expect_true(is.na(.match_df_col("ZZZ", df_names)))

  # Trailing-"f" fallback: as-is misses, strip one "f" -> match
  expect_equal(.resolve_sas_name_to_col("SEXEf", c("SEXE", "AGE_D"), TRUE), "SEXE")

  # Truncation-safety: a real column ending in "f" is matched AS-IS, never stripped
  expect_equal(
    .resolve_sas_name_to_col("pap_tir_sptf", c("PAP_TIR_SPT", "PAP_TIR_SPTF"), TRUE),
    "PAP_TIR_SPTF"
  )
  # As-is preferred even when a stripped candidate also exists
  expect_equal(.resolve_sas_name_to_col("SEXEf", c("SEXEF"), TRUE), "SEXEF")

  # strip_f = FALSE disables the fallback
  expect_true(is.na(.resolve_sas_name_to_col("SEXEf", c("SEXE"), FALSE)))
})


# ===========================================================================
# V1: case-insensitive application (the core gap) - lowercase SAS -> upper df
# ===========================================================================
test_that("V1: apply_sas_value_labels matches lower-case SAS names to upper-case df columns", {
  f <- tempfile(fileext = ".sas")
  writeLines(v_sas_mapped, f, useBytes = TRUE)
  on.exit(unlink(f))

  result <- apply_sas_value_labels(v_df_sport(), f, quiet = TRUE)

  expect_true(inherits(result$PAP_HIPHOP, "haven_labelled"))
  expect_equal(labelled::val_labels(result$PAP_HIPHOP), c(Non = "0", Oui = "1"))
})


# ===========================================================================
# V2: trailing-"f" edge - variable ending in "f" labelled, decoy untouched
# ===========================================================================
test_that("V2: variable ending in 'f' is labelled from its double-f format; decoy stays plain", {
  f <- tempfile(fileext = ".sas")
  writeLines(v_sas_mapped, f, useBytes = TRUE)
  on.exit(unlink(f))

  result <- apply_sas_value_labels(v_df_sport(), f, quiet = TRUE)

  expect_true(inherits(result$PAP_TIR_SPTF, "haven_labelled"))
  expect_equal(labelled::val_labels(result$PAP_TIR_SPTF), c(Non = "0", Oui = "1"))
  # The decoy column (absent from the script) must NOT be truncation-matched
  expect_false(inherits(result$PAP_TIR_SPT, "haven_labelled"))
})


# ===========================================================================
# V3: no-mapping fallback - resolve via format name + strip "f"
# ===========================================================================
test_that("V3: without a mapping block, format names resolve via as-is then strip-f", {
  df <- tibble::tibble(
    SEXE   = c("h", "f", "h"),
    CL_AGE = c("20-24", "25-29", "20-24"),
    CHOMF  = c("0", "1", "0")            # variable ending in "f"; format CHOMFf
  )
  f <- tempfile(fileext = ".sas")
  writeLines(v_sas_nomap, f, useBytes = TRUE)
  on.exit(unlink(f))

  result <- apply_sas_value_labels(df, f, quiet = TRUE)

  expect_true(inherits(result$SEXE, "haven_labelled"))
  expect_true(inherits(result$CL_AGE, "haven_labelled"))
  expect_true(inherits(result$CHOMF, "haven_labelled"))   # CHOMFf -> strip -> CHOMF
  labs <- labelled::val_labels(result$SEXE)
  expect_length(labs, 2L)
  expect_equal(labs[["Homme"]], "h")
  expect_equal(labs[["Femme"]], "f")
})


# ===========================================================================
# V3b: fallback as-is beats strip - a real "...F" column is not truncated
# ===========================================================================
test_that("V3b: fallback prefers an as-is match over stripping, protecting '...F' columns", {
  df <- tibble::tibble(
    ACTIF = c("0", "1", "0"),  # real column ending in F
    ACTI  = c("1", "0", "1")   # would be the (wrong) stripped target
  )
  # No-mapping script; format named exactly like the column (no extra "f")
  sas <- '
proc format library=;

\t;value $ ACTIF
\t\t"0"="Non"
\t\t"1"="Oui"

;
run;
'
  f <- tempfile(fileext = ".sas")
  writeLines(sas, f, useBytes = TRUE)
  on.exit(unlink(f))

  result <- apply_sas_value_labels(df, f, quiet = TRUE)

  expect_true(inherits(result$ACTIF, "haven_labelled"))   # as-is match
  expect_false(inherits(result$ACTI, "haven_labelled"))   # never truncated onto ACTI
})


# ===========================================================================
# V4: values are preserved - only labels are attached
# ===========================================================================
test_that("V4: underlying stored codes are unchanged after labelling", {
  f <- tempfile(fileext = ".sas")
  writeLines(v_sas_mapped, f, useBytes = TRUE)
  on.exit(unlink(f))

  result <- apply_sas_value_labels(v_df_sport(), f, quiet = TRUE)

  raw <- result$SEXE
  attributes(raw) <- NULL              # strip class + labels + label
  expect_equal(raw, c("h", "f", "h"))  # codes, NOT "Homme"/"Femme"
})


# ===========================================================================
# V5: existing variable label preserved (value labels do not strip it)
# ===========================================================================
test_that("V5: applying value labels preserves a pre-existing variable label", {
  df <- tibble::tibble(
    SEXE = structure(c("h", "f", "h"), label = "Sexe (pre-existing)")
  )
  f <- tempfile(fileext = ".sas")
  writeLines(v_sas_mapped, f, useBytes = TRUE)
  on.exit(unlink(f))

  result <- apply_sas_value_labels(df, f, quiet = TRUE)

  expect_true(inherits(result$SEXE, "haven_labelled"))
  # Neither the value-label step nor the SAS 'label sexe=' overwrites it
  expect_equal(attr(result$SEXE, "label"), "Sexe (pre-existing)")
})


# ===========================================================================
# V6: numeric column labelled numerically; non-coercible codes skipped
# ===========================================================================
test_that("V6: numeric columns get numeric value labels; non-coercible codes are skipped", {
  f <- tempfile(fileext = ".sas")
  writeLines(v_sas_mapped, f, useBytes = TRUE)
  on.exit(unlink(f))

  result <- apply_sas_value_labels(v_df_sport(), f, quiet = TRUE)

  # AGE_D numeric, codes "20"/"30"/"50" coerce cleanly
  expect_true(inherits(result$AGE_D, "haven_labelled"))
  labs <- labelled::val_labels(result$AGE_D)
  expect_type(unclass(labs), "double")
  expect_equal(labs, c("20-29 ans" = 20, "30-39 ans" = 30, "50-59 ans" = 50))
  raw <- result$AGE_D; attributes(raw) <- NULL
  expect_equal(raw, c(20, 30, 50))

  # WEIRD numeric, codes "h"/"f"/"x" do NOT coerce -> left plain
  expect_false(inherits(result$WEIRD, "haven_labelled"))
})


# ===========================================================================
# V7: unmatched SAS names are reported in the summary message
# ===========================================================================
test_that("V7: formats/variables with no df column are reported as unmatched", {
  f <- tempfile(fileext = ".sas")
  writeLines(v_sas_mapped, f, useBytes = TRUE)
  on.exit(unlink(f))

  # FOO ($FOOf) has no df column -> unmatched
  expect_message(apply_sas_value_labels(v_df_sport(), f), "unmatched")
  expect_message(apply_sas_value_labels(v_df_sport(), f), "FOO")
})


# ===========================================================================
# V8: already-labelled columns skipped by default; replaced with overwrite = TRUE
# ===========================================================================
test_that("V8: already haven_labelled columns are skipped by default", {
  sexe_lab <- structure(
    c("h", "f", "h"),
    class  = c("haven_labelled", "vctrs_vctr", "character"),
    labels = c(Masculin = "h", "F\u00e9minin" = "f")
  )
  df <- tibble::tibble(SEXE = sexe_lab)
  f <- tempfile(fileext = ".sas")
  writeLines(v_sas_mapped, f, useBytes = TRUE)
  on.exit(unlink(f))

  result <- apply_sas_value_labels(df, f, quiet = TRUE)
  labs <- labelled::val_labels(result$SEXE)
  expect_true("Masculin" %in% names(labs))   # original kept
  expect_false("Homme" %in% names(labs))      # SAS label NOT applied
})


test_that("V8b: overwrite = TRUE replaces value labels while keeping the codes", {
  sexe_lab <- structure(
    c("h", "f", "h"),
    class  = c("haven_labelled", "vctrs_vctr", "character"),
    labels = c(Masculin = "h", "F\u00e9minin" = "f")
  )
  df <- tibble::tibble(SEXE = sexe_lab)
  f <- tempfile(fileext = ".sas")
  writeLines(v_sas_mapped, f, useBytes = TRUE)
  on.exit(unlink(f))

  result <- apply_sas_value_labels(df, f, overwrite = TRUE, quiet = TRUE)
  labs <- labelled::val_labels(result$SEXE)
  expect_length(labs, 2L)
  expect_equal(labs[["Homme"]], "h")          # replaced with SAS labels
  expect_equal(labs[["Femme"]], "f")
  expect_false("Masculin" %in% names(labs))
  raw <- result$SEXE; attributes(raw) <- NULL
  expect_equal(raw, c("h", "f", "h"))   # codes unchanged
})


# ===========================================================================
# V9: SAS variable labels applied only where the column has none
# ===========================================================================
test_that("V9: SAS 'label' descriptions fill missing labels but never overwrite existing ones", {
  df <- tibble::tibble(
    SEXE       = c("h", "f", "h"),                                   # no label -> gets SAS label
    PAP_HIPHOP = structure(c("0", "1", "0"), label = "Hip-hop (pre)") # keeps its label
  )
  f <- tempfile(fileext = ".sas")
  writeLines(v_sas_mapped, f, useBytes = TRUE)
  on.exit(unlink(f))

  result <- apply_sas_value_labels(df, f, quiet = TRUE)

  expect_equal(attr(result$SEXE, "label"), "Sexe de l'enqu\u00eat\u00e9-e")
  expect_equal(attr(result$PAP_HIPHOP, "label"), "Hip-hop (pre)")
})


# ===========================================================================
# V10: regression parity with apply_sas_labels() on the upper-case + mapping case
# ===========================================================================
test_that("V10: apply_sas_value_labels reproduces apply_sas_labels on the Emploi fixture", {
  f <- tempfile(fileext = ".sas")
  writeLines(.sas_emploi_inline, f, useBytes = TRUE)
  on.exit(unlink(f))

  result <- apply_sas_value_labels(.emploi_dummy, f, quiet = TRUE)

  # Mapped variables get value labels
  expect_true(inherits(result$METRODOM, "haven_labelled"))
  expect_true(inherits(result$AGED, "haven_labelled"))
  expect_true(inherits(result$PCS1, "haven_labelled"))
  # Unmapped variables stay plain
  expect_false(inherits(result$HCONT, "haven_labelled"))
  expect_false(inherits(result$NAIA, "haven_labelled"))
  # Pre-existing variable labels preserved
  expect_equal(
    attr(result$METRODOM, "label"),
    "R\u00e9gion du logement de r\u00e9sidence en 2 modalit\u00e9s (M\u00e9tropole vs DOM)"
  )
})
