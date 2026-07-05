# Tests for add_new_variables_to_codebook_from_df() and its internals.
# Functions under test: .cb_final_names, .cb_assert_shared_order,
#   .cb_check_new_var_labels, .cb_new_vars_json, .cb_inject_new_batteries,
#   .cb_segment_by_variable, .cb_new_positioning, .cb_reassemble,
#   .cb_assert_battery_contiguity_rows, add_new_variables_to_codebook_from_df
# Prefix: NV

# --- Fixtures ---------------------------------------------------------------

# Meta JSON for the ORIGINAL variables (3 vars, meta order = df order).
# AGE_ORIG carries a changed label (new_label != label) so the orig_val column
# is kept -> we can assert new-var rows are blank there.
nv_meta_vars <- function() list(
  AGE_ORIG = list(var_label = "Âge", role = "factor_ordinal", r_class = "character",
                  new_name = "AGE_ORIG", n_distinct_data = 3L, na_n = 1L, na_pct = 10,
                  levels = list(
                    "1" = list(order = 1L, label = "Jeune", new_label = "Jeune*", n = 4L, pct = 44L),
                    "2" = list(order = 2L, label = "Moyen", new_label = "Moyen*", n = 3L, pct = 33L),
                    "3" = list(order = 3L, label = "Vieux", new_label = "Vieux*", n = 2L, pct = 22L))),
  SEX_ORIG = list(var_label = "Sexe", role = "factor_binary", r_class = "character",
                  new_name = "SEX_ORIG", n_distinct_data = 2L, na_n = 0L, na_pct = 0,
                  levels = list(
                    "1" = list(order = 1L, label = "Homme", n = 5L, pct = 50L),
                    "2" = list(order = 2L, label = "Femme", n = 5L, pct = 50L))),
  REG_ORIG = list(var_label = "Région", role = "factor_nominal", r_class = "character",
                  new_name = "REG_ORIG", n_distinct_data = 2L, na_n = 0L, na_pct = 0,
                  levels = list(
                    "1" = list(order = 1L, label = "Nord", n = 5L, pct = 50L),
                    "2" = list(order = 2L, label = "Sud",  n = 5L, pct = 50L)))
)

nv_json <- function(n_individuals = 10L) {
  ml <- make_meta_list(nv_meta_vars())
  ml$config$n_individuals <- n_individuals
  path <- tmp_json()
  .write_meta_json(ml, path)
  path
}

# Final df: originals (dummy columns) + AGE_NEW (inline, ordered->ordinal) +
# SCORE_NEW (trailing, numeric).
nv_df <- function() {
  d <- data.frame(
    AGE_ORIG  = factor(rep(c("a", "b"), 5)),
    AGE_NEW   = factor(c("1-Jeune", "2-Moyen", "3-Vieux", "1-Jeune", "2-Moyen",
                         "3-Vieux", "1-Jeune", "2-Moyen", "3-Vieux", "2-Moyen"),
                       levels = c("1-Jeune", "2-Moyen", "3-Vieux"), ordered = TRUE),
    SEX_ORIG  = factor(rep(c("H", "F"), 5)),
    REG_ORIG  = factor(rep(c("N", "S"), 5)),
    SCORE_NEW = as.numeric(rep(1:5, 2)),
    stringsAsFactors = FALSE)
  attr(d$AGE_NEW,   "label") <- "Âge recodé en 3 classes"
  attr(d$SCORE_NEW, "label") <- "Score synthétique"
  d
}


# ---------------------------------------------------------------------------
# NV1. Positioning + segmentation internals (no openxlsx2 needed)
# ---------------------------------------------------------------------------

test_that("NV1: .cb_new_positioning splits inline vs trailing by df order", {
  final_of <- stats::setNames(c("AGE_ORIG", "SEX_ORIG", "REG_ORIG"),
                              c("AGE_ORIG", "SEX_ORIG", "REG_ORIG"))
  batt     <- stats::setNames(c("", "", ""), names(final_of))
  df_names <- c("AGE_ORIG", "AGE_NEW", "SEX_ORIG", "REG_ORIG", "SCORE_NEW")
  pos <- .cb_new_positioning(df_names, final_of, batt)
  expect_equal(pos$inline[["AGE_ORIG"]], "AGE_NEW")
  expect_equal(pos$trailing, "SCORE_NEW")
  expect_null(pos$inline[["SEX_ORIG"]])
})

test_that("NV1b: .cb_new_positioning bumps a battery anchor to its last member", {
  final_of <- stats::setNames(c("Q1", "Q2", "Q3"), c("Q1", "Q2", "Q3"))
  batt     <- stats::setNames(c("Bat", "Bat", ""), c("Q1", "Q2", "Q3"))
  # NEW anchored to Q1 (a non-last battery member) must bump to Q2 (last member).
  pos <- .cb_new_positioning(c("Q1", "NEW", "Q2", "Q3"), final_of, batt)
  expect_null(pos$inline[["Q1"]])
  expect_equal(pos$inline[["Q2"]], "NEW")
})

test_that("NV1c: .cb_segment_by_variable keys blocks by .orig_name in order", {
  cb  <- .cb_build_tibble(.read_meta_json(nv_json()))
  seg <- .cb_segment_by_variable(cb)
  expect_equal(seg$order, c("AGE_ORIG", "SEX_ORIG", "REG_ORIG"))
  expect_length(seg$front, 0L)                       # no survey_title / front-matter
  expect_true(all(cb$.orig_name[seg$segs[["AGE_ORIG"]]] == "AGE_ORIG"))
})


# ---------------------------------------------------------------------------
# NV2. Validations (no openxlsx2 needed)
# ---------------------------------------------------------------------------

test_that("NV2: reordered original variables abort with a clear message", {
  path <- nv_json()
  df2  <- nv_df()[, c("SEX_ORIG", "AGE_ORIG", "AGE_NEW", "REG_ORIG", "SCORE_NEW")]
  expect_error(
    add_new_variables_to_codebook_from_df(path, df2, output_path = tempfile(fileext = ".xlsx")),
    "ordre")
})

test_that("NV2b: new var without a label is flagged", {
  jv <- .read_meta_json(nv_json())$variables
  df <- nv_df(); attr(df$SCORE_NEW, "label") <- NULL
  expect_message(.cb_check_new_var_labels(df, c("AGE_NEW", "SCORE_NEW"), jv),
                 "sans label")
})

test_that("NV2c: new var label duplicating another variable's is flagged", {
  jv <- .read_meta_json(nv_json())$variables
  df <- nv_df(); attr(df$SCORE_NEW, "label") <- "Âge"   # == AGE_ORIG var_label
  expect_message(.cb_check_new_var_labels(df, c("AGE_NEW", "SCORE_NEW"), jv),
                 "déjà utilisé")
})


# ---------------------------------------------------------------------------
# NV3. New-var extraction + battery injection (no openxlsx2 needed)
# ---------------------------------------------------------------------------

test_that("NV3: an R ordered factor new var is detected factor_ordinal", {
  nj <- .cb_new_vars_json(nv_df()["AGE_NEW"], list())
  expect_equal(nj$variables$AGE_NEW$role, "factor_ordinal")
})

test_that("NV3b: question_prefix attr becomes a new-var battery title", {
  new_df <- data.frame(P_A = factor(rep(c("Oui", "Non"), 5)),
                       P_B = factor(rep(c("Oui", "Non"), 5)),
                       stringsAsFactors = FALSE)
  attr(new_df$P_A, "label") <- "Pratique A"
  attr(new_df$P_B, "label") <- "Pratique B"
  attr(new_df$P_A, "question_prefix") <- "Pratiques"
  attr(new_df$P_B, "question_prefix") <- "Pratiques"
  nj <- .cb_inject_new_batteries(.cb_new_vars_json(new_df, list()), new_df)
  expect_equal(nj$variables$P_A$battery, "Pratiques")
  tn <- .cb_build_tibble(nj, natural_order = TRUE)
  expect_true(any(tn$.battery %in% "Pratiques"))
})


# ---------------------------------------------------------------------------
# NV4. Full function end-to-end (writes xlsx -> needs openxlsx2)
# ---------------------------------------------------------------------------

test_that("NV4: inline + trailing new vars placed correctly, xlsx written", {
  skip_if_not_installed("openxlsx2")
  path <- nv_json()
  out  <- tempfile(fileext = ".xlsx")
  res  <- suppressMessages(
    add_new_variables_to_codebook_from_df(path, nv_df(), output_path = out))

  expect_true(file.exists(out))
  expect_s3_class(res, "tbl_df")
  expect_false(any(grepl("^\\.", names(res))))       # internal dot-cols dropped

  # AGE_NEW: ordinal, placed between AGE_ORIG and SEX_ORIG
  expect_true("AGE_NEW" %in% res$variable)
  expect_equal(unique(res$role[res$variable %in% "AGE_NEW"]), "ordinale")
  an <- min(which(res$variable == "AGE_NEW"))
  expect_true(min(which(res$variable == "AGE_ORIG")) < an)
  expect_true(an < min(which(res$variable == "SEX_ORIG")))

  # SCORE_NEW: trailing, under the "Nouvelles variables" section
  nvt <- which(res$h == "Nouvelles variables")
  expect_length(nvt, 1L)
  expect_true(all(which(res$variable == "SCORE_NEW") > nvt))

  # new vars carry no original label/code
  new_rows <- res$variable %in% c("AGE_NEW", "SCORE_NEW")
  expect_true(all(is.na(res$orig_code[new_rows])))
  expect_true(all(is.na(res$orig_val[new_rows])))    # orig_val kept (AGE_ORIG relabelled)
})

test_that("NV4b: no new variables -> standard codebook, still written", {
  skip_if_not_installed("openxlsx2")
  path <- nv_json()
  df   <- nv_df()[, c("AGE_ORIG", "SEX_ORIG", "REG_ORIG")]   # originals only
  out  <- tempfile(fileext = ".xlsx")
  res  <- suppressMessages(
    add_new_variables_to_codebook_from_df(path, df, output_path = out))
  expect_true(file.exists(out))
  expect_false("Nouvelles variables" %in% res$h)
  expect_setequal(unique(stats::na.omit(res$variable)),
                  c("AGE_ORIG", "SEX_ORIG", "REG_ORIG"))
})
