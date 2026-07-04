# Tests for generate_codebook() and its internals.
# Functions under test: .cb_type_label, .cb_role_label, .cb_build_tibble,
#   .cb_write_xlsx, generate_codebook
# Prefix: C

# Helper: write a JSON from vars (+ optional n_individuals) and load json_data.
cb_json <- function(vars, n_individuals = 1000L) {
  ml <- make_meta_list(vars)
  ml$config$n_individuals <- n_individuals
  path <- tmp_json()
  .write_meta_json(ml, path)
  path
}

# A representative set of variables covering every role.
cb_vars <- function() list(
  IDENT = list(var_label = "Identifiant", role = "identifier", r_class = "character",
               new_name = "IDENT", n_distinct_data = 1000L, levels = list()),
  DECL = list(var_label = "Déclare", role = "factor_binary", r_class = "double",
              new_name = "DECL", n_distinct_data = 2L,
              levels = list(
                "1" = list(order = 1L, label = "Oui", new_label = "Déclare", n = 780L, pct = 78L),
                "0" = list(order = 2L, label = "Non", new_label = "Ne déclare pas", n = 220L, pct = 22L))),
  FREQ = list(var_label = "Fréquence", role = "factor_ordinal", r_class = "double",
              new_name = "FREQ", n_distinct_data = 3L,
              levels = list(
                "1" = list(order = 3L, label = "Rarement", new_label = "Rare",    n = 200L, pct = 22L),
                "2" = list(order = 2L, label = "Parfois",  new_label = "Parfois", n = 300L, pct = 33L),
                "3" = list(order = 1L, label = "Souvent",  new_label = "Souvent", n = 400L, pct = 44L),
                "9" = list(missing = TRUE, label = "NSP"))),
  NBAPS = list(var_label = "Nombre d'APS", role = "integer_count", r_class = "numeric",
               new_name = "NBAPS", n_distinct_data = 20L, levels = list(),
               na_n = 30L, na_pct = 3.0,
               num_stats = list(min = 0, max = 46, mean = 6.04, sd = 4.95,
                                q1 = 3, median = 5, q3 = 8)),
  NOTE = list(var_label = "Commentaire", role = "other", r_class = "character",
              new_name = "NOTE", n_distinct_data = 5L, levels = list(),
              examples = c("a", "b", "c", "d", "e"))
)


# ---------------------------------------------------------------------------
# C1. Type / role label maps
# ---------------------------------------------------------------------------

test_that("C1: type labels derive R class from role (+ r_class fallback)", {
  expect_equal(.cb_type_label("factor_ordinal", "double", "fr"), "catégorielle")
  expect_equal(.cb_type_label("integer_count", "numeric", "fr"), "nb entier")
  expect_equal(.cb_type_label("double", "numeric", "fr"), "nb décimal")
  expect_equal(.cb_type_label("identifier", "character", "fr"), "texte")
  expect_equal(.cb_type_label("factor_binary", "double", "en"), "factor")
})

test_that("C1b: role labels translate, integer_count -> comptage / count", {
  expect_equal(.cb_role_label("integer_count", "fr"), "comptage")
  expect_equal(.cb_role_label("integer_scale", "fr"), "échelle")
  expect_equal(.cb_role_label("double", "fr"), "continue")
  expect_equal(.cb_role_label("factor_ordinal", "fr"), "ordinale")
  expect_equal(.cb_role_label("integer_count", "en"), "count")
})


# ---------------------------------------------------------------------------
# C2. Tibble shape per role
# ---------------------------------------------------------------------------

test_that("C2: binary = 1 row (positive level), val == .gfs_level_label", {
  jd <- .read_meta_json(cb_json(cb_vars()))
  cb <- .cb_build_tibble(jd)
  rows <- cb[cb$variable %in% "DECL" & cb$.row_type == "value", ]
  expect_equal(nrow(rows), 1L)
  expect_equal(rows$val, "1-Déclare")
  expect_equal(rows$n, 780)
  expect_equal(rows$pct, 78)
  expect_true(rows$.is_binary)
  expect_equal(rows$orig_val, "Oui / Non")   # both original labels for binaries
})

test_that("C2b: nominal/ordinal = one row per level, ascending order", {
  jd <- .read_meta_json(cb_json(cb_vars()))
  cb <- .cb_build_tibble(jd)
  rows <- cb[cb$variable %in% "FREQ" & cb$.row_type == "value", ]
  expect_equal(nrow(rows), 3L)
  expect_equal(rows$val, c("1-Souvent", "2-Parfois", "3-Rare"))
})

test_that("C2c: numeric = 6 stat rows, mean row carries sd + rule", {
  jd <- .read_meta_json(cb_json(cb_vars()))
  cb <- .cb_build_tibble(jd)
  rows <- cb[cb$variable %in% "NBAPS" & cb$.row_type == "value", ]
  expect_equal(nrow(rows), 6L)
  # mean + sd FIRST, then max, Q3, median, Q1, min
  expect_equal(rows$val, c("moyenne + écart-type", "max", "Q3", "médiane", "Q1", "min"))
  expect_equal(rows$n, c(6.04, 46, 8, 5, 3, 0))
  expect_equal(rows$pct[1], 4.95)          # sd on the mean row (now first)
  expect_true(rows$.stat_rule[1])
  expect_true(all(!rows$.stat_rule[2:6]))
})

test_that("C2d: text/other = 1 row, 'Ex. : ' + 4 quoted example values", {
  jd <- .read_meta_json(cb_json(cb_vars()))
  cb <- .cb_build_tibble(jd)
  rows <- cb[cb$variable %in% "NOTE" & cb$.row_type == "value", ]
  expect_equal(nrow(rows), 1L)
  expect_match(rows$val, 'Ex. : "a", "b", "c", "d"', fixed = TRUE)
})

test_that("C2e: keep_original shows raw labels (no prefix), sorted by code", {
  jd <- .read_meta_json(cb_json(cb_vars()))
  cb <- .cb_build_tibble(jd, natural_order = TRUE)
  rows <- cb[cb$variable %in% "FREQ" & cb$.row_type == "value", ]
  # codes 1,2,3 -> labels as stored, no "1-" ordering prefix
  expect_equal(rows$val, c("Rare", "Parfois", "Souvent"))
})


# ---------------------------------------------------------------------------
# C3. Missing-value cell
# ---------------------------------------------------------------------------

test_that("C3: factor NA = n_individuals - sum(level n), lists missing labels", {
  jd <- .read_meta_json(cb_json(cb_vars(), n_individuals = 1000L))
  cb <- .cb_build_tibble(jd)
  # FREQ: 400 + 300 + 200 = 900 valid -> 100 missing (10%); "9"=NSP recoded to NA
  na_val <- unique(cb$na[cb$variable %in% "FREQ" & !is.na(cb$variable)])
  expect_equal(na_val, "NA: 100 (10%) ; NSP")
  # DECL: 780 + 220 = 1000 -> 0 missing, no missing level -> no trailing labels
  expect_equal(unique(cb$na[cb$variable %in% "DECL" & !is.na(cb$variable)]), "NA: 0 (0%)")
})

test_that("C3b: numeric NA read from stored top-level na_n / na_pct", {
  jd <- .read_meta_json(cb_json(cb_vars()))
  cb <- .cb_build_tibble(jd)
  expect_equal(unique(cb$na[cb$variable %in% "NBAPS" & !is.na(cb$variable)]), "NA: 30 (3%)")
})

test_that("C3c: factor NA blank when n_individuals absent", {
  jd <- .read_meta_json(cb_json(cb_vars(), n_individuals = NA))
  cb <- .cb_build_tibble(jd)
  expect_equal(unique(cb$na[cb$variable %in% "FREQ" & !is.na(cb$variable)]), "")
})

test_that("C3d: missing levels with counts render 'NA: n (pct%) ; <n> <label> ; <blank> vide'", {
  vars <- list(
    Q = list(var_label = "Question", role = "factor_binary", r_class = "double",
             new_name = "Q", n_distinct_data = 2L, na_n = 1481L, na_pct = 13.0,
             levels = list(
               "1" = list(order = 1L, label = "Oui", new_label = "Oui", n = 5000L, pct = 50L),
               "0" = list(order = 2L, label = "Non", new_label = "Non", n = 5000L, pct = 50L),
               "8" = list(missing = TRUE, label = "Non concerné", n = 900L),
               "9" = list(missing = TRUE, label = "Non-réponse",  n = 500L))))
  cb <- .cb_build_tibble(.read_meta_json(cb_json(vars, n_individuals = 11481L)))
  # biggest coded missing first; genuine blanks (1481 - 900 - 500 = 81) last as "vide"
  expect_equal(unique(cb$na[cb$variable %in% "Q" & !is.na(cb$variable)]),
               "NA: 1481 (13%) ; 900 Non concerné ; 500 Non-réponse ; 81 vide")
})

test_that("C3e: numeric sentinels: unlabelled fold into the total, labelled ones are listed", {
  vars <- list(
    BRIC = list(var_label = "Bricolage", role = "double", r_class = "numeric",
                new_name = "BRIC", n_distinct_data = 40L, na_n = 25L, na_pct = 20.0,
                levels = list("999" = list(missing = TRUE, n = 25L)),
                num_stats = list(mean = 6.2, sd = 4.9, min = 0, q1 = 1,
                                 median = 5, q3 = 12, max = 40)),
    HOURS = list(var_label = "Heures", role = "double", r_class = "numeric",
                 new_name = "HOURS", n_distinct_data = 30L, na_n = 12L, na_pct = 10.0,
                 levels = list("99" = list(missing = TRUE, label = "Ne sait pas", n = 12L)),
                 num_stats = list(mean = 20, sd = 8, min = 0, q1 = 10,
                                  median = 18, q3 = 30, max = 60)))
  cb <- .cb_build_tibble(.read_meta_json(cb_json(vars, n_individuals = 125L)))
  # Unlabelled sentinel 999: collapses into the overall NA total (no bare code).
  expect_equal(unique(cb$na[cb$variable %in% "BRIC" & !is.na(cb$variable)]),
               "NA: 25 (20%)")
  # Labelled sentinel 99: numeric vars now list labelled missing levels like factors.
  expect_equal(unique(cb$na[cb$variable %in% "HOURS" & !is.na(cb$variable)]),
               "NA: 12 (10%) ; 12 Ne sait pas")
})


# ---------------------------------------------------------------------------
# C4. Section titles + battery spacers
# ---------------------------------------------------------------------------

test_that("C4: titles insert markdown title rows before the target variable", {
  jd <- .read_meta_json(cb_json(cb_vars()))
  cb <- .cb_build_tibble(jd, titles = c("## Bloc" = "FREQ", "### Détail" = "FREQ"))
  titles <- cb[cb$.row_type == "title", ]
  expect_equal(nrow(titles), 2L)
  expect_equal(titles$h, c("## Bloc", "### Détail"))
  expect_equal(titles$.h_level, c(2L, 3L))
  # Title rows appear immediately before the FREQ block.
  first_freq <- min(which(cb$variable %in% "FREQ"))
  expect_equal(cb$.row_type[first_freq - 1L], "title")
})

test_that("C4b: binary battery prefixes get a spacer around the run", {
  vars <- list(
    A_ONE = list(var_label = "q1", role = "factor_binary", r_class = "double", new_name = "A_ONE",
                 levels = list("1" = list(order = 1L, label = "Oui", n = 6L, pct = 60L),
                               "0" = list(order = 2L, label = "Non", n = 4L, pct = 40L))),
    A_TWO = list(var_label = "q2", role = "factor_binary", r_class = "double", new_name = "A_TWO",
                 levels = list("1" = list(order = 1L, label = "Oui", n = 5L, pct = 50L),
                               "0" = list(order = 2L, label = "Non", n = 5L, pct = 50L))),
    OTHER = list(var_label = "x", role = "factor_nominal", r_class = "character", new_name = "OTHER",
                 levels = list("1" = list(order = 1L, label = "a", n = 5L, pct = 50L),
                               "2" = list(order = 2L, label = "b", n = 5L, pct = 50L)))
  )
  jd <- .read_meta_json(cb_json(vars))
  cb <- .cb_build_tibble(jd, binary_batteries = c("A_"))
  expect_true(any(cb$.row_type == "spacer"))
  # A spacer separates the A_ battery from OTHER.
  first_other <- min(which(cb$variable %in% "OTHER"))
  expect_equal(cb$.row_type[first_other - 1L], "spacer")
})


# ---------------------------------------------------------------------------
# C5. orig_val column dropped when no new labels differ
# ---------------------------------------------------------------------------

test_that("C5: any_new_label FALSE when new_label == label everywhere", {
  vars <- list(
    NOM = list(var_label = "x", role = "factor_nominal", r_class = "character", new_name = "NOM",
               levels = list(
                 "1" = list(order = 1L, label = "Alpha", new_label = "Alpha", n = 5L, pct = 50L),
                 "2" = list(order = 2L, label = "Beta",  new_label = "Beta",  n = 5L, pct = 50L)))
  )
  jd <- .read_meta_json(cb_json(vars))
  cb <- .cb_build_tibble(jd)
  expect_false(isTRUE(attr(cb, "any_new_label")))
})

test_that("C5b: any_new_label TRUE when a new_label differs", {
  jd <- .read_meta_json(cb_json(cb_vars()))
  cb <- .cb_build_tibble(jd)
  expect_true(isTRUE(attr(cb, "any_new_label")))
})


# ---------------------------------------------------------------------------
# C6. End-to-end write (skips if openxlsx2 absent)
# ---------------------------------------------------------------------------

test_that("C6: generate_codebook writes a readable .xlsx and returns a tibble", {
  skip_if_not_installed("openxlsx2")
  path <- cb_json(cb_vars())
  out  <- tempfile(fileext = ".xlsx")
  on.exit(unlink(c(path, out)), add = TRUE)

  cb <- suppressMessages(generate_codebook(path, output_path = out,
          titles = c("## Bloc" = "FREQ"), binary_batteries = c("DECL")))
  expect_true(file.exists(out))
  expect_s3_class(cb, "tbl_df")
  expect_false(any(grepl("^\\.", names(cb))))     # internal cols dropped
  # File re-reads without error and has the header row.
  back <- openxlsx2::wb_to_df(openxlsx2::wb_load(out), col_names = FALSE)
  expect_true(nrow(back) > 1)
})


# ---------------------------------------------------------------------------
# C7. df-first mode: build a codebook straight from a data frame (no AI)
# ---------------------------------------------------------------------------

test_that("C7: generate_codebook(df) builds a temp JSON + xlsx silently", {
  skip_if_not_installed("openxlsx2")
  out <- tempfile(fileext = ".xlsx")
  on.exit(unlink(out), add = TRUE)

  cb <- suppressMessages(generate_codebook(.virage_dummy, output_path = out))
  expect_true(file.exists(out))
  expect_s3_class(cb, "tbl_df")
  expect_true(nrow(cb) > 0)
  expect_false(any(grepl("^\\.", names(cb))))     # internal cols dropped
})
