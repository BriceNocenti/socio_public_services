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

test_that("C2b-bis: binary-battery members show positional codes in the codebook", {
  vars <- stats::setNames(lapply(1:3, function(k) list(
    var_label = paste0("Item ", k), role = "factor_binary", r_class = "double",
    new_name = paste0("B", k), n_distinct_data = 2L, battery = "Univers",
    levels = list(
      "1" = list(order = 1L, label = "Oui", new_label = paste0("Item", k),     n = 10L, pct = 30L),
      "2" = list(order = 2L, label = "Non", new_label = paste0("Pas item", k), n = 20L, pct = 70L)))),
    paste0("B", 1:3))
  jd <- .read_meta_json(cb_json(vars))
  cb <- .cb_build_tibble(jd)
  vals <- vapply(paste0("B", 1:3), function(v)
    cb$val[cb$variable == v & cb$.row_type == "value"], character(1))
  expect_equal(unname(vals), c("1-Item1", "2-Item2", "3-Item3"))
})

test_that("C2b-ter: keep_codes variable keeps original codes in the codebook", {
  vars <- list(REGION = list(
    var_label = "Region", role = "factor_nominal", r_class = "character",
    new_name = "REGION", n_distinct_data = 4L, keep_codes = TRUE,
    levels = list(
      "01" = list(order = 1L, label = "Guadeloupe", new_label = "Guadeloupe", n = 5L,  pct = 5L),
      "06" = list(order = 2L, label = "Mayotte",    new_label = "Mayotte",    n = 4L,  pct = 4L),
      "11" = list(order = 3L, label = "IDF",        new_label = "Île de France", n = 20L, pct = 20L),
      "94" = list(order = 4L, label = "Corse",      new_label = "Corse",      n = 2L,  pct = 2L))))
  jd <- .read_meta_json(cb_json(vars))
  cb <- .cb_build_tibble(jd)
  rows <- cb[cb$variable %in% "REGION" & cb$.row_type == "value", ]
  expect_equal(rows$val, c("01-Guadeloupe", "06-Mayotte", "11-Île de France", "94-Corse"))
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
  # Labelled sentinel 99 accounts for ALL the NA (12 == na_n): drop the repeated
  # count, show the label alone.
  expect_equal(unique(cb$na[cb$variable %in% "HOURS" & !is.na(cb$variable)]),
               "NA: 12 (10%) ; Ne sait pas")
})

test_that("C3f: single labelled missing level below na_n keeps its count + vide tail", {
  vars <- list(
    HOURS = list(var_label = "Heures", role = "double", r_class = "numeric",
                 new_name = "HOURS", n_distinct_data = 30L, na_n = 20L, na_pct = 16.0,
                 levels = list("99" = list(missing = TRUE, label = "Ne sait pas", n = 12L)),
                 num_stats = list(mean = 20, sd = 8, min = 0, q1 = 10,
                                  median = 18, q3 = 30, max = 60)))
  cb <- .cb_build_tibble(.read_meta_json(cb_json(vars, n_individuals = 125L)))
  # 12 labelled + (20 - 12 = 8) genuine blanks -> count kept, "vide" appended.
  expect_equal(unique(cb$na[cb$variable %in% "HOURS" & !is.na(cb$variable)]),
               "NA: 20 (16%) ; 12 Ne sait pas ; 8 vide")
})

test_that("C3g: no labelled missing level -> only 'NA: n (pct%)', no bare 'vide'", {
  vars <- list(
    CNT = list(var_label = "Compte", role = "integer_count", r_class = "numeric",
               new_name = "CNT", n_distinct_data = 40L, na_n = 811L, na_pct = 7.0,
               levels = list("999" = list(missing = TRUE, n = 1L)),
               num_stats = list(mean = 6, sd = 5, min = 1, q1 = 3,
                                median = 5, q3 = 8, max = 46)))
  cb <- .cb_build_tibble(.read_meta_json(cb_json(vars, n_individuals = 11082L)))
  expect_equal(unique(cb$na[cb$variable %in% "CNT" & !is.na(cb$variable)]), "NA: 811 (7%)")
})


# ---------------------------------------------------------------------------
# C4. Outline headers (## / ###) + battery (####) headers, from the JSON
# ---------------------------------------------------------------------------

test_that("C4: `headers` on a variable insert markdown title rows (# kept) before it", {
  vars <- cb_vars()
  vars$FREQ$headers <- list("## Bloc", "### Détail")
  jd <- .read_meta_json(cb_json(vars))
  cb <- .cb_build_tibble(jd)
  titles <- cb[cb$.row_type == "title", ]
  expect_equal(nrow(titles), 2L)
  # Markdown "#"s set the level and are KEPT in the displayed text (normalised to
  # one space) so the header hierarchy is machine-readable from the xlsx.
  expect_equal(titles$h, c("## Bloc", "### Détail"))
  expect_equal(titles$.h_level, c(2L, 3L))
  first_freq <- min(which(cb$variable %in% "FREQ"))
  expect_equal(cb$.row_type[first_freq - 1L], "title")
})

test_that("C4d: a #### group in `headers` renders a level-4 title row (not a battery)", {
  vars <- cb_vars()
  vars$FREQ$headers <- list("### Sous-theme", "#### Groupe thematique")
  jd <- .read_meta_json(cb_json(vars))
  cb <- .cb_build_tibble(jd)
  titles <- cb[cb$.row_type == "title", ]
  # The #### group is a start-marker: a level-4 title row, "#" markers kept.
  expect_equal(titles$h, c("### Sous-theme", "#### Groupe thematique"))
  expect_equal(titles$.h_level, c(3L, 4L))
  # No battery field -> no selector column populated.
  expect_true(all(is.na(cb$question_prefix)))
})

test_that("C4b: a `battery` title emits ONE #### header before the run's first member", {
  vars <- list(
    A_ONE = list(var_label = "q1", role = "factor_binary", r_class = "double", new_name = "A_ONE",
                 battery = "Batterie A",
                 levels = list("1" = list(order = 1L, label = "Oui", n = 6L, pct = 60L),
                               "0" = list(order = 2L, label = "Non", n = 4L, pct = 40L))),
    A_TWO = list(var_label = "q2", role = "factor_binary", r_class = "double", new_name = "A_TWO",
                 battery = "Batterie A",
                 levels = list("1" = list(order = 1L, label = "Oui", n = 5L, pct = 50L),
                               "0" = list(order = 2L, label = "Non", n = 5L, pct = 50L))),
    OTHER = list(var_label = "x", role = "factor_nominal", r_class = "character", new_name = "OTHER",
                 levels = list("1" = list(order = 1L, label = "a", n = 5L, pct = 50L),
                               "2" = list(order = 2L, label = "b", n = 5L, pct = 50L)))
  )
  jd <- .read_meta_json(cb_json(vars))
  cb <- .cb_build_tibble(jd)
  titles <- cb[cb$.row_type == "title", ]
  # Exactly one #### header, carrying the battery title, at level 4 (# kept).
  expect_equal(nrow(titles), 1L)
  expect_equal(titles$h, "#### Batterie A")
  expect_equal(titles$.h_level, 4L)
  # It sits immediately before A_ONE, and there is no header before A_TWO.
  first_a1 <- min(which(cb$variable %in% "A_ONE"))
  expect_equal(cb$.row_type[first_a1 - 1L], "title")
  first_a2 <- min(which(cb$variable %in% "A_TWO"))
  expect_equal(cb$.row_type[first_a2 - 1L], "value")
  # A closing spacer row detaches the standalone OTHER from the battery above it.
  first_other <- min(which(cb$variable %in% "OTHER"))
  expect_equal(cb$.row_type[first_other - 1L], "spacer")
})

test_that("C4b2: no closing spacer between two adjacent batteries (new #### separates)", {
  bin <- function(nm, batt) list(var_label = nm, role = "factor_binary", r_class = "double",
    new_name = nm, battery = batt,
    levels = list("1" = list(order = 1L, label = "Oui", n = 6L, pct = 60L),
                  "0" = list(order = 2L, label = "Non", n = 4L, pct = 40L)))
  vars <- list(A1 = bin("A1", "Bat A"), A2 = bin("A2", "Bat A"),
               B1 = bin("B1", "Bat B"), B2 = bin("B2", "Bat B"))
  jd <- .read_meta_json(cb_json(vars))
  cb <- .cb_build_tibble(jd)
  expect_false("spacer" %in% cb$.row_type)          # battery -> battery: no gap
  first_b1 <- min(which(cb$variable %in% "B1"))
  expect_equal(cb$.row_type[first_b1 - 1L], "title") # B's own #### header instead
})

test_that("C4c: a battery auto-adds a question_prefix selector (unique common prefix)", {
  bin <- function(nm) list(var_label = nm, role = "factor_binary", r_class = "double",
    new_name = nm, battery = "Batterie A",
    levels = list("1" = list(order = 1L, label = "Oui", n = 6L, pct = 60L),
                  "0" = list(order = 2L, label = "Non", n = 4L, pct = 40L)))
  vars <- list(
    PAP_A = bin("PAP_A"), PAP_B = bin("PAP_B"),
    OTHER = list(var_label = "x", role = "factor_nominal", r_class = "character",
                 new_name = "OTHER",
                 levels = list("1" = list(order = 1L, label = "a", n = 5L, pct = 50L),
                               "2" = list(order = 2L, label = "b", n = 5L, pct = 50L))))
  jd <- .read_meta_json(cb_json(vars))
  cb <- .cb_build_tibble(jd)
  qp <- unique(cb$question_prefix[!is.na(cb$question_prefix)])
  expect_equal(qp, "PAP_")                                   # prefix unique to the battery
  expect_true(all(is.na(cb$question_prefix[cb$variable %in% "OTHER"])))   # standalone: none
  # .battery marks the members (for the red rectangle), not OTHER.
  expect_true(all(cb$.battery[cb$variable %in% c("PAP_A", "PAP_B")] == "Batterie A"))
  expect_true(all(is.na(cb$.battery[cb$variable %in% "OTHER"])))
})

test_that("C4e: .battery_selector prefers a unique prefix, else pipe-joins names", {
  expect_equal(.battery_selector(c("PAP_A", "PAP_B"), c("PAP_A", "PAP_B", "OTHER")), "PAP_")
  # prefix shared by a variable OUTSIDE the battery -> fall back to pipe list
  expect_equal(.battery_selector(c("PAP_A", "PAP_B"), c("PAP_A", "PAP_B", "PAP_C")),
               "PAP_A|PAP_B")
  # no common prefix -> pipe list
  expect_equal(.battery_selector(c("Q1", "Z2"), c("Q1", "Z2", "X")), "Q1|Z2")
  # single member -> its own name
  expect_equal(.battery_selector("SOLO", c("SOLO", "OTHER")), "SOLO")
})

test_that("C4f: a non-contiguous battery title aborts the codebook with a helpful message", {
  bin <- function(nm, batt) list(var_label = nm, role = "factor_binary", r_class = "double",
    new_name = nm, battery = batt,
    levels = list("1" = list(order = 1L, label = "Oui", n = 6L, pct = 60L),
                  "0" = list(order = 2L, label = "Non", n = 4L, pct = 40L)))
  # "Bat A" is carried by A1 and A3 but split by A2 (a mistyped "Bat B") — the
  # exact shape of the pps20 hand-edit typo that triggered the openxlsx2 crash.
  vars <- list(A1 = bin("A1", "Bat A"), A2 = bin("A2", "Bat B"), A3 = bin("A3", "Bat A"))
  path <- cb_json(vars)

  # The guard sits in .cb_build_tibble(), before any xlsx write -> no openxlsx2 needed.
  err <- tryCatch(.cb_build_tibble(.read_meta_json(path)),
                  error = function(e) conditionMessage(e))
  expect_match(err, "non contigu")
  expect_match(err, "Bat A")            # the split battery
  expect_match(err, "Bat B")            # the interrupting (typo'd) sibling title
  expect_match(err, "A1")               # variable names listed
  # generate_codebook() surfaces the same abort end-to-end.
  expect_error(generate_codebook(path, output_path = tempfile(fileext = ".xlsx")),
               "non contigu")
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

  cb <- suppressMessages(generate_codebook(path, output_path = out))
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


# ---------------------------------------------------------------------------
# C8. Regression: a level with no `n` must not coerce `new_label` (R `$` partial
#     matching once made lv$n resolve to lv$new_label -> "NAs introduced by
#     coercion"). Now [["n"]] is exact -> NULL -> blank n, no warning.
# ---------------------------------------------------------------------------

test_that("C8: factor level without `n` builds without warning and shows blank n", {
  vars <- list(
    DEC = list(var_label = "Décile", role = "factor_ordinal", r_class = "character",
               new_name = "DEC",
               levels = list(
                 "01" = list(order = 1L, label = "d1", new_label = "1er décile"),
                 "02" = list(order = 2L, label = "d2", new_label = "2e décile")))
  )
  jd <- .read_meta_json(cb_json(vars))
  expect_no_warning(cb <- .cb_build_tibble(jd))
  expect_true(all(is.na(cb$n[cb$variable %in% "DEC" & !is.na(cb$variable)])))
})


# ---------------------------------------------------------------------------
# C9. Markdown -> rich text tokenising (front-matter cell)
# ---------------------------------------------------------------------------

test_that("C9: .md_tokens splits **bold** / *italic* / plain runs", {
  tk <- .md_tokens("a **b** c *d*")
  expect_equal(vapply(tk, `[[`, "", "text"),        c("a ", "b", " c ", "d"))
  expect_equal(vapply(tk, `[[`, logical(1), "bold"),   c(FALSE, TRUE, FALSE, FALSE))
  expect_equal(vapply(tk, `[[`, logical(1), "italic"), c(FALSE, FALSE, FALSE, TRUE))
  plain <- .md_tokens("nothing here")
  expect_equal(length(plain), 1L)
  expect_false(plain[[1]]$bold || plain[[1]]$italic)
})

test_that("C9b: .md_to_fmt_txt returns a fmt_txt object", {
  skip_if_not_installed("openxlsx2")
  ft <- .md_to_fmt_txt("**Champ :** x")
  expect_true(!is.null(ft))
  expect_no_error(openxlsx2::fmt_txt(ft))
})


# ---------------------------------------------------------------------------
# C10. Survey front-matter: level-1 title + one metadata row
# ---------------------------------------------------------------------------

test_that("C10: survey_* build a level-1 title + ONE frontmatter row per field", {
  ml <- make_meta_list(cb_vars())
  ml$config$n_individuals      <- 1000L
  ml$config$survey_title       <- "Mon enquête"
  ml$config$survey_description  <- "Une **enquête** de test"
  ml$config$survey_population   <- "Les gens"
  path <- tmp_json(); .write_meta_json(ml, path)
  cb <- .cb_build_tibble(.read_meta_json(path))

  t1 <- cb[cb$.row_type %in% "title" & cb$.h_level %in% 1L, ]
  expect_equal(nrow(t1), 1L)
  expect_match(t1$h, "Dictionnaire des codes")
  expect_match(t1$h, "Mon enquête")

  fm <- cb[cb$.row_type %in% "frontmatter", ]
  expect_equal(nrow(fm), 2L)                                       # description + population
  expect_true(any(grepl("enquête", fm$description)))              # survey_description row
  pop <- fm[grepl("Champ", fm$description), ]
  expect_equal(nrow(pop), 1L)
  expect_true(grepl("Les gens", pop$description))
  expect_equal(pop$n, 1000)                                        # n_individuals on the Champ row
  expect_true(all(is.na(fm$n[!grepl("Champ", fm$description)])))   # other rows: no n
})

test_that("C10b: no survey_title/description -> no title/frontmatter rows", {
  jd <- .read_meta_json(cb_json(cb_vars()))
  cb <- .cb_build_tibble(jd)
  expect_equal(nrow(cb[cb$.row_type %in% "frontmatter", ]), 0L)
  expect_equal(nrow(cb[cb$.row_type %in% "title" & cb$.h_level %in% 1L, ]), 0L)
})


# ---------------------------------------------------------------------------
# C11. Visual layer (2026-07): battery rose fill, role chips, freq right border
#      on non-battery blocks, data bars in factor freq, hyperlinked TOC.
# ---------------------------------------------------------------------------

test_that("C11: xlsx cell fills/borders + data bars + TOC map to the right cells", {
  skip_if_not_installed("openxlsx2")
  lv <- function(o, lab, n, p) list(order = o, label = lab, n = n, pct = p)
  vars <- list(
    FRUIT = list(var_label = "Fruit", role = "factor_nominal", r_class = "character",
                 new_name = "FRUIT", headers = list("## Bloc A"),
                 levels = list("1" = lv(1, "Pomme", 6, 60), "2" = lv(2, "Poire", 4, 40))),
    B1 = list(var_label = "aime x", role = "factor_binary", r_class = "double", new_name = "B1",
              battery = "Bat B",
              levels = list("1" = lv(1, "Oui", 6, 60), "0" = lv(2, "Non", 4, 40))),
    B2 = list(var_label = "aime y", role = "factor_binary", r_class = "double", new_name = "B2",
              battery = "Bat B",
              levels = list("1" = lv(1, "Oui", 5, 50), "0" = lv(2, "Non", 5, 50)))
  )
  path <- tmp_json(); .write_meta_json(list(config = list(), variables = vars), path)
  out  <- tempfile(fileext = ".xlsx")
  on.exit(unlink(c(path, out)), add = TRUE)
  suppressMessages(generate_codebook(path, output_path = out))

  wb <- openxlsx2::wb_load(out)
  sm <- wb$styles_mgr$styles
  a_of <- function(xml, nm) {
    m <- regmatches(xml, regexpr(paste0(nm, '="[^"]*"'), xml))
    if (!length(m)) NA_character_ else sub(paste0(nm, '="([^"]*)"'), "\\1", m)
  }
  fill_hex <- function(cell) {
    xf  <- sm$cellXfs[[as.integer(openxlsx2::wb_get_cell_style(wb, 1, cell)) + 1L]]
    fid <- suppressWarnings(as.integer(a_of(xf, "fillId"))); if (is.na(fid)) return(NA_character_)
    fx  <- sm$fills[[fid + 1L]]
    m   <- regmatches(fx, regexpr('fgColor[^/]*rgb="[0-9A-Fa-f]{8}"', fx))
    if (!length(m)) NA_character_ else toupper(sub('.*rgb="([0-9A-Fa-f]{8})".*', "\\1", m))
  }
  right_border <- function(cell) {
    xf  <- sm$cellXfs[[as.integer(openxlsx2::wb_get_cell_style(wb, 1, cell)) + 1L]]
    bid <- suppressWarnings(as.integer(a_of(xf, "borderId"))); if (is.na(bid)) return(FALSE)
    grepl("<right[^>]*style=", sm$borders[[bid + 1L]])
  }
  d <- openxlsx2::wb_to_df(wb, sheet = 1, col_names = FALSE, skip_empty_rows = FALSE,
                           na.strings = NULL)
  rF <- which(d[[7]] == "1-Pomme")[1]                 # FRUIT val row (col G = 7)
  rB <- which(d[[2]] == "B1")[1]                       # B1 row (col B = 2)

  # Battery B1: valeur|n|freq (G,H,I) are rose; role (E) is the binary chip, not rose.
  expect_equal(fill_hex(paste0("G", rB)), "FFFDE9ED")
  expect_equal(fill_hex(paste0("H", rB)), "FFFDE9ED")
  expect_equal(fill_hex(paste0("I", rB)), "FFFDE9ED")
  expect_equal(fill_hex(paste0("E", rB)), "FFDCE6F1")
  # Non-battery FRUIT: role chip (nominal green), val not rose, freq has a right border.
  expect_equal(fill_hex(paste0("E", rF)), "FFE2EFDA")
  expect_false(identical(fill_hex(paste0("G", rF)), "FFFDE9ED"))
  expect_true(right_border(paste0("I", rF)))

  # Data bars over the factor freq column, and one internal TOC hyperlink (## Bloc A).
  expect_true(any(grepl("dataBar", unlist(wb$worksheets[[1]]$conditionalFormatting))))
  expect_gte(length(unlist(wb$worksheets[[1]]$hyperlinks)), 1L)
})
