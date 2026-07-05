# Tests for generate_format_script() and its internal helpers.
# Functions under test: .gfs_numeric_prefix, .gfs_compute_numeric_stats,
#   .gfs_build_entries, .gfs_level_label, .gfs_num_stats_comment,
#   .gfs_format_blocks, generate_format_script


# ---------------------------------------------------------------------------
# A. .gfs_numeric_prefix
# ---------------------------------------------------------------------------

test_that("numeric prefix: 1-9 levels use no leading zero", {
  expect_equal(.gfs_numeric_prefix(1, 5), "1-")
  expect_equal(.gfs_numeric_prefix(5, 9), "5-")
  expect_equal(.gfs_numeric_prefix(9, 9), "9-")
})

test_that("numeric prefix: 10-99 levels use leading zero", {
  expect_equal(.gfs_numeric_prefix(1, 12), "01-")
  expect_equal(.gfs_numeric_prefix(12, 12), "12-")
  expect_equal(.gfs_numeric_prefix(3, 44), "03-")
  expect_equal(.gfs_numeric_prefix(44, 44), "44-")
})

test_that("numeric prefix: 100+ levels use two leading zeros", {
  expect_equal(.gfs_numeric_prefix(1, 150), "001-")
  expect_equal(.gfs_numeric_prefix(99, 150), "099-")
  expect_equal(.gfs_numeric_prefix(150, 150), "150-")
})


# ---------------------------------------------------------------------------
# B. .gfs_compute_numeric_stats
# ---------------------------------------------------------------------------

test_that("numeric stats: basic computation", {
  col <- c(10, 20, 30, 40, 50, 99)
  st <- .gfs_compute_numeric_stats(col, missing_codes = "99")
  expect_equal(st$min, 10)
  expect_equal(st$max, 50)
  expect_equal(st$mean, 30)
  expect_equal(st$median, 30)
  expect_false(is.null(st$sd))
  expect_false(is.null(st$q1))
  expect_false(is.null(st$q3))
})

test_that("numeric stats: na_n / na_pct count NA + missing-coded values", {
  col <- c(10, 20, 30, 40, 50, 99, NA)   # 99 = missing code, plus one NA
  st <- .gfs_compute_numeric_stats(col, missing_codes = "99")
  expect_equal(st$na_n, 2L)                       # the 99 and the NA
  expect_equal(round(st$na_pct, 2), round(2 / 7 * 100, 2))
})

test_that("numeric stats: all missing returns NULL", {
  col <- c(99, 99, 99)
  st <- .gfs_compute_numeric_stats(col, missing_codes = "99")
  expect_null(st)
})

test_that("numeric stats: character column handled", {
  col <- c("10", "20", "30", "88")
  st <- .gfs_compute_numeric_stats(col, missing_codes = "88")
  expect_equal(st$min, 10)
  expect_equal(st$max, 30)
})


# ---------------------------------------------------------------------------
# C. .gfs_build_entries
# ---------------------------------------------------------------------------

test_that("build_entries: ordinal with missing, sorted by order", {
  vars <- list(
    Q1 = list(
      var_label = "Question one",
      role      = "factor_ordinal",
      new_name  = "Q1_NEW",
      levels = list(
        "01" = list(order = 3L, label = "Low",  new_label = "Bas",  n = 100L, pct = 20L),
        "02" = list(order = 1L, label = "High", new_label = "Haut", n = 300L, pct = 60L),
        "03" = list(order = 2L, label = "Mid",  new_label = "Moy",  n = 100L, pct = 20L),
        "99" = list(missing = TRUE, label = "NSP")
      )
    )
  )
  entries <- .gfs_build_entries(vars)

  expect_length(entries, 1)
  e <- entries[[1]]
  expect_equal(e$orig_name, "Q1")
  expect_equal(e$new_name, "Q1_NEW")
  expect_equal(e$role, "factor_ordinal")
  expect_equal(e$n_non_missing, 3L)
  expect_equal(e$max_order, 3L)

  # Sorted by order: 1=High, 2=Mid, 3=Low
  expect_equal(e$levels_sorted[[1]]$order, 1L)
  expect_equal(e$levels_sorted[[1]]$display_label, "Haut")
  expect_equal(e$levels_sorted[[2]]$order, 2L)
  expect_equal(e$levels_sorted[[3]]$order, 3L)

  # Missing
  expect_length(e$missing_levels, 1)
  expect_equal(e$missing_levels[[1]]$code, "99")
})


# ---------------------------------------------------------------------------
# D. Binary-battery numbering (positive = position, negative = 9…9 sentinel)
# ---------------------------------------------------------------------------

# Build `n` binary members sharing one battery title.
batt_vars <- function(n, title = "Batterie", role = "factor_binary") {
  stats::setNames(lapply(seq_len(n), function(k) list(
    var_label = paste0("Item ", k), role = role, new_name = paste0("B", k),
    battery = title,
    levels = list(
      "1" = list(order = 1L, label = "Oui", new_label = paste0("Item", k),      n = 10L, pct = 30),
      "2" = list(order = 2L, label = "Non", new_label = paste0("Pas item", k),  n = 20L, pct = 70)))),
    paste0("B", seq_len(n)))
}

test_that("D1: 3-member binary battery -> positive 1,2,3 ; negative sentinel 9", {
  entries <- .gfs_build_entries(batt_vars(3))
  for (k in seq_along(entries)) {
    e <- entries[[k]]
    expect_equal(e$max_order, 9L)
    expect_equal(e$levels_sorted[[1]]$order, k)     # positive -> battery position
    expect_equal(e$levels_sorted[[2]]$order, 9L)    # negative -> all-nines sentinel
    expect_equal(.gfs_level_label(e$levels_sorted[[1]], e$max_order), paste0(k, "-Item", k))
    expect_equal(.gfs_level_label(e$levels_sorted[[2]], e$max_order), paste0("9-Pas item", k))
  }
})

test_that("D2: 10-member battery -> positives 01..10, negative 99 (width tracks size)", {
  entries <- .gfs_build_entries(batt_vars(10))
  expect_equal(entries[[1]]$max_order, 99L)
  expect_equal(.gfs_level_label(entries[[1]]$levels_sorted[[1]], entries[[1]]$max_order), "01-Item1")
  expect_equal(.gfs_level_label(entries[[10]]$levels_sorted[[1]], entries[[10]]$max_order), "10-Item10")
  expect_equal(.gfs_level_label(entries[[1]]$levels_sorted[[2]], entries[[1]]$max_order), "99-Pas item1")
})

test_that("D3: a battery with a non-binary member is left untouched (silently)", {
  vars <- batt_vars(2)
  vars$B3 <- list(var_label = "Nominal", role = "factor_nominal", new_name = "B3",
    battery = "Batterie",
    levels = list(
      "1" = list(order = 1L, label = "a", new_label = "a", n = 1L, pct = 33),
      "2" = list(order = 2L, label = "b", new_label = "b", n = 1L, pct = 33),
      "3" = list(order = 3L, label = "c", new_label = "c", n = 1L, pct = 34)))
  expect_no_message(entries <- .gfs_build_entries(vars))   # no more "skipped" chatter
  # Members keep their normal per-variable numbering (max_order == 2), not the sentinel.
  expect_equal(entries[[1]]$max_order, 2L)
  expect_equal(entries[[1]]$levels_sorted[[2]]$order, 2L)
})


# ---------------------------------------------------------------------------
# E. Format blocks
# ---------------------------------------------------------------------------

test_that("format: ordinal gets fct_recode + as.ordered()", {
  vars <- list(
    Q1 = list(
      var_label = "Satisfaction", role = "factor_ordinal", new_name = "SATIS",
      levels = list(
        "1" = list(order = 1L, label = "Pas du tout", new_label = "Pas du tout", n = 50L, pct = 10L),
        "2" = list(order = 2L, label = "Moyen",       new_label = "Moyen",       n = 200L, pct = 40L),
        "3" = list(order = 3L, label = "Très",        new_label = "Très",        n = 250L, pct = 50L),
        "9" = list(missing = TRUE, label = "NSP")
      )
    )
  )
  entries <- .gfs_build_entries(vars)
  fmt <- .gfs_format_blocks(entries, "data")
  combined <- paste(fmt, collapse = "\n")

  expect_match(combined, "fct_recode")
  expect_match(combined, "as.ordered()", fixed = TRUE)
  expect_match(combined, "fct_relevel(sort)", fixed = TRUE)
  expect_match(combined, 'factor(as.character(', fixed = TRUE)
  # Missing at end
  expect_match(combined, 'NULL')
})

test_that("format: nominal does NOT get as.ordered()", {
  vars <- list(
    Q1 = list(
      var_label = "Region", role = "factor_nominal", new_name = "REGION",
      levels = list(
        "1" = list(order = 1L, label = "Nord", n = 100L, pct = 50L),
        "2" = list(order = 2L, label = "Sud",  n = 100L, pct = 50L)
      )
    )
  )
  entries <- .gfs_build_entries(vars)
  fmt <- .gfs_format_blocks(entries, "data")
  combined <- paste(fmt, collapse = "\n")

  expect_match(combined, "fct_recode")
  expect_match(combined, "fct_relevel(sort)", fixed = TRUE)
  expect_false(grepl("as.ordered()", combined, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# C-empty. Empty levels (n:0) -> forcats fct_expand() so the pole survives
# ---------------------------------------------------------------------------

test_that("format: an empty level (n:0) emits fct_expand() for its code", {
  vars <- list(
    PAP_X = list(
      var_label = "Autre glisse", role = "factor_binary", new_name = "PAP_X",
      levels = list(
        "1" = list(order = 1L, label = "Oui", n = 0L,  pct = 0L),   # empty pole
        "0" = list(order = 2L, label = "Non", n = 30L, pct = 100L)
      )
    )
  )
  entries <- .gfs_build_entries(vars)
  combined <- paste(.gfs_format_blocks(entries, "data"), collapse = "\n")
  expect_match(combined, 'fct_expand("1")', fixed = TRUE)
  expect_match(combined, "fct_recode", fixed = TRUE)
})

test_that("format: a fully-observed factor emits NO fct_expand()", {
  vars <- list(
    SEXE = list(
      var_label = "Sexe", role = "factor_binary", new_name = "SEXE",
      levels = list(
        "1" = list(order = 1L, label = "Oui", n = 60L, pct = 60L),
        "0" = list(order = 2L, label = "Non", n = 40L, pct = 40L)
      )
    )
  )
  entries <- .gfs_build_entries(vars)
  combined <- paste(.gfs_format_blocks(entries, "data"), collapse = "\n")
  expect_false(grepl("fct_expand", combined, fixed = TRUE))
})

test_that("format: the emitted factor keeps the empty pole as a real (empty) level", {
  skip_if_not_installed("forcats")
  vars <- list(
    PAP_X = list(
      var_label = "Autre glisse", role = "factor_binary", new_name = "PAP_X",
      levels = list(
        "1" = list(order = 1L, label = "Oui", n = 0L,  pct = 0L),
        "0" = list(order = 2L, label = "Non", n = 30L, pct = 100L)
      )
    )
  )
  entries <- .gfs_build_entries(vars)
  fmt <- paste(.gfs_format_blocks(entries, "data"), collapse = "\n")

  # Evaluate the emitted block on data containing only "Non"; forcats fns injected.
  fenv <- list2env(list(fct_recode  = forcats::fct_recode,
                        fct_expand  = forcats::fct_expand,
                        fct_relevel = forcats::fct_relevel),
                   parent = globalenv())
  env  <- new.env(parent = fenv)
  env$data <- data.frame(PAP_X = rep(0, 5))
  expect_warning(eval(parse(text = fmt), envir = env), NA)   # no "Unknown levels" warning
  res <- env$data$PAP_X
  expect_true("1-Oui" %in% levels(res))                       # empty pole is a real level
  expect_equal(as.integer(sum(res == "1-Oui", na.rm = TRUE)), 0L)
  expect_equal(as.integer(sum(res == "2-Non", na.rm = TRUE)), 5L)
})

test_that("format: missing levels recoded to NULL, placed last", {
  vars <- list(
    Q1 = list(
      var_label = "Q", role = "factor_binary", new_name = "Q1",
      levels = list(
        "1" = list(order = 1L, label = "Oui", n = 90L, pct = 90L),
        "2" = list(order = 2L, label = "Non", n = 10L, pct = 10L),
        "8" = list(missing = TRUE, label = "NVPD"),
        "9" = list(missing = TRUE, label = "NSP")
      )
    )
  )
  entries <- .gfs_build_entries(vars)
  fmt <- .gfs_format_blocks(entries, "data")

  # Find recode lines (indented lines with "label" = "code" or NULL = "code")
  recode_lines <- fmt[grepl('^\\s+".*"\\s+=\\s+"', fmt) | grepl("^\\s+NULL\\s+=", fmt)]
  # Last two recode lines should be NULL
  n <- length(recode_lines)
  expect_match(recode_lines[n],     "NULL")
  expect_match(recode_lines[n - 1], "NULL")
  # First two should not be NULL
  expect_false(grepl("NULL", recode_lines[1]))
  expect_false(grepl("NULL", recode_lines[2]))
})

test_that("format: rename block only includes renamed vars", {
  vars <- list(
    Q1 = list(var_label = "Q1", role = "identifier", new_name = "RENAMED", levels = list()),
    Q2 = list(var_label = "Q2", role = "identifier", new_name = "Q2",      levels = list())
  )
  entries <- .gfs_build_entries(vars)
  fmt <- .gfs_format_blocks(entries, "data")
  combined <- paste(fmt, collapse = "\n")

  expect_match(combined, "RENAMED = Q1")
  expect_false(grepl("Q2 = Q2", combined, fixed = TRUE))
})

test_that("format: integer_count gets as.integer + NA assignment", {
  vars <- list(
    AGE = list(
      var_label = "Age", role = "integer_count", new_name = "AGE_P1",
      levels = list(
        "88" = list(missing = TRUE, label = "NVPD"),
        "99" = list(missing = TRUE, label = "NSP")
      )
    )
  )
  entries <- .gfs_build_entries(vars)
  fmt <- .gfs_format_blocks(entries, "data")
  combined <- paste(fmt, collapse = "\n")

  expect_match(combined, "as.integer(as.character(", fixed = TRUE)
  expect_match(combined, "88L", fixed = TRUE)
  expect_match(combined, "99L", fixed = TRUE)
  expect_match(combined, "NA_integer_", fixed = TRUE)
})

test_that("format: double gets as.double + NA_real_", {
  vars <- list(
    WT = list(
      var_label = "Weight", role = "double", new_name = "POIDS",
      levels = list(
        "99" = list(missing = TRUE, label = "NSP")
      )
    )
  )
  entries <- .gfs_build_entries(vars)
  fmt <- .gfs_format_blocks(entries, "data")
  combined <- paste(fmt, collapse = "\n")

  expect_match(combined, "as.double(as.character(", fixed = TRUE)
  expect_match(combined, "NA_real_", fixed = TRUE)
})

test_that("format: '# Valeurs manquantes' comment lists counts, biggest first, blanks last", {
  vars <- list(
    WT = list(
      var_label = "Weight", role = "double", new_name = "POIDS",
      na_n = 1481L, na_pct = 13.0,
      levels = list(
        "8" = list(missing = TRUE, label = "Non concerné", n = 900L),
        "9" = list(missing = TRUE, label = "Non-réponse",  n = 500L))))
  combined <- paste(.gfs_format_blocks(.gfs_build_entries(vars), "data"), collapse = "\n")
  expect_match(combined,
    "# Valeurs manquantes — NA: 1481 (13%) ; 900 Non concerné ; 500 Non-réponse ; 81 vide",
    fixed = TRUE)
})

test_that("format: no missing comment when na_n absent (graceful, JSON pre-stats)", {
  vars <- list(
    WT = list(var_label = "Weight", role = "double", new_name = "POIDS",
              levels = list("9" = list(missing = TRUE, label = "NSP"))))
  combined <- paste(.gfs_format_blocks(.gfs_build_entries(vars), "data"), collapse = "\n")
  expect_false(grepl("Valeurs manquantes", combined, fixed = TRUE))
})

test_that("format: identifier leaves column untouched but applies label", {
  vars <- list(
    ID = list(var_label = "Identifier", role = "identifier", new_name = "ID",
              levels = list())
  )
  entries <- .gfs_build_entries(vars)
  fmt <- .gfs_format_blocks(entries, "data")

  # Column value is NOT reassigned (no conversion)
  expect_false(any(grepl("data\\$ID <-", fmt)))
  # Header comment + label applied via varlab
  expect_true(any(grepl('^# "ID" identifier', fmt)))
  expect_true(any(grepl('"Identifier" -> varlab', fmt, fixed = TRUE)))
  expect_true(any(grepl('attr(data$ID, "label") <- varlab', fmt, fixed = TRUE)))
})

test_that("format: level order matches order field (ascending)", {
  vars <- list(
    Q1 = list(
      var_label = "Age group", role = "factor_ordinal", new_name = "AGE_GRP",
      levels = list(
        "01" = list(order = 4L, label = "20-29", new_label = "20 a 29",     n = 100L, pct = 25L),
        "02" = list(order = 3L, label = "30-39", new_label = "30 a 39",     n = 100L, pct = 25L),
        "03" = list(order = 2L, label = "40-49", new_label = "40 a 49",     n = 100L, pct = 25L),
        "04" = list(order = 1L, label = "50-69", new_label = "50 a 69 ans", n = 100L, pct = 25L)
      )
    )
  )
  entries <- .gfs_build_entries(vars)
  fmt <- .gfs_format_blocks(entries, "data")

  # Find recode lines (indented lines with = "code", pattern)
  recode_lines <- fmt[grepl('^\\s+".*"\\s+=\\s+"', fmt) | grepl("^\\s+NULL\\s+=", fmt)]
  # First recode should be order=1 ("50 a 69 ans" from code "04")
  expect_match(recode_lines[1], "1-50 a 69 ans")
  expect_match(recode_lines[1], '"04"')
  # Last recode should be order=4 ("20 a 29" from code "01")
  expect_match(recode_lines[4], "4-20 a 29")
  expect_match(recode_lines[4], '"01"')
})


# ---------------------------------------------------------------------------
# F. Merged levels (shared order value)
# ---------------------------------------------------------------------------

test_that("format: merged levels — multiple codes map to same prefixed label", {
  vars <- list(
    Q1 = list(
      var_label = "Grouped", role = "factor_ordinal", new_name = "GROUPED",
      levels = list(
        "1" = list(order = 1L, label = "A", new_label = "Group1", n = 50L, pct = 25L),
        "2" = list(order = 1L, label = "B", new_label = "Group1", n = 50L, pct = 25L),
        "3" = list(order = 2L, label = "C", new_label = "Group2", n = 100L, pct = 50L)
      )
    )
  )
  entries <- .gfs_build_entries(vars)
  fmt <- .gfs_format_blocks(entries, "data")

  # Two lines should map to "1-Group1"
  group1_lines <- fmt[grepl("1-Group1", fmt)]
  expect_true(length(group1_lines) >= 2)
})


# ---------------------------------------------------------------------------
# G. Full integration: generate_format_script
# ---------------------------------------------------------------------------

test_that("generate_format_script: produces valid parseable R script", {
  vars <- list(
    Q1 = list(
      var_label = "Sexe", role = "factor_binary", r_class = "double",
      new_name = "SEXE",
      levels = list(
        "1" = list(order = 1L, label = "Homme", n = 500L, pct = 50L),
        "2" = list(order = 2L, label = "Femme", n = 500L, pct = 50L)
      )
    ),
    Q2 = list(
      var_label = "Age", role = "integer_count", r_class = "double",
      new_name = "AGE",
      levels = list(
        "99" = list(missing = TRUE, label = "NSP")
      )
    ),
    ID = list(
      var_label = "Identifiant", role = "identifier", r_class = "integer",
      new_name = "ID",
      levels = list()
    )
  )

  path <- tmp_json()
  on.exit(unlink(path))
  .write_meta_json(make_meta_list(vars), path)

  out_path <- tempfile(fileext = ".R")
  on.exit(unlink(out_path), add = TRUE)
  result <- generate_format_script(path, output_path = out_path)

  expect_equal(result, out_path)
  expect_true(file.exists(out_path))

  script_text <- readLines(out_path, encoding = "UTF-8")
  expect_no_error(parse(text = script_text))
})

test_that("generate_format_script: contains expected sections", {
  vars <- list(
    Q1 = list(
      var_label = "Group", role = "factor_ordinal", r_class = "character",
      new_name = "GROUP",
      levels = list(
        "1" = list(order = 1L, label = "A", new_label = "Alpha", n = 100L, pct = 50L),
        "2" = list(order = 2L, label = "B", new_label = "Beta",  n = 100L, pct = 50L),
        "9" = list(missing = TRUE, label = "NSP")
      )
    )
  )

  path <- tmp_json()
  on.exit(unlink(path))
  .write_meta_json(make_meta_list(vars), path)

  out_path <- tempfile(fileext = ".R")
  on.exit(unlink(out_path), add = TRUE)
  generate_format_script(path, output_path = out_path)

  script <- paste(readLines(out_path, encoding = "UTF-8"), collapse = "\n")

  # Header sections
  expect_match(script, "library(haven)",   fixed = TRUE)
  expect_match(script, "library(dplyr)",   fixed = TRUE)
  expect_match(script, "library(forcats)", fixed = TRUE)

  # No variable-list / select-reorder sections anymore (codebook covers that)
  expect_false(grepl("var_list", script, fixed = TRUE))
  expect_false(grepl("Select and reorder", script, fixed = TRUE))

  # Formatting
  expect_match(script, "fct_recode")
  expect_match(script, "as.ordered()", fixed = TRUE)

  # Variable label applied inline via the `attr<-` pipe (label-first via varlab)
  expect_match(script, '"Group" -> varlab', fixed = TRUE)
  expect_match(script, '`attr<-`("label", varlab)', fixed = TRUE)

  # Rename block should be present (Q1 -> GROUP)
  expect_match(script, "dplyr::rename", fixed = TRUE)
  expect_match(script, "GROUP = Q1", fixed = TRUE)
})

test_that("generate_format_script: no num_stats in JSON omits stats", {
  vars <- list(
    AGE = list(
      var_label = "Age", role = "integer_count", r_class = "double",
      new_name = "AGE",
      levels = list()
    )
  )

  path <- tmp_json()
  on.exit(unlink(path))
  .write_meta_json(make_meta_list(vars), path)

  out_path <- tempfile(fileext = ".R")
  on.exit(unlink(out_path), add = TRUE)
  generate_format_script(path, output_path = out_path)

  script <- paste(readLines(out_path, encoding = "UTF-8"), collapse = "\n")

  # Should NOT contain range/mean/sd stats
  expect_false(grepl("range:", script, fixed = TRUE))
  # But should still have the variable
  expect_match(script, "AGE")
})


test_that("generate_format_script: numeric prefix with 10+ levels uses leading zeros", {
  levels_list <- list()
  for (i in 1:12) {
    code <- formatC(i, width = 2, flag = "0")
    levels_list[[code]] <- list(
      order = as.integer(i), label = paste("Level", i),
      new_label = paste("Niv", i), n = 100L, pct = 8L
    )
  }
  levels_list[["99"]] <- list(missing = TRUE, label = "NSP")

  vars <- list(
    Q1 = list(var_label = "Many levels", role = "factor_nominal",
              r_class = "character", new_name = "MANY", levels = levels_list)
  )

  path <- tmp_json()
  on.exit(unlink(path))
  .write_meta_json(make_meta_list(vars), path)

  out_path <- tempfile(fileext = ".R")
  on.exit(unlink(out_path), add = TRUE)
  generate_format_script(path, output_path = out_path)

  script <- paste(readLines(out_path, encoding = "UTF-8"), collapse = "\n")

  expect_match(script, '"01-Niv 1"')
  expect_match(script, '"12-Niv 12"')
})


# ---------------------------------------------------------------------------
# H. Conversion safety tests (CV1-CV9)
# ---------------------------------------------------------------------------

# These test that the generated code uses the right conversion patterns.
# We check the generated R code text for correct conversion calls.

test_that("CV1: factor conversion always uses factor(as.character())", {
  vars <- list(
    Q1 = list(
      var_label = "Q", role = "factor_binary", new_name = "Q1",
      levels = list(
        "01" = list(order = 1L, label = "Oui", n = 90L, pct = 90L),
        "02" = list(order = 2L, label = "Non", n = 10L, pct = 10L)
      )
    )
  )
  entries <- .gfs_build_entries(vars)
  fmt <- .gfs_format_blocks(entries, "data")
  combined <- paste(fmt, collapse = "\n")

  expect_match(combined, "factor(as.character(", fixed = TRUE)
})

test_that("CV3: integer_count conversion uses as.integer(as.character())", {
  vars <- list(
    AGE = list(
      var_label = "Age", role = "integer_count", new_name = "AGE",
      levels = list("99" = list(missing = TRUE, label = "NSP"))
    )
  )
  entries <- .gfs_build_entries(vars)
  fmt <- .gfs_format_blocks(entries, "data")
  combined <- paste(fmt, collapse = "\n")

  expect_match(combined, "as.integer(as.character(", fixed = TRUE)
})

test_that("CV6: double conversion uses as.double(as.character())", {
  vars <- list(
    WT = list(
      var_label = "Weight", role = "double", new_name = "WT",
      levels = list()
    )
  )
  entries <- .gfs_build_entries(vars)
  fmt <- .gfs_format_blocks(entries, "data")
  combined <- paste(fmt, collapse = "\n")

  expect_match(combined, "as.double(as.character(", fixed = TRUE)
})


# ---------------------------------------------------------------------------
# H. Shared level-label helper + numeric stats comment + inline labels
# ---------------------------------------------------------------------------

test_that("level-label helper: prefix + display label, zero-padded by max_order", {
  lv <- list(order = 2L, display_label = "Natation")
  expect_equal(.gfs_level_label(lv, max_order = 5L), "2-Natation")
  expect_equal(.gfs_level_label(lv, max_order = 12L), "02-Natation")
})

test_that("num stats comment: integer rounds quantiles to 0, mean/sd to 1", {
  st <- list(min = 0, max = 46, mean = 6.0397, sd = 4.9478, q1 = 3, median = 5, q3 = 8)
  expect_equal(.gfs_num_stats_comment(st, digits = 0L),
               "# min=0 Q1=3 median=5 Q3=8 max=46 ; mean 6.0 σ4.9")
  expect_null(.gfs_num_stats_comment(NULL))
})

test_that("factor block: label applied via `attr<-` pipe end, label-first via varlab", {
  vars <- list(
    Q = list(
      var_label = "Aime le sport", role = "factor_binary", new_name = "SPORT",
      levels = list(
        "1" = list(order = 1L, label = "Oui", new_label = "Sport", n = 60L, pct = 60L),
        "0" = list(order = 2L, label = "Non", new_label = "Pas sport", n = 40L, pct = 40L)
      )
    )
  )
  entries <- .gfs_build_entries(vars)
  fmt <- paste(.gfs_format_blocks(entries, "df"), collapse = "\n")
  expect_match(fmt, '"Aime le sport" -> varlab', fixed = TRUE)
  expect_match(fmt, 'fct_relevel(sort) |> `attr<-`("label", varlab)', fixed = TRUE)
  expect_match(fmt, '"1-Sport"', fixed = TRUE)   # value label == codebook val
})
