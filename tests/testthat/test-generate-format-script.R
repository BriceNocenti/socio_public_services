# Tests for generate_format_script() and its internal helpers.
# Functions under test: .gfs_numeric_prefix, .gfs_compute_numeric_stats,
#   .gfs_build_entries, .gfs_codebook_lines, .gfs_format_blocks,
#   generate_format_script


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
# D. Codebook lines
# ---------------------------------------------------------------------------

test_that("codebook: ordinal/nominal uses 2 lines", {
  vars <- list(
    Q1 = list(
      var_label = "Group age", role = "factor_ordinal", new_name = "AGE_GRP",
      levels = list(
        "1" = list(order = 1L, label = "Young", new_label = "Jeune", n = 100L, pct = 50L),
        "2" = list(order = 2L, label = "Old",   new_label = "Vieux", n = 100L, pct = 50L),
        "9" = list(missing = TRUE, label = "NSP")
      )
    )
  )
  entries <- .gfs_build_entries(vars)
  cb <- .gfs_codebook_lines(entries)

  # Should have: var_list <- c( + 2 content lines + )
  content_lines <- cb[!grepl("^(var_list|\\))", cb)]
  expect_length(content_lines, 2)

  # Line 1: variable name + var_label
  expect_match(content_lines[1], '"AGE_GRP"')
  expect_match(content_lines[1], "Group age")

  # Line 2: continuation with levels
  expect_match(content_lines[2], "^\\s+#")
  expect_match(content_lines[2], "1-Jeune")
  expect_match(content_lines[2], "2-Vieux")
})

test_that("codebook: binary uses 1 line", {
  vars <- list(
    Q_BIN = list(
      var_label = "Married?", role = "factor_binary", new_name = "MARRIED",
      levels = list(
        "1" = list(order = 1L, label = "Yes", new_label = "Oui", n = 300L, pct = 60L),
        "2" = list(order = 2L, label = "No",  new_label = "Non", n = 200L, pct = 40L),
        "9" = list(missing = TRUE, label = "NSP")
      )
    )
  )
  entries <- .gfs_build_entries(vars)
  cb <- .gfs_codebook_lines(entries)

  content_lines <- cb[!grepl("^(var_list|\\))", cb)]
  expect_length(content_lines, 1)
  expect_match(content_lines[1], '"MARRIED"')
  expect_match(content_lines[1], "1-Oui")
  expect_match(content_lines[1], "2-Non")
})

test_that("codebook: numeric variable uses 1 line", {
  vars <- list(
    AGE = list(
      var_label = "Age in years", role = "integer_count", new_name = "AGE_P1",
      levels = list()
    )
  )
  entries <- .gfs_build_entries(vars)
  cb <- .gfs_codebook_lines(entries)

  content_lines <- cb[!grepl("^(var_list|\\))", cb)]
  expect_length(content_lines, 1)
  expect_match(content_lines[1], '"AGE_P1"')
  expect_match(content_lines[1], "Age in years")
})

test_that("codebook: identifier uses 1 line with role keyword", {
  vars <- list(
    ID = list(
      var_label = "Identifier", role = "identifier", new_name = "ID",
      levels = list()
    )
  )
  entries <- .gfs_build_entries(vars)
  cb <- .gfs_codebook_lines(entries)

  content_lines <- cb[!grepl("^(var_list|\\))", cb)]
  expect_length(content_lines, 1)
  expect_match(content_lines[1], "identifier")
  expect_match(content_lines[1], "Identifier")
})

test_that("codebook: no trailing comma on last entry", {
  vars <- list(
    Q1 = list(var_label = "Q1", role = "identifier", new_name = "V1", levels = list()),
    Q2 = list(var_label = "Q2", role = "identifier", new_name = "V2", levels = list())
  )
  entries <- .gfs_build_entries(vars)
  cb <- .gfs_codebook_lines(entries)

  # Last content line (before closing paren) should not have comma after the quoted name
  content_lines <- cb[!grepl("^(var_list|\\))", cb)]
  last_line <- content_lines[length(content_lines)]
  # Should have "V2" followed by space then # (no comma)
  expect_false(grepl('"V2",', last_line, fixed = TRUE))
  expect_true(grepl('"V2"\\s', last_line))
})

test_that("codebook: orig name hidden when same as new_name", {
  vars <- list(
    SAME = list(var_label = "Same name var", role = "identifier", new_name = "SAME",
                levels = list())
  )
  entries <- .gfs_build_entries(vars)
  cb <- .gfs_codebook_lines(entries)

  content <- paste(cb, collapse = "\n")
  # The orig name "SAME" should NOT appear as a second mention after the label
  # It appears in "SAME" as the var name, but not again in the comment
  comment_part <- sub('.*# ', '', cb[grepl('"SAME"', cb)])
  # The comment should have 'identifier "Same name var"' without trailing SAME
  expect_false(grepl('Same name var"\\s+SAME', comment_part))
})

test_that("codebook: padding alignment — all # at same column", {
  vars <- list(
    Q_SHORT = list(var_label = "Short", role = "identifier", new_name = "A",
                   levels = list()),
    Q_LONG_NAME = list(var_label = "Long", role = "identifier",
                        new_name = "VERY_LONG_VARIABLE_NAME", levels = list())
  )
  entries <- .gfs_build_entries(vars)
  cb <- .gfs_codebook_lines(entries)

  content_lines <- cb[!grepl("^(var_list|\\))", cb)]
  # Find position of first # in each line
  hash_positions <- regexpr("# ", content_lines)
  # All should be at the same position
  expect_true(length(unique(hash_positions)) == 1)
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

test_that("format: identifier produces comment-only, no assignment", {
  vars <- list(
    ID = list(var_label = "Identifier", role = "identifier", new_name = "ID",
              levels = list())
  )
  entries <- .gfs_build_entries(vars)
  fmt <- .gfs_format_blocks(entries, "data")

  # No assignment (no <-)
  assignment_lines <- fmt[grepl("<-", fmt)]
  expect_length(assignment_lines, 0)

  # Has a comment line
  comment_lines <- fmt[grepl("^# ID", fmt)]
  expect_true(length(comment_lines) >= 1)
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

  # Codebook
  expect_match(script, "var_list <- c(", fixed = TRUE)
  expect_match(script, '"GROUP"')

  # Formatting
  expect_match(script, "fct_recode")
  expect_match(script, "as.ordered()", fixed = TRUE)

  # Variable labels
  expect_match(script, "Variable labels", fixed = TRUE)
  expect_match(script, 'attr(data$GROUP, "label")', fixed = TRUE)

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
