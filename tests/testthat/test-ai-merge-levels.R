# Tests for ai_merge_levels()
# Functions under test: ai_merge_levels()
# No real API calls are made; ai_call_claude() is replaced via withr::local_bindings.
#
# Test coverage:
#   M1  – basic ordinal merge: correct order integers written to JSON
#   M2  – no-merge case: each key in its own group (all keys preserved)
#   M3  – key mismatch in response → warning, variable skipped
#   M4  – collapse to 2 groups → role updated to factor_binary
#   M5  – missing meta_json arg → stop()
#   M6  – metadata lacks level_counts column → stop()
#   M7  – multi-variable chunk: two vars in one response object
#   M8  – prompt format: parameter line and array brackets present
#   M9  – nominal var included when nominal = TRUE
#   M10 – nominal var excluded when nominal = FALSE (default)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Build a minimal metadata tibble row suitable for ai_merge_levels().
# level_counts and level_freqs are per-level counts/freqs in sorted order.
make_merge_meta <- function(var_name, var_label,
                            detected_role = "factor_ordinal",
                            n_levels = 3L) {
  tibble::tibble(
    var_name      = var_name,
    var_label     = var_label,
    r_class       = "integer",
    n_distinct     = n_levels,
    detected_role  = detected_role,
    values         = list(seq_len(n_levels)),
    level_counts   = list(rep(100L, n_levels)),
    level_freqs    = list(rep(1 / n_levels, n_levels))
  )
}

# Build a JSON vars list with non-missing levels and order/n/pct fields.
make_merge_vars <- function(var_name, keys, labels, n_vec, pct_vec,
                            role = "factor_ordinal") {
  levs <- purrr::set_names(
    purrr::imap(keys, function(k, i) {
      list(label = labels[[i]], n = n_vec[[i]], pct = pct_vec[[i]],
           order = i)
    }),
    keys
  )
  list(var_label = "test", role = role, levels = levs)
}


# ---------------------------------------------------------------------------
# M1 – basic ordinal merge: correct order integers written to JSON
# ---------------------------------------------------------------------------

test_that("M1: ordinal merge response assigns correct order integers", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()

  vars <- list(
    SATISF = make_merge_vars(
      "SATISF",
      keys    = c("1", "2", "3", "4"),
      labels  = c("Très satisfait", "Satisfait", "Peu satisfait", "Pas du tout satisfait"),
      n_vec   = c(3000L, 5000L, 2000L, 500L),
      pct_vec = c(23L, 39L, 16L, 4L)
    )
  )
  .write_meta_json(make_meta_list(vars), path)

  meta <- make_merge_meta("SATISF", "Satisfaction", n_levels = 4L)

  # AI merges [1,2] → group 1, [3,4] → group 2
  fake_resp <- '{"SATISF":[{"order":1,"keys":["1","2"]},{"order":2,"keys":["3","4"]}]}'

  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude", mock_ai(fake_resp), envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)

  suppressMessages(
    ai_merge_levels(meta, meta_json = path)
  )

  res <- .read_meta_json(path)$variables$SATISF$levels
  expect_equal(res[["1"]]$order, 1L, label = "key 1 → group order 1")
  expect_equal(res[["2"]]$order, 1L, label = "key 2 → group order 1")
  expect_equal(res[["3"]]$order, 2L, label = "key 3 → group order 2")
  expect_equal(res[["4"]]$order, 2L, label = "key 4 → group order 2")
})


# ---------------------------------------------------------------------------
# M2 – no-merge: each key in its own group
# ---------------------------------------------------------------------------

test_that("M2: no-merge response keeps each key in its own group", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()

  vars <- list(
    DIPLOME = make_merge_vars(
      "DIPLOME",
      keys    = c("1", "2", "3"),
      labels  = c("Aucun", "Bac", "Bac+5"),
      n_vec   = c(2000L, 4000L, 4000L),
      pct_vec = c(20L, 40L, 40L)
    )
  )
  .write_meta_json(make_meta_list(vars), path)

  meta <- make_merge_meta("DIPLOME", "Diplôme", n_levels = 3L)

  fake_resp <- '{"DIPLOME":[{"order":1,"keys":["1"]},{"order":2,"keys":["2"]},{"order":3,"keys":["3"]}]}'

  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude", mock_ai(fake_resp), envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)

  suppressMessages(
    ai_merge_levels(meta, meta_json = path)
  )

  res <- .read_meta_json(path)$variables$DIPLOME$levels
  expect_equal(res[["1"]]$order, 1L)
  expect_equal(res[["2"]]$order, 2L)
  expect_equal(res[["3"]]$order, 3L)
})


# ---------------------------------------------------------------------------
# M3 – key mismatch in response → warning + variable skipped
# ---------------------------------------------------------------------------

test_that("M3: key mismatch in AI response triggers warning and skips variable", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()

  vars <- list(
    FREQ = make_merge_vars(
      "FREQ",
      keys    = c("1", "2", "3"),
      labels  = c("Jamais", "Parfois", "Souvent"),
      n_vec   = c(5000L, 3000L, 2000L),
      pct_vec = c(50L, 30L, 20L)
    )
  )
  .write_meta_json(make_meta_list(vars), path)

  meta <- make_merge_meta("FREQ", "Fréquence", n_levels = 3L)

  # Response references key "9" which doesn't exist in input
  fake_resp <- '{"FREQ":[{"order":1,"keys":["1"]},{"order":2,"keys":["2","9"]}]}'

  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude", mock_ai(fake_resp), envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)

  expect_warning(
    suppressMessages(
      ai_merge_levels(meta, meta_json = path)
    ),
    regexp = "FREQ"
  )

  # Variable should not have been updated (orders remain from initial write = 1,2,3)
  res <- .read_meta_json(path)$variables$FREQ$levels
  expect_equal(res[["1"]]$order, 1L, label = "FREQ key 1 order unchanged after mismatch")
  expect_equal(res[["2"]]$order, 2L, label = "FREQ key 2 order unchanged after mismatch")
  expect_equal(res[["3"]]$order, 3L, label = "FREQ key 3 order unchanged after mismatch")
})


# ---------------------------------------------------------------------------
# M4 – collapse to 2 groups → role set to factor_binary
# ---------------------------------------------------------------------------

test_that("M4: 2-group merge sets role to factor_binary", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()

  vars <- list(
    CONF = make_merge_vars(
      "CONF",
      keys    = c("00", "01", "02"),
      labels  = c("Non", "Parfois", "Souvent"),
      n_vec   = c(9000L, 600L, 400L),
      pct_vec = c(90L, 6L, 4L)
    )
  )
  .write_meta_json(make_meta_list(vars), path)

  meta <- make_merge_meta("CONF", "Conflit", n_levels = 3L)

  fake_resp <- '{"CONF":[{"order":1,"keys":["00"]},{"order":2,"keys":["01","02"]}]}'

  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude", mock_ai(fake_resp), envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)

  suppressMessages(
    ai_merge_levels(meta, meta_json = path)
  )

  res_var <- .read_meta_json(path)$variables$CONF
  expect_equal(res_var$role, "factor_binary",
               label = "2-group collapse → role must be factor_binary")
  expect_equal(res_var$levels[["00"]]$order,  1L)
  expect_equal(res_var$levels[["01"]]$order,  2L)
  expect_equal(res_var$levels[["02"]]$order,  2L)
})


# ---------------------------------------------------------------------------
# M5 – missing meta_json → stop()
# ---------------------------------------------------------------------------

test_that("M5: missing meta_json argument triggers stop()", {
  meta <- make_merge_meta("X", "x", n_levels = 3L)
  expect_error(
    suppressMessages(ai_merge_levels(meta)),
    regexp = "meta_json"
  )
})


# ---------------------------------------------------------------------------
# M6 – metadata lacks level_counts column → stop()
# ---------------------------------------------------------------------------

test_that("M6: metadata without level_counts column triggers stop()", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()

  vars <- list(X = list(var_label = "x", role = "factor_ordinal",
                        levels = list("1" = list(label = "A", order = 1L))))
  .write_meta_json(make_meta_list(vars), path)

  # Metadata tibble without level_counts
  meta <- tibble::tibble(
    var_name      = "X",
    var_label     = "x",
    r_class       = "integer",
    n_distinct     = 1L,
    detected_role  = "factor_ordinal",
    values         = list(1L)
  )

  expect_error(
    suppressMessages(ai_merge_levels(meta, meta_json = path)),
    regexp = "level_counts"
  )
})


# ---------------------------------------------------------------------------
# M7 – multi-variable chunk: two vars updated from one response
# ---------------------------------------------------------------------------

test_that("M7: two variables in one chunk both get order integers updated", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()

  vars <- list(
    VAR_A = make_merge_vars("VAR_A", c("1","2","3"),
                            c("Bas","Moyen","Haut"),
                            c(2000L,5000L,3000L), c(20L,50L,30L)),
    VAR_B = make_merge_vars("VAR_B", c("0","1"),
                            c("Non","Oui"),
                            c(8000L,2000L), c(80L,20L))
  )
  .write_meta_json(make_meta_list(vars), path)

  meta <- dplyr::bind_rows(
    make_merge_meta("VAR_A", "Variable A", n_levels = 3L),
    make_merge_meta("VAR_B", "Variable B", n_levels = 2L)
  )

  fake_resp <- paste0(
    '{"VAR_A":[{"order":1,"keys":["1","2"]},{"order":2,"keys":["3"]}],',
    '"VAR_B":[{"order":1,"keys":["0"]},{"order":2,"keys":["1"]}]}'
  )

  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude", mock_ai(fake_resp), envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)

  suppressMessages(
    ai_merge_levels(meta, meta_json = path)
  )

  res <- .read_meta_json(path)$variables
  expect_equal(res$VAR_A$levels[["1"]]$order, 1L)
  expect_equal(res$VAR_A$levels[["2"]]$order, 1L)
  expect_equal(res$VAR_A$levels[["3"]]$order, 2L)
  expect_equal(res$VAR_B$levels[["0"]]$order, 1L)
  expect_equal(res$VAR_B$levels[["1"]]$order, 2L)
})


# ---------------------------------------------------------------------------
# M8 – prompt format: parameter line and array brackets present
# ---------------------------------------------------------------------------

test_that("M8: dry_run prompt contains parameter line and array brackets", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()

  vars <- list(
    FREQ = make_merge_vars("FREQ", c("1","2","3"),
                           c("Jamais","Parfois","Souvent"),
                           c(5000L,3000L,2000L), c(50L,30L,20L))
  )
  .write_meta_json(make_meta_list(vars), path)

  meta <- make_merge_meta("FREQ", "Fréquence", n_levels = 3L)

  prompts <- suppressMessages(
    ai_merge_levels(meta, meta_json = path, dry_run = TRUE)
  )

  expect_type(prompts, "list")
  expect_true(length(prompts) >= 1L)

  p <- prompts[[1]]
  # JSON preamble format
  expect_true(grepl('"optimal_levels"', p), label = "prompt must use JSON preamble key 'optimal_levels'")
  expect_true(grepl('"min_pct"', p),        label = "prompt must use JSON preamble key 'min_pct'")
  # Old mini-DSL format must NOT appear
  expect_false(grepl("optimal_levels:[0-9]", p), label = "old mini-DSL format must be absent")
  # Array brackets present
  expect_true(grepl("\\[", p),              label = "prompt must open with [ array bracket")
  expect_true(grepl("\\]", p),              label = "prompt must close with ] array bracket")
  # Variable present
  expect_true(grepl('"FREQ"', p),           label = "prompt must contain variable name")
})


# ---------------------------------------------------------------------------
# M9 – nominal included when nominal = TRUE
# ---------------------------------------------------------------------------

test_that("M9: nominal variable processed when nominal = TRUE", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()

  vars <- list(
    REGION = make_merge_vars("REGION", c("1","2","3"),
                             c("Nord","Sud","Est"),
                             c(3000L,4000L,3000L), c(30L,40L,30L),
                             role = "factor_nominal")
  )
  .write_meta_json(make_meta_list(vars), path)

  meta <- make_merge_meta("REGION", "Région",
                          detected_role = "factor_nominal", n_levels = 3L)

  fake_resp <- '{"REGION":[{"order":1,"keys":["1","2"]},{"order":2,"keys":["3"]}]}'

  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude", mock_ai(fake_resp), envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)

  suppressMessages(
    ai_merge_levels(meta, meta_json = path, nominal = TRUE)
  )

  res <- .read_meta_json(path)$variables$REGION$levels
  expect_equal(res[["1"]]$order, 1L)
  expect_equal(res[["2"]]$order, 1L)
  expect_equal(res[["3"]]$order, 2L)
})


# ---------------------------------------------------------------------------
# M10 – nominal excluded when nominal = FALSE (default)
# ---------------------------------------------------------------------------

test_that("M10: nominal variable skipped when nominal = FALSE (default)", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()

  vars <- list(
    REGION = make_merge_vars("REGION", c("1","2","3"),
                             c("Nord","Sud","Est"),
                             c(3000L,4000L,3000L), c(30L,40L,30L),
                             role = "factor_nominal")
  )
  .write_meta_json(make_meta_list(vars), path)

  meta <- make_merge_meta("REGION", "Région",
                          detected_role = "factor_nominal", n_levels = 3L)

  # API should not be called — if it is, the test fails
  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude",
    function(...) stop("API should not be called when nominal = FALSE"),
    envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)

  suppressMessages(
    ai_merge_levels(meta, meta_json = path, nominal = FALSE)
  )

  # Orders must be unchanged (initial values written by make_merge_vars: 1,2,3)
  res <- .read_meta_json(path)$variables$REGION$levels
  expect_equal(res[["1"]]$order, 1L)
  expect_equal(res[["2"]]$order, 2L)
  expect_equal(res[["3"]]$order, 3L)
})


# ---------------------------------------------------------------------------
# M11 – nominal = FALSE: system prompt must not contain nominal section text
# ---------------------------------------------------------------------------

test_that("M11: dry_run system prompt excludes nominal rules when nominal = FALSE", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()

  vars <- list(
    SATISF = make_merge_vars(
      "SATISF",
      keys    = c("1", "2", "3"),
      labels  = c("Peu", "Moyen", "Très"),
      n_vec   = c(2000L, 5000L, 3000L),
      pct_vec = c(20L, 50L, 30L)
    )
  )
  .write_meta_json(make_meta_list(vars), path)
  meta <- make_merge_meta("SATISF", "Satisfaction", n_levels = 3L)

  output_lines <- capture.output(
    suppressMessages(
      ai_merge_levels(meta, meta_json = path, dry_run = TRUE, nominal = FALSE)
    )
  )
  captured <- paste(output_lines, collapse = "\n")

  expect_false(grepl("R\u00e8gles pour variables nominales", captured),
               label = "system prompt must not contain 'Règles pour variables nominales'")
  expect_false(grepl("\\bN1\\b", captured),
               label = "N1 rule must be absent")
  expect_false(grepl("\\bN2\\b", captured),
               label = "N2 rule must be absent")
  expect_false(grepl("### Nominale", captured),
               label = "'### Nominale' example headers must be absent")
  # Ordinal rules must still be present
  expect_true(grepl("R\u00e8gles pour variables ordinales", captured),
              label = "ordinal rules must still be present")
})


# ---------------------------------------------------------------------------
# M12 – nominal = TRUE: system prompt must contain nominal section text
# ---------------------------------------------------------------------------

test_that("M12: dry_run system prompt includes nominal rules when nominal = TRUE", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()

  vars <- list(
    REGION = make_merge_vars(
      "REGION",
      keys    = c("1", "2", "3"),
      labels  = c("Nord", "Sud", "Est"),
      n_vec   = c(3000L, 4000L, 3000L),
      pct_vec = c(30L, 40L, 30L),
      role    = "factor_nominal"
    )
  )
  .write_meta_json(make_meta_list(vars), path)
  meta <- make_merge_meta("REGION", "R\u00e9gion",
                          detected_role = "factor_nominal", n_levels = 3L)

  output_lines <- capture.output(
    suppressMessages(
      ai_merge_levels(meta, meta_json = path, dry_run = TRUE, nominal = TRUE)
    )
  )
  captured <- paste(output_lines, collapse = "\n")

  expect_true(grepl("R\u00e8gles pour variables nominales", captured),
              label = "system prompt must contain 'Règles pour variables nominales'")
  expect_true(grepl("\\bN1\\b", captured),
              label = "N1 rule must be present")
  expect_true(grepl("\\bN2\\b", captured),
              label = "N2 rule must be present")
  expect_true(grepl("### Nominale", captured),
              label = "'### Nominale' example headers must be present")
})
