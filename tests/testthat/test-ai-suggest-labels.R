# Tests for ai_suggest_labels() — order field preservation.
# Functions under test: ai_suggest_labels(), .parse_labels_json_responses(),
#                       .merge_labels_into_meta_vars(), .build_levels_map()
#
# Focus: the "order" integers set by ai_classify_roles() must survive
#        ai_suggest_labels() untouched, for ordinal, binary and nominal variables.
#
# No real API calls are made. ai_call_claude() is replaced via withr::local_bindings.

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Build a minimal metadata tibble row suitable for ai_suggest_labels().
make_labels_meta <- function(var_name, var_label, detected_role,
                              values_vec, labels_vec, missing_vec,
                              new_labels_vec, order_vec,
                              level_counts_vec = NULL, level_freqs_vec = NULL) {
  n <- length(values_vec)
  tibble::tibble(
    var_name        = var_name,
    var_label       = var_label,
    r_class         = "integer",
    n_distinct      = sum(new_labels_vec != "NULL"),
    n_distinct_data = sum(new_labels_vec != "NULL"),
    detected_role   = detected_role,
    values          = list(values_vec),
    labels          = list(labels_vec),
    missing_vals    = list(missing_vec),
    new_labels      = list(new_labels_vec),
    new_name        = var_name,
    order           = list(order_vec),
    level_counts    = list(if (is.null(level_counts_vec)) rep(100L, n) else level_counts_vec),
    level_freqs     = list(if (is.null(level_freqs_vec))  rep(1/n, n) else level_freqs_vec)
  )
}

# Build a JSON vars list with non-missing levels already having order integers.
make_labels_vars <- function(var_name, keys, labels, order_vec,
                              missing_keys = character(0),
                              role = "factor_ordinal") {
  levs <- purrr::set_names(
    purrr::imap(keys, function(k, i) {
      if (k %in% missing_keys) {
        list(label = labels[[i]], missing = TRUE)
      } else {
        list(label = labels[[i]], order = order_vec[[i]])
      }
    }),
    keys
  )
  list(var_label = "test", role = role, new_name = var_name, levels = levs)
}


# ---------------------------------------------------------------------------
# L. ai_suggest_labels() — order preservation
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# L1: ordinal variable — JSON order integers untouched after label suggestion
# ---------------------------------------------------------------------------

test_that("L1: ordinal variable — order integers in JSON unchanged after ai_suggest_labels", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()

  vars <- list(
    FREQ = make_labels_vars(
      "FREQ",
      keys      = c("1", "2", "3", "9"),
      labels    = c("Toujours", "Souvent", "Rarement", "NSP"),
      order_vec = c(1L, 2L, 3L, NA),
      missing_keys = "9",
      role = "factor_ordinal"
    )
  )
  .write_meta_json(make_meta_list(vars), path)

  meta <- make_labels_meta(
    "FREQ", "Fréquence", "factor_ordinal",
    values_vec    = c("1", "2", "3", "9"),
    labels_vec    = c("Toujours", "Souvent", "Rarement", "NSP"),
    missing_vec   = "9",
    new_labels_vec = c("Toujours", "Souvent", "Rarement", "NULL"),
    order_vec     = c(1L, 2L, 3L, NA_integer_)
  )

  # AI suggests new labels keyed by value code
  fake_resp <- '{"FREQ": {"1": "Très souvent", "2": "Souvent", "3": "Rarement"}}'
  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude", mock_ai(fake_resp), envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)

  suppressMessages(ai_suggest_labels(meta, meta_json = path))

  res <- .read_meta_json(path)$variables$FREQ$levels
  # Order integers must be unchanged
  expect_equal(res[["1"]]$order, 1L)
  expect_equal(res[["2"]]$order, 2L)
  expect_equal(res[["3"]]$order, 3L)
  expect_null(res[["9"]]$order)
  # New labels were written
  expect_equal(res[["1"]]$new_label, "Très souvent")
  expect_equal(res[["2"]]$new_label, "Souvent")
  expect_equal(res[["3"]]$new_label, "Rarement")
  # Missing level has no new_label
  expect_null(res[["9"]]$new_label)
})

# ---------------------------------------------------------------------------
# L2: binary variable with reversed order (positive = 2nd key, order 1)
# ---------------------------------------------------------------------------

test_that("L2: binary reversed order (positive=2nd key) preserved after ai_suggest_labels", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()

  # Key "2" is positive (order=1), key "1" is negative (order=2)
  vars <- list(
    VOTE = make_labels_vars(
      "VOTE",
      keys      = c("1", "2", "9"),
      labels    = c("Non", "Oui", "NSP"),
      order_vec = c(2L, 1L, NA),
      missing_keys = "9",
      role = "factor_binary"
    )
  )
  .write_meta_json(make_meta_list(vars), path)

  meta <- make_labels_meta(
    "VOTE", "A voté ?", "factor_binary",
    values_vec    = c("1", "2", "9"),
    labels_vec    = c("Non", "Oui", "NSP"),
    missing_vec   = "9",
    new_labels_vec = c("Non", "Oui", "NULL"),
    order_vec     = c(2L, 1L, NA_integer_)
  )

  fake_resp <- '{"VOTE": {"1": "Non", "2": "Oui"}}'
  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude", mock_ai(fake_resp), envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)

  suppressMessages(ai_suggest_labels(meta, meta_json = path))

  res <- .read_meta_json(path)$variables$VOTE$levels
  # Reversed order must be preserved: key "2" = order 1 (positive)
  expect_equal(res[["1"]]$order, 2L)
  expect_equal(res[["2"]]$order, 1L)
  expect_null(res[["9"]]$order)
  # Labels written correctly
  expect_equal(res[["1"]]$new_label, "Non")
  expect_equal(res[["2"]]$new_label, "Oui")
})

# ---------------------------------------------------------------------------
# L3: ordinal with merged levels (same order integer) — group label expanded
# ---------------------------------------------------------------------------

test_that("L3: merged ordinal (shared order integer) — group label expanded to all members, order kept", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()

  # Keys "1" and "2" share order=1 (merged group); "3"=order2, "4"=order3
  vars <- list(
    SATIS = make_labels_vars(
      "SATIS",
      keys      = c("1", "2", "3", "4", "9"),
      labels    = c("Pas du tout", "Peu", "Moyen", "Beaucoup", "NSP"),
      order_vec = c(1L, 1L, 2L, 3L, NA),
      missing_keys = "9",
      role = "factor_ordinal"
    )
  )
  .write_meta_json(make_meta_list(vars), path)

  meta <- make_labels_meta(
    "SATIS", "Satisfaction", "factor_ordinal",
    values_vec    = c("1", "2", "3", "4", "9"),
    labels_vec    = c("Pas du tout", "Peu", "Moyen", "Beaucoup", "NSP"),
    missing_vec   = "9",
    new_labels_vec = c("Pas du tout", "Peu", "Moyen", "Beaucoup", "NULL"),
    order_vec     = c(1L, 1L, 2L, 3L, NA_integer_)
  )

  # AI returns one label per group: group key = first code in group = "1"
  # Groups are: {1,2}→"Faible", {3}→"Moyen", {4}→"Élevé"
  fake_resp <- '{"SATIS": {"1": "Faible", "3": "Moyen", "4": "Élevé"}}'
  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude", mock_ai(fake_resp), envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)

  suppressMessages(ai_suggest_labels(meta, meta_json = path))

  res <- .read_meta_json(path)$variables$SATIS$levels
  # Both merged members get the same label
  expect_equal(res[["1"]]$new_label, "Faible")
  expect_equal(res[["2"]]$new_label, "Faible")
  expect_equal(res[["3"]]$new_label, "Moyen")
  expect_equal(res[["4"]]$new_label, "Élevé")
  # Order integers must be unchanged
  expect_equal(res[["1"]]$order, 1L)
  expect_equal(res[["2"]]$order, 1L)
  expect_equal(res[["3"]]$order, 2L)
  expect_equal(res[["4"]]$order, 3L)
  expect_null(res[["9"]]$order)
})

# ---------------------------------------------------------------------------
# L4: nominal variable — no order field written by ai_suggest_labels
# ---------------------------------------------------------------------------

test_that("L4: nominal variable — ai_suggest_labels does not modify existing order integers", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()

  # Nominal variable with explicit order integers already in JSON
  # (as assigned by .read_meta_json migration for levels lacking order)
  vars <- list(
    PAYS = make_labels_vars(
      "PAYS",
      keys      = c("1", "2", "9"),
      labels    = c("France", "Allemagne", "NSP"),
      order_vec = c(1L, 2L, NA),
      missing_keys = "9",
      role = "factor_nominal"
    )
  )
  .write_meta_json(make_meta_list(vars), path)

  meta <- make_labels_meta(
    "PAYS", "Pays de naissance", "factor_nominal",
    values_vec    = c("1", "2", "9"),
    labels_vec    = c("France", "Allemagne", "NSP"),
    missing_vec   = "9",
    new_labels_vec = c("France", "Allemagne", "NULL"),
    order_vec     = c(1L, 2L, NA_integer_)
  )

  fake_resp <- '{"PAYS": {"1": "France", "2": "Allemagne"}}'
  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude", mock_ai(fake_resp), envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)

  suppressMessages(ai_suggest_labels(meta, meta_json = path))

  res <- .read_meta_json(path)$variables$PAYS$levels
  # Order integers must be unchanged by ai_suggest_labels
  expect_equal(res[["1"]]$order, 1L)
  expect_equal(res[["2"]]$order, 2L)
  expect_null(res[["9"]]$order)
  # Labels still written
  expect_equal(res[["1"]]$new_label, "France")
  expect_equal(res[["2"]]$new_label, "Allemagne")
  # Missing level has no new_label
  expect_null(res[["9"]]$new_label)
})

# ---------------------------------------------------------------------------
# L5: un-permutation — send in order 3→2→1, result in original position order
# ---------------------------------------------------------------------------

test_that("L5: un-permutation correct — reversed ordinal sends in order 3,2,1 but result restored to positions 1,2,3", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()

  # order: key "1"→order3, "2"→order2, "3"→order1 (descending coded variable)
  # AI sees levels in order 3→2→1 (ascending order integers 1,2,3)
  # so AI receives key "3" first, "2" second, "1" third
  vars <- list(
    AGREE = make_labels_vars(
      "AGREE",
      keys      = c("1", "2", "3", "9"),
      labels    = c("Pas du tout", "Un peu", "Tout à fait", "NSP"),
      order_vec = c(3L, 2L, 1L, NA),
      missing_keys = "9",
      role = "factor_ordinal"
    )
  )
  .write_meta_json(make_meta_list(vars), path)

  meta <- make_labels_meta(
    "AGREE", "Accord", "factor_ordinal",
    values_vec    = c("1", "2", "3", "9"),
    labels_vec    = c("Pas du tout", "Un peu", "Tout à fait", "NSP"),
    missing_vec   = "9",
    new_labels_vec = c("Pas du tout", "Un peu", "Tout à fait", "NULL"),
    order_vec     = c(3L, 2L, 1L, NA_integer_)
  )

  # AI response keyed by value code (AI sees them in order 3,2,1 but responds by code)
  fake_resp <- '{"AGREE": {"3": "Très fort", "2": "Moyen", "1": "Faible"}}'
  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude", mock_ai(fake_resp), envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)

  suppressMessages(ai_suggest_labels(meta, meta_json = path))

  res <- .read_meta_json(path)$variables$AGREE$levels
  # Labels matched by key, order unchanged
  expect_equal(res[["1"]]$new_label, "Faible")     # key "1" had order=3 → label for "Faible"
  expect_equal(res[["2"]]$new_label, "Moyen")       # key "2" had order=2
  expect_equal(res[["3"]]$new_label, "Très fort")   # key "3" had order=1 (top)
  # Order integers preserved as-is
  expect_equal(res[["1"]]$order, 3L)
  expect_equal(res[["2"]]$order, 2L)
  expect_equal(res[["3"]]$order, 1L)
  expect_null(res[["9"]]$order)
})

# ---------------------------------------------------------------------------
# L6: missing levels — NULL-coded levels never get new_label in JSON
# ---------------------------------------------------------------------------

test_that("L6: NULL-coded (missing) levels never get new_label written to JSON", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()

  vars <- list(
    SAT = make_labels_vars(
      "SAT",
      keys      = c("1", "2", "88", "99"),
      labels    = c("Satisfait", "Insatisfait", "NVPD", "NSP"),
      order_vec = c(1L, 2L, NA, NA),
      missing_keys = c("88", "99"),
      role = "factor_binary"
    )
  )
  .write_meta_json(make_meta_list(vars), path)

  meta <- make_labels_meta(
    "SAT", "Satisfaction", "factor_binary",
    values_vec    = c("1", "2", "88", "99"),
    labels_vec    = c("Satisfait", "Insatisfait", "NVPD", "NSP"),
    missing_vec   = c("88", "99"),
    new_labels_vec = c("Satisfait", "Insatisfait", "NULL", "NULL"),
    order_vec     = c(1L, 2L, NA_integer_, NA_integer_)
  )

  # AI attempts to label missing codes (should be ignored)
  fake_resp <- '{"SAT": {"1": "Satisfait", "2": "Insatisfait", "88": "NVPD", "99": "NSP"}}'
  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude", mock_ai(fake_resp), envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)

  suppressMessages(ai_suggest_labels(meta, meta_json = path))

  res <- .read_meta_json(path)$variables$SAT$levels
  # Non-missing: labels written, order intact
  expect_equal(res[["1"]]$new_label, "Satisfait")
  expect_equal(res[["2"]]$new_label, "Insatisfait")
  expect_equal(res[["1"]]$order, 1L)
  expect_equal(res[["2"]]$order, 2L)
  # Missing levels: no new_label, no order
  expect_null(res[["88"]]$new_label)
  expect_null(res[["99"]]$new_label)
  expect_null(res[["88"]]$order)
  expect_null(res[["99"]]$order)
  expect_true(isTRUE(res[["88"]]$missing))
  expect_true(isTRUE(res[["99"]]$missing))
})

# ---------------------------------------------------------------------------
# L7: multi-variable chunk — order preserved independently for each variable
# ---------------------------------------------------------------------------

test_that("L7: multi-variable — order preserved independently for each variable in same chunk", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()

  # Two variables: one ordinal (ascending), one binary (reversed)
  vars <- list(
    FREQ = make_labels_vars(
      "FREQ",
      keys      = c("1", "2", "3", "9"),
      labels    = c("Jamais", "Parfois", "Toujours", "NSP"),
      order_vec = c(3L, 2L, 1L, NA),  # descending coded: key 3 = best
      missing_keys = "9",
      role = "factor_ordinal"
    ),
    SEXE = make_labels_vars(
      "SEXE",
      keys      = c("1", "2"),
      labels    = c("Femme", "Homme"),
      order_vec = c(2L, 1L, NA),      # key 2 = positive (Homme = order 1)
      missing_keys = character(0),
      role = "factor_binary"
    )[c("var_label", "role", "new_name", "levels")]
  )
  # Fix SEXE order_vec (only 2 keys, no missing)
  vars$SEXE <- list(
    var_label = "test", role = "factor_binary", new_name = "SEXE",
    levels = list(
      "1" = list(label = "Femme", order = 2L),
      "2" = list(label = "Homme", order = 1L)
    )
  )
  .write_meta_json(make_meta_list(vars), path)

  meta <- dplyr::bind_rows(
    make_labels_meta(
      "FREQ", "Fréquence", "factor_ordinal",
      values_vec    = c("1", "2", "3", "9"),
      labels_vec    = c("Jamais", "Parfois", "Toujours", "NSP"),
      missing_vec   = "9",
      new_labels_vec = c("Jamais", "Parfois", "Toujours", "NULL"),
      order_vec     = c(3L, 2L, 1L, NA_integer_)
    ),
    make_labels_meta(
      "SEXE", "Sexe", "factor_binary",
      values_vec    = c("1", "2"),
      labels_vec    = c("Femme", "Homme"),
      missing_vec   = character(0),
      new_labels_vec = c("Femme", "Homme"),
      order_vec     = c(2L, 1L)
    )
  )

  fake_resp <- paste0(
    '{"FREQ": {"3": "Toujours", "2": "Parfois", "1": "Jamais"},',
    ' "SEXE": {"1": "Femme", "2": "Homme"}}'
  )
  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude", mock_ai(fake_resp), envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)

  suppressMessages(ai_suggest_labels(meta, meta_json = path))

  raw <- .read_meta_json(path)

  # FREQ: descending order preserved
  freq <- raw$variables$FREQ$levels
  expect_equal(freq[["1"]]$order, 3L)
  expect_equal(freq[["2"]]$order, 2L)
  expect_equal(freq[["3"]]$order, 1L)
  expect_null(freq[["9"]]$order)
  expect_equal(freq[["1"]]$new_label, "Jamais")
  expect_equal(freq[["3"]]$new_label, "Toujours")

  # SEXE: reversed binary order preserved
  sexe <- raw$variables$SEXE$levels
  expect_equal(sexe[["1"]]$order, 2L)  # Femme = negative
  expect_equal(sexe[["2"]]$order, 1L)  # Homme = positive
  expect_equal(sexe[["1"]]$new_label, "Femme")
  expect_equal(sexe[["2"]]$new_label, "Homme")
})

# ---------------------------------------------------------------------------
# L8: AI omits one variable from response — other still labeled, missing unlabeled
# ---------------------------------------------------------------------------

test_that("L8: AI omits one variable — labeled variable written, omitted has no new_label", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()

  vars <- list(
    VAR_A = make_labels_vars(
      "VAR_A",
      keys      = c("1", "2"),
      labels    = c("Oui", "Non"),
      order_vec = c(1L, 2L),
      role = "factor_binary"
    ),
    VAR_B = make_labels_vars(
      "VAR_B",
      keys      = c("1", "2"),
      labels    = c("Oui", "Non"),
      order_vec = c(1L, 2L),
      role = "factor_binary"
    )
  )
  .write_meta_json(make_meta_list(vars), path)

  meta <- dplyr::bind_rows(
    make_labels_meta(
      "VAR_A", "A voté ?", "factor_binary",
      values_vec    = c("1", "2"),
      labels_vec    = c("Oui", "Non"),
      missing_vec   = character(0),
      new_labels_vec = c("Oui", "Non"),
      order_vec     = c(1L, 2L)
    ),
    make_labels_meta(
      "VAR_B", "A travaillé ?", "factor_binary",
      values_vec    = c("1", "2"),
      labels_vec    = c("Oui", "Non"),
      missing_vec   = character(0),
      new_labels_vec = c("Oui", "Non"),
      order_vec     = c(1L, 2L)
    )
  )

  # AI returns only VAR_A, omits VAR_B
  fake_resp <- '{"VAR_A": {"1": "A voté", "2": "Pas voté"}}'
  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude", mock_ai(fake_resp), envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)

  expect_message(
    ai_suggest_labels(meta, meta_json = path,
                      replace_existing_new_labels = TRUE),
    "VAR_B"
  )

  res <- .read_meta_json(path)$variables
  # VAR_A: labeled
  expect_equal(res$VAR_A$levels[["1"]]$new_label, "A voté")
  expect_equal(res$VAR_A$levels[["2"]]$new_label, "Pas voté")
  # VAR_B: absent from AI response → no new_label
  expect_null(res$VAR_B$levels[["1"]]$new_label)
  expect_null(res$VAR_B$levels[["2"]]$new_label)
})

# ---------------------------------------------------------------------------
# L9: replace_existing_new_labels = FALSE (default) — fully-labeled vars skipped
# ---------------------------------------------------------------------------

test_that("L9: replace_existing_new_labels=FALSE skips fully-labeled, processes partial", {
  withr::local_dir(.test_proj_root)
  path <- tmp_json()

  # VAR_FULL: already has new_label on all non-missing levels in JSON
  # VAR_PART: has no new_label → must be sent to AI
  vars <- list(
    VAR_FULL = list(
      var_label = "test", role = "factor_binary", new_name = "VAR_FULL",
      levels = list(
        "1" = list(label = "Oui", new_label = "Déjà fait", order = 1L),
        "2" = list(label = "Non", new_label = "Pas fait",  order = 2L)
      )
    ),
    VAR_PART = make_labels_vars(
      "VAR_PART",
      keys      = c("1", "2"),
      labels    = c("Oui", "Non"),
      order_vec = c(1L, 2L),
      role = "factor_binary"
    )
  )
  .write_meta_json(make_meta_list(vars), path)

  meta <- dplyr::bind_rows(
    make_labels_meta(
      "VAR_FULL", "Déjà fait ?", "factor_binary",
      values_vec    = c("1", "2"),
      labels_vec    = c("Oui", "Non"),
      missing_vec   = character(0),
      new_labels_vec = c("Déjà fait", "Pas fait"),
      order_vec     = c(1L, 2L)
    ),
    make_labels_meta(
      "VAR_PART", "A travaillé ?", "factor_binary",
      values_vec    = c("1", "2"),
      labels_vec    = c("Oui", "Non"),
      missing_vec   = character(0),
      new_labels_vec = c("Oui", "Non"),
      order_vec     = c(1L, 2L)
    )
  )

  fake_resp <- '{"VAR_PART": {"1": "A travaillé", "2": "Pas travaillé"}}'
  .orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude", mock_ai(fake_resp), envir = globalenv())
  on.exit(assign("ai_call_claude", .orig, envir = globalenv()), add = TRUE)

  # Default: replace_existing_new_labels = FALSE → VAR_FULL skipped
  suppressMessages(ai_suggest_labels(meta, meta_json = path))

  res <- .read_meta_json(path)$variables
  # VAR_FULL: existing labels must be untouched (we never re-sent it)
  expect_equal(res$VAR_FULL$levels[["1"]]$new_label, "Déjà fait")
  expect_equal(res$VAR_FULL$levels[["2"]]$new_label, "Pas fait")
  # VAR_PART: newly labeled
  expect_equal(res$VAR_PART$levels[["1"]]$new_label, "A travaillé")
  expect_equal(res$VAR_PART$levels[["2"]]$new_label, "Pas travaillé")
})
