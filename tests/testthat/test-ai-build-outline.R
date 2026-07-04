# Tests for ai_build_outline() — AI outline (### subthemes + #### groups), mocked.
# Functions under test: ai_build_outline, .parse_outline_spans,
#   .extract_outline_objects, .build_outline_system_prompt, .hdr_level
# Prefix: OU

ou_lv2 <- function() list(
  "0" = list(order = 2L, label = "Non", n = 40L, pct = 40L),
  "1" = list(order = 1L, label = "Oui", n = 60L, pct = 60L))
ou_lv3 <- function() list(
  "0" = list(order = 1L, label = "A", n = 30L, pct = 30L),
  "1" = list(order = 2L, label = "B", n = 40L, pct = 40L),
  "2" = list(order = 3L, label = "C", n = 30L, pct = 30L))
ou_bin <- function(lbl, nm, headers = NULL) c(
  list(var_label = lbl, role = "factor_binary", r_class = "double",
       new_name = nm, levels = ou_lv2()),
  if (!is.null(headers)) list(headers = as.list(headers)) else list())
ou_nom <- function(lbl, nm, headers = NULL) c(
  list(var_label = lbl, role = "factor_nominal", r_class = "double",
       new_name = nm, levels = ou_lv3()),
  if (!is.null(headers)) list(headers = as.list(headers)) else list())

ou_json <- function(vars) {
  ml <- make_meta_list(vars); ml$config$n_individuals <- 100L
  path <- tmp_json(); .write_meta_json(ml, path); path
}
ou_batt <- function(path) {
  bk <- .read_meta_json(path)
  vapply(bk$variables, function(v) v$battery %||% "", character(1))
}
ou_hdr <- function(path, v)
  as.character(.read_meta_json(path)$variables[[v]]$headers %||% character(0))

# assign a fake ai_call_claude returning `text`; restore on exit of a test.
with_mock_ai <- function(text, code) {
  orig <- get("ai_call_claude", envir = globalenv())
  assign("ai_call_claude", mock_ai(text), envir = globalenv())
  on.exit(assign("ai_call_claude", orig, envir = globalenv()), add = TRUE)
  force(code)
}

# One ## bloc anchor on HEAD; two seed-able prefix clusters; a standalone.
ou_vars <- function() list(
  HEAD   = ou_nom("Statut", "HEAD", headers = "## Bloc B"),
  UNIV_A = ou_bin("Aquatique", "UNIV_A"),
  UNIV_B = ou_bin("Collectifs", "UNIV_B"),
  UNIV_C = ou_bin("Marche", "UNIV_C"),
  FAM_X  = ou_bin("Athletisme", "FAM_X"),
  FAM_Y  = ou_bin("Basket", "FAM_Y"),
  FAM_Z  = ou_bin("Foot", "FAM_Z"),
  WEIGHT = list(var_label = "Poids", role = "double", r_class = "numeric",
                new_name = "WEIGHT", levels = list())
)


# ---------------------------------------------------------------------------
# OU1. dry_run: interleaved anchors + seed + survey_description, no write
# ---------------------------------------------------------------------------

test_that("OU1: dry_run builds the input with ## anchors, seeds + config survey_description", {
  path <- ou_json(ou_vars())
  # survey_description lives in config (set at extract), read here — not an argument.
  m <- .read_meta_json(path); m$config$survey_description <- "Une enquete de test"
  .write_meta_json(m, path)
  before <- ou_batt(path)
  res <- suppressWarnings(suppressMessages(ai_build_outline(path, dry_run = TRUE)))
  expect_type(res, "list")
  expect_match(res$user, '"var":"UNIV_A"', fixed = TRUE)
  expect_match(res$user, '"section":"## Bloc B"', fixed = TRUE)  # anchor interleaved
  expect_match(res$user, '"batt":null', fixed = TRUE)           # WEIGHT: no seed
  expect_true(grepl('"batt":"UNIV', res$user))                  # UNIV_* seeded
  expect_match(res$user, "SURVEY DESCRIPTION", fixed = TRUE)
  expect_match(res$user, "Une enquete de test", fixed = TRUE)
  expect_equal(ou_batt(path), before)                           # nothing written
})

test_that("OU1b: seed = FALSE sends batt null everywhere", {
  path <- ou_json(ou_vars())
  res  <- suppressWarnings(suppressMessages(
    ai_build_outline(path, seed = FALSE, dry_run = TRUE)))
  expect_match(res$user, '"batt":null', fixed = TRUE)
  expect_false(grepl('"batt":"UNIV', res$user))
})


# ---------------------------------------------------------------------------
# OU2. leveled spans -> ### / #### headers + battery field
# ---------------------------------------------------------------------------

test_that("OU2: ### to headers, #### battery to battery field, #### group to headers", {
  path <- ou_json(ou_vars())
  resp <- paste0(
    '[{"level":3,"title":"Univers","from":"UNIV_A","to":"UNIV_C"},',
    '{"level":4,"title":"Univers de pratique","from":"UNIV_A","to":"UNIV_C","battery":true},',
    '{"level":4,"title":"Familles","from":"FAM_X","to":"FAM_Z","battery":false}]')
  with_mock_ai(resp, suppressWarnings(suppressMessages(ai_build_outline(path))))
  bt <- ou_batt(path)
  expect_equal(unname(bt["UNIV_A"]), "Univers de pratique")   # battery repeated
  expect_equal(unname(bt["UNIV_C"]), "Univers de pratique")
  expect_equal(unname(bt["FAM_X"]),  "")                      # group is NOT a battery
  expect_true("### Univers"  %in% ou_hdr(path, "UNIV_A"))     # subtheme -> headers
  expect_true("#### Familles" %in% ou_hdr(path, "FAM_X"))     # group -> headers (start only)
  expect_equal(ou_hdr(path, "FAM_Y"), character(0))           # group not repeated
  expect_equal(ou_hdr(path, "HEAD"), "## Bloc B")             # ## anchor untouched
})

test_that("OU2c: a battery below min_size is demoted to a group; small groups kept", {
  path <- ou_json(ou_vars())
  resp <- paste0(
    '[{"level":4,"title":"Petit","from":"UNIV_A","to":"UNIV_B","battery":true},',  # 2 vars -> demote
    '{"level":4,"title":"Duo","from":"FAM_X","to":"FAM_Y","battery":false}]')       # 2-var group kept
  msg <- with_mock_ai(resp,
    capture_messages(suppressWarnings(ai_build_outline(path))))
  # demoted battery -> a #### header (headers), NOT the boxed battery field
  expect_equal(unname(ou_batt(path)["UNIV_A"]), "")
  expect_true("#### Petit" %in% ou_hdr(path, "UNIV_A"))
  # a small thematic group is kept (no minimum size for groups)
  expect_true("#### Duo" %in% ou_hdr(path, "FAM_X"))
  expect_true(any(grepl("demoted", msg)))
})


# ---------------------------------------------------------------------------
# OU3. a #### that crosses a ### boundary is rejected
# ---------------------------------------------------------------------------

test_that("OU3: a #### crossing a ### boundary is rejected", {
  path <- ou_json(ou_vars())
  resp <- paste0(
    '[{"level":3,"title":"Sub1","from":"UNIV_A","to":"UNIV_C"},',
    '{"level":3,"title":"Sub2","from":"FAM_X","to":"FAM_Z"},',
    '{"level":4,"title":"Cross","from":"UNIV_B","to":"FAM_Y","battery":true}]')
  msg <- with_mock_ai(resp,
    capture_messages(suppressWarnings(ai_build_outline(path))))
  expect_true("### Sub1" %in% ou_hdr(path, "UNIV_A"))
  expect_true("### Sub2" %in% ou_hdr(path, "FAM_X"))
  expect_equal(unname(ou_batt(path)["UNIV_B"]), "")           # Cross not applied
  expect_true(any(grepl("crosses ### boundary", msg)))
})


# ---------------------------------------------------------------------------
# OU4. a span that crosses a ## bloc boundary is rejected
# ---------------------------------------------------------------------------

test_that("OU4: a ### crossing a ## bloc boundary is rejected", {
  vars <- ou_vars()
  vars$FAM_X <- ou_bin("Athletisme", "FAM_X", headers = "## Bloc C")  # 2nd anchor
  path <- ou_json(vars)
  resp <- '[{"level":3,"title":"Straddle","from":"UNIV_A","to":"FAM_Z"}]'  # crosses ##
  msg <- with_mock_ai(resp,
    capture_messages(suppressWarnings(ai_build_outline(path))))
  expect_false("### Straddle" %in% ou_hdr(path, "UNIV_A"))
  expect_true(any(grepl("crosses ## boundary", msg)))
  expect_equal(ou_hdr(path, "FAM_X"), "## Bloc C")            # anchor preserved
})


# ---------------------------------------------------------------------------
# OU5. guard: unparseable response leaves the JSON untouched
# ---------------------------------------------------------------------------

test_that("OU5: zero valid spans -> meta_json left unchanged (no wipe)", {
  path <- ou_json(ou_vars())
  before_b <- ou_batt(path); before_h <- ou_hdr(path, "HEAD")
  with_mock_ai("sorry, I cannot help",
    suppressWarnings(suppressMessages(ai_build_outline(path))))
  expect_equal(ou_batt(path), before_b)
  expect_equal(ou_hdr(path, "HEAD"), before_h)               # ## anchor intact
})


# ---------------------------------------------------------------------------
# OU6. parser: fences, truncation, level/battery defaults
# ---------------------------------------------------------------------------

test_that("OU6: .parse_outline_spans survives fences + truncation", {
  txt <- paste0(
    "```json\n[{\"level\":4,\"title\":\"A\",\"from\":\"X\",\"to\":\"Y\",\"battery\":true},",
    "{\"level\":3,\"title\":\"B\",\"from\":\"Z\",\"to\":")          # truncated tail
  spans <- .parse_outline_spans(list(txt))
  expect_equal(length(spans), 1L)
  expect_equal(spans[[1]]$level, 4L)
  expect_true(spans[[1]]$battery)
})

test_that("OU6b: level defaults to 4; battery defaults to TRUE; false honored", {
  spans <- .parse_outline_spans(list(paste0(
    '[{"title":"G","from":"X","to":"Y","battery":false},',   # explicit group
    '{"title":"H","from":"A","to":"B"}]')))                  # no level, no battery
  expect_equal(length(spans), 2L)
  expect_false(spans[[1]]$battery)
  expect_equal(spans[[2]]$level, 4L)
  expect_true(spans[[2]]$battery)
})


# ---------------------------------------------------------------------------
# OU7. subthemes = FALSE: AI owns only ####; existing ### kept as anchor
# ---------------------------------------------------------------------------

test_that("OU7: subthemes = FALSE keeps ### anchors, applies only ####", {
  vars <- ou_vars()
  vars$UNIV_A <- ou_bin("Aquatique", "UNIV_A", headers = "### Univers")  # ### anchor
  path <- ou_json(vars)
  resp <- paste0(
    '[{"level":3,"title":"IgnoredSub","from":"FAM_X","to":"FAM_Z"},',
    '{"level":4,"title":"Bat","from":"UNIV_A","to":"UNIV_C","battery":true}]')
  with_mock_ai(resp,
    suppressWarnings(suppressMessages(ai_build_outline(path, subthemes = FALSE))))
  expect_true("### Univers" %in% ou_hdr(path, "UNIV_A"))       # anchor kept
  expect_equal(unname(ou_batt(path)["UNIV_A"]), "Bat")         # #### applied
  expect_false("### IgnoredSub" %in% ou_hdr(path, "FAM_X"))    # level-3 not owned
})


# ---------------------------------------------------------------------------
# OU8. system prompt file loads with the leveled schema + real examples
# ---------------------------------------------------------------------------

test_that("OU8: outline prompt file has the leveled schema + real examples", {
  f <- file.path(.test_proj_root, "instructions", "outline_prompt.md")
  skip_if_not(file.exists(f))
  sp <- paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  expect_match(sp, '"level"', fixed = TRUE)
  expect_match(sp, '"battery"', fixed = TRUE)
  expect_match(sp, "UNIV_AQUA_NAUT", fixed = TRUE)   # real pps20 example embedded
})
