# Tests for the deterministic battery-candidate seed (internal, feeds
# ai_build_outline) + preview_outline() + check_batteries(). Functions under test:
#   .batt_seed_candidates, .batt_signature, .batt_common_token_prefix,
#   .batt_provisional_title, .batt_strip_common, .batt_precision_ok,
#   preview_outline, check_batteries
# Prefix: D

# Build a JSON with the given variables and return its path.
d_json <- function(vars, n_individuals = 100L) {
  ml <- make_meta_list(vars)
  ml$config$n_individuals <- n_individuals
  path <- tmp_json()
  .write_meta_json(ml, path)
  path
}

# yes/no binary levels
d_lv2 <- function() list(
  "0" = list(order = 2L, label = "Non", n = 40L, pct = 40L),
  "1" = list(order = 1L, label = "Oui", n = 60L, pct = 60L))
# 3-level nominal
d_lv3 <- function() list(
  "0" = list(order = 1L, label = "A", n = 30L, pct = 30L),
  "1" = list(order = 2L, label = "B", n = 40L, pct = 40L),
  "2" = list(order = 3L, label = "C", n = 30L, pct = 30L))

d_bin  <- function(lbl, nm, batt = NULL) c(
  list(var_label = lbl, role = "factor_binary", r_class = "double",
       new_name = nm, levels = d_lv2()),
  if (!is.null(batt)) list(battery = batt) else list())
d_nom3 <- function(lbl, nm) list(var_label = lbl, role = "factor_nominal",
  r_class = "double", new_name = nm, levels = d_lv3())
d_cnt  <- function(lbl, nm) list(var_label = lbl, role = "integer_count",
  r_class = "numeric", new_name = nm, levels = list())

# Deterministic seed titles, named by variable (in order).
seed_of <- function(path) {
  m  <- .read_meta_json(path)
  sr <- .batt_seed_candidates(m)
  stats::setNames(sr$seed, names(m$variables))
}
outliers_of <- function(path) .batt_seed_candidates(.read_meta_json(path))$outliers


# ---------------------------------------------------------------------------
# D1. Same-signature contiguous run, split by first name-token into clusters
# ---------------------------------------------------------------------------

test_that("D1: UNIV_/FAM_ clusters (>=3 each) seed as separate candidates", {
  vars <- list(
    UNIV_A = d_bin("Aquatique 12 mois", "UNIV_A"),
    UNIV_B = d_bin("Collectifs 12 mois", "UNIV_B"),
    UNIV_C = d_bin("Marche 12 mois", "UNIV_C"),
    FAM_X  = d_bin("Athletisme 12 mois", "FAM_X"),
    FAM_Y  = d_bin("Basket 12 mois", "FAM_Y"),
    FAM_Z  = d_bin("Foot 12 mois", "FAM_Z")
  )
  sd <- seed_of(d_json(vars))
  expect_equal(length(unique(sd[c("UNIV_A","UNIV_B","UNIV_C")])), 1L)
  expect_equal(length(unique(sd[c("FAM_X","FAM_Y","FAM_Z")])), 1L)
  expect_true(nzchar(sd["UNIV_A"]))
  expect_false(identical(unname(sd["UNIV_A"]), unname(sd["FAM_X"])))
})


# ---------------------------------------------------------------------------
# D2. No name prefix but a shared label stem -> one candidate (precision gate)
# ---------------------------------------------------------------------------

test_that("D2: a no-prefix run with a common label stem is one candidate", {
  vars <- list(
    LONE  = d_cnt("Nombre total", "LONE"),
    OBJ   = d_nom3("Utilisation d'un objet connecte", "OBJ"),
    APP   = d_nom3("Utilisation d'une application", "APP"),
    RESO  = d_nom3("Utilisation des reseaux sociaux", "RESO")
  )
  sd <- seed_of(d_json(vars))
  expect_true(nzchar(sd["OBJ"]))                       # shared "Utilisation d" stem
  expect_equal(length(unique(sd[c("OBJ","APP","RESO")])), 1L)
  expect_equal(unname(sd["LONE"]), "")                 # standalone count
})


# ---------------------------------------------------------------------------
# D3. Type-outlier healed into the candidate run AND reported as an outlier
# ---------------------------------------------------------------------------

test_that("D3: a mis-typed single-level variable stays in its run + is flagged", {
  vars <- list(
    PAP_A   = d_bin("Tennis 12 mois", "PAP_A"),
    PAP_ODD = list(var_label = "Autre glisse 12 mois", role = "factor_nominal",
                   r_class = "double", new_name = "PAP_ODD",
                   levels = list("0" = list(order = 1L, label = "Non", n = 100L, pct = 100L))),
    PAP_B   = d_bin("Judo 12 mois", "PAP_B")
  )
  path <- d_json(vars)
  sd <- seed_of(path)
  expect_equal(unname(sd["PAP_A"]), unname(sd["PAP_ODD"]))
  expect_equal(unname(sd["PAP_A"]), unname(sd["PAP_B"]))
  expect_true(2L %in% outliers_of(path))               # PAP_ODD (index 2) flagged
})


# ---------------------------------------------------------------------------
# D4. Interleaved binary/count is NOT seeded — left for the AI to discover
# ---------------------------------------------------------------------------

test_that("D4: period-2 binary/count alternation is not seeded", {
  vars <- list(
    PRAT_A = d_bin("APS en ville", "PRAT_A"),
    NB_A   = d_cnt("Nombre APS ville", "NB_A"),
    PRAT_B = d_bin("APS domicile", "PRAT_B"),
    NB_B   = d_cnt("Nombre APS domicile", "NB_B")
  )
  sd <- seed_of(d_json(vars))
  expect_true(all(sd == ""))    # no same-signature run of >= 3; AI handles it
})


# ---------------------------------------------------------------------------
# D5. Outline headers are section boundaries a candidate run cannot cross
# ---------------------------------------------------------------------------

test_that("D5: a variable carrying a `headers` entry breaks the run before it", {
  vars <- list(
    Q_A = d_bin("q un", "Q_A"),
    Q_B = d_bin("q deux", "Q_B"),
    Q_C = d_bin("q trois", "Q_C"),
    Q_D = c(d_bin("q quatre", "Q_D"), list(headers = list("## Nouvelle partie")))
  )
  sd <- seed_of(d_json(vars))
  expect_equal(length(unique(sd[c("Q_A","Q_B","Q_C")])), 1L)   # one candidate
  expect_true(nzchar(sd["Q_A"]))
  expect_equal(unname(sd["Q_D"]), "")                          # new section
})


# ---------------------------------------------------------------------------
# D6. min_size + precision gate reject runs that are not batteries
# ---------------------------------------------------------------------------

test_that("D6: a two-variable run is not seeded (min_size = 3 default)", {
  vars <- list(DUP_A = d_bin("q1", "DUP_A"), DUP_B = d_bin("q2", "DUP_B"),
               OTHER = d_nom3("x", "OTHER"))
  sd <- seed_of(d_json(vars))
  expect_equal(unname(sd["DUP_A"]), "")
  expect_equal(unname(sd["DUP_B"]), "")
})

test_that("D6b: same signature but no shared name/label -> precision gate drops it", {
  # Three unrelated yes/no questions in a row: same role + codes, but no common
  # name prefix and no >=10-char label stem -> NOT a battery candidate.
  vars <- list(
    SEXE_BIN = d_bin("Etes-vous une femme", "SEXE_BIN"),
    FUMEUR   = d_bin("Fumez-vous", "FUMEUR"),
    VOTE     = d_bin("Avez-vous vote", "VOTE")
  )
  sd <- seed_of(d_json(vars))
  expect_true(all(sd == ""))
})


# ---------------------------------------------------------------------------
# D7. Signature + prefix helpers
# ---------------------------------------------------------------------------

test_that("D7: signature separates roles / level sets; token prefix is the first shared token", {
  s_bin2 <- .batt_signature(list(role = "factor_binary", levels = d_lv2()))
  s_nom3 <- .batt_signature(list(role = "factor_nominal", levels = d_lv3()))
  s_cnt  <- .batt_signature(list(role = "integer_count", levels = list()))
  expect_false(identical(s_bin2, s_nom3))
  expect_true(startsWith(s_cnt, "N|"))
  expect_true(startsWith(.batt_signature(list(role = "identifier")), "X|"))
  expect_equal(.batt_common_token_prefix(c("UNIV_SP_A", "UNIV_SP_B")), "UNIV_SP")
  expect_equal(.batt_common_token_prefix(c("UNIV_A", "FAM_B")), "")
})


# ---------------------------------------------------------------------------
# D8. preview_outline() markdown output
# ---------------------------------------------------------------------------

test_that("D8: preview_outline prints outline headers + battery members (common part stripped)", {
  vars <- list(
    HEAD   = c(d_nom3("Statut", "HEAD"), list(headers = list("## Bloc B", "### Univers"))),
    UNIV_A = d_bin("Activites aquatiques, au moins une fois sur 12 mois", "UNIV_A", batt = "Univers"),
    UNIV_B = d_bin("Sports collectifs, au moins une fois sur 12 mois", "UNIV_B", batt = "Univers"),
    UNIV_C = d_bin("Sports de combat, au moins une fois sur 12 mois", "UNIV_C", batt = "Univers")
  )
  path <- d_json(vars)
  md <- suppressMessages(preview_outline(path))
  expect_match(md, "## Bloc B", fixed = TRUE)
  expect_match(md, "### Univers", fixed = TRUE)
  expect_match(md, "#### Univers", fixed = TRUE)          # battery header
  expect_match(md, "aquatiques", fixed = TRUE)            # distinctive part kept
  expect_false(grepl("au moins une fois sur 12 mois", md))
})

test_that("D8b: .batt_strip_common removes common prefix AND suffix", {
  x <- c("Au moins une APS pratiquee en ville, sur 12 mois",
         "Au moins une APS pratiquee au domicile, sur 12 mois")
  out <- .batt_strip_common(x)
  expect_equal(out, c("en ville", "au domicile"))
})


# ---------------------------------------------------------------------------
# D9. check_batteries(): interleaved batteries -> reorder + relocate() strings
# ---------------------------------------------------------------------------

test_that("D9: two interleaved same-signature groups are flagged with relocate()", {
  vars <- list(
    PRAT_A = d_bin("APS en ville", "PRAT_A"),
    NB_A   = d_cnt("Nb ville", "NB_A"),
    PRAT_B = d_bin("APS a domicile", "PRAT_B"),
    NB_B   = d_cnt("Nb domicile", "NB_B"),
    PRAT_C = d_bin("APS au travail", "PRAT_C"),
    NB_C   = d_cnt("Nb travail", "NB_C")
  )
  res    <- suppressMessages(check_batteries(d_json(vars)))
  relocs <- vapply(res$reorder, function(r) r$relocate, character(1))
  expect_equal(length(res$reorder), 2L)                       # binary + count groups
  expect_true('relocate(all_of(c("PRAT_B", "PRAT_C")), .after = "PRAT_A")' %in% relocs)
  expect_true('relocate(all_of(c("NB_B", "NB_C")), .after = "NB_A")' %in% relocs)
})


# ---------------------------------------------------------------------------
# D10. check_batteries(): a mis-typed member (LIVRE-like) is flagged, no prefix
# ---------------------------------------------------------------------------

test_that("D10: a wrong role between two same-question neighbours is an outlier", {
  vars <- list(
    CINEMA  = d_bin("Aller au cinema au cours des 4 dernieres semaines", "CINEMA"),
    LIVRE   = d_cnt("Lire un livre au cours des 4 dernieres semaines", "LIVRE"),
    THEATRE = d_bin("Aller au theatre au cours des 4 dernieres semaines", "THEATRE")
  )
  res <- suppressMessages(check_batteries(d_json(vars)))
  expect_true("LIVRE" %in% res$outliers)                      # shared label stem
  expect_equal(length(res$reorder), 0L)                       # 2-var groups, no reorder
})


# ---------------------------------------------------------------------------
# D11. check_batteries(): a clean contiguous battery flags nothing
# ---------------------------------------------------------------------------

test_that("D11: a clean contiguous same-signature battery is not flagged", {
  vars <- list(
    LIC_A = d_bin("Licence federale", "LIC_A"),
    LIC_B = d_bin("Licence scolaire", "LIC_B"),
    LIC_C = d_bin("Licence autre", "LIC_C")
  )
  res <- suppressMessages(check_batteries(d_json(vars)))
  expect_equal(length(res$reorder), 0L)
  expect_equal(length(res$outliers), 0L)
})
