# Tests for renumber_binary_batteries() — data-level battery numbering.
# Function under test: renumber_binary_batteries (reuses .nines_sentinel /
# .gfs_numeric_prefix). No AI calls; inline fixtures only.


# --- local fixture helper --------------------------------------------------
# Build a 2-level factor with the positive pole as the FIRST level, optionally
# carrying a variable label + question_prefix attribute and/or an ordered class.
.rb_bin <- function(vals, levels = c("Oui", "Non"),
                    label = NULL, prefix = NULL, ordered = FALSE) {
  f <- factor(vals, levels = levels, ordered = ordered)
  if (!is.null(label))  attr(f, "label") <- label
  if (!is.null(prefix)) attr(f, "question_prefix") <- prefix
  f
}


# ---------------------------------------------------------------------------
# RB1. Numbering + sentinel width
# ---------------------------------------------------------------------------

test_that("RB1: 4-member binary battery -> 1..4 positives, shared 9- negative", {
  df <- data.frame(id = 1:4)
  df[["HANDI_VIS"]] <- .rb_bin(c("Oui", "Non", "Oui", "Non"))
  df[["HANDI_MOT"]] <- .rb_bin(c("Non", "Non", "Oui", "Oui"))
  df[["HANDI_PSY"]] <- .rb_bin(c("Oui", "Oui", "Non", "Non"))
  df[["HANDI_DEV"]] <- .rb_bin(c("Non", "Oui", "Oui", "Non"))

  out <- suppressMessages(renumber_binary_batteries(df, "HANDI_"))

  expect_equal(levels(out$HANDI_VIS), c("1-Oui", "9-Non"))
  expect_equal(levels(out$HANDI_MOT), c("2-Oui", "9-Non"))
  expect_equal(levels(out$HANDI_PSY), c("3-Oui", "9-Non"))
  expect_equal(levels(out$HANDI_DEV), c("4-Oui", "9-Non"))

  # Only level LABELS changed: integer codes (pole order) are untouched.
  expect_equal(as.integer(out$HANDI_VIS), as.integer(df$HANDI_VIS))
})

test_that("RB1b: 12-member battery uses width-2 sentinel (01-..12- / 99-)", {
  df <- data.frame(id = 1:3)
  nm <- sprintf("BAT_%02d", 1:12)
  for (v in nm) df[[v]] <- .rb_bin(c("Y", "N", "Y"), levels = c("Y", "N"))

  out <- suppressMessages(renumber_binary_batteries(df, "BAT_"))

  expect_equal(levels(out$BAT_01), c("01-Y", "99-N"))
  expect_equal(levels(out$BAT_12), c("12-Y", "99-N"))
})


# ---------------------------------------------------------------------------
# RB2. Attribute / class preservation
# ---------------------------------------------------------------------------

test_that("RB2: label, question_prefix, ordered class and unobserved level preserved", {
  df <- data.frame(id = 1:4)
  df[["REV_SAL"]] <- .rb_bin(c("Oui", "Non", "Oui", "Non"),
                             label = "Salaire", prefix = "Sources de revenus")
  df[["REV_RET"]] <- .rb_bin(c("Non", "Oui", "Non", "Oui"),
                             label = "Retraite", prefix = "Sources de revenus",
                             ordered = TRUE)
  # 2 declared levels but "Non" never observed
  df[["REV_RSA"]] <- .rb_bin(c("Oui", "Oui", "Oui", "Oui"), label = "RSA")

  # Guard: the fixture actually carries the attributes before renumbering.
  expect_equal(attr(df$REV_SAL, "label"), "Salaire")
  expect_equal(attr(df$REV_SAL, "question_prefix"), "Sources de revenus")

  out <- suppressMessages(renumber_binary_batteries(df, "REV_"))

  expect_equal(levels(out$REV_SAL), c("1-Oui", "9-Non"))
  expect_equal(levels(out$REV_RET), c("2-Oui", "9-Non"))
  expect_equal(levels(out$REV_RSA), c("3-Oui", "9-Non"))   # unobserved pole still relabeled

  expect_equal(attr(out$REV_SAL, "label"), "Salaire")
  expect_equal(attr(out$REV_SAL, "question_prefix"), "Sources de revenus")
  expect_equal(attr(out$REV_RET, "label"), "Retraite")
  expect_equal(attr(out$REV_RSA, "label"), "RSA")
  expect_true(is.ordered(out$REV_RET))
})


# ---------------------------------------------------------------------------
# RB3. Graceful skip of a non-binary group
# ---------------------------------------------------------------------------

test_that("RB3: a group with a non-binary member is skipped; other groups still done", {
  df <- data.frame(id = 1:4)
  df[["CHAUF_GAZ"]]  <- .rb_bin(c("Oui", "Non", "Oui", "Non"))
  df[["CHAUF_BOIS"]] <- .rb_bin(c("Non", "Oui", "Non", "Oui"))
  # non-binary member (3 levels) forces the whole REGIME_ group to be skipped
  df[["REGIME_VIAN"]] <- factor(c("Jamais", "Parfois", "Souvent", "Parfois"),
                                levels = c("Jamais", "Parfois", "Souvent"))
  df[["REGIME_BIO"]]  <- .rb_bin(c("Oui", "Non", "Oui", "Non"))

  expect_message(
    out <- renumber_binary_batteries(df, c("CHAUF_", "REGIME_")),
    "ignor"
  )

  # CHAUF_ renumbered
  expect_equal(levels(out$CHAUF_GAZ),  c("1-Oui", "9-Non"))
  expect_equal(levels(out$CHAUF_BOIS), c("2-Oui", "9-Non"))
  # REGIME_ left untouched
  expect_equal(levels(out$REGIME_VIAN), c("Jamais", "Parfois", "Souvent"))
  expect_equal(levels(out$REGIME_BIO),  c("Oui", "Non"))
})


# ---------------------------------------------------------------------------
# RB4. Overlap safety — longest prefix wins
# ---------------------------------------------------------------------------

test_that("RB4: VETACHAT does not swallow VETACHATLIEU (longest-prefix match)", {
  df <- data.frame(id = 1:4)
  df[["VETACHAT_PER"]]   <- .rb_bin(c("Oui", "Non", "Oui", "Non"))
  df[["VETACHAT_ADU"]]   <- .rb_bin(c("Non", "Oui", "Non", "Oui"))
  df[["VETACHAT_ENF"]]   <- .rb_bin(c("Oui", "Oui", "Non", "Non"))
  df[["VETACHATLIEU_1"]] <- .rb_bin(c("Oui", "Non", "Non", "Oui"))
  df[["VETACHATLIEU_2"]] <- .rb_bin(c("Non", "Non", "Oui", "Oui"))

  out <- suppressMessages(
    renumber_binary_batteries(df, c("VETACHAT", "VETACHATLIEU")))

  # VETACHAT group = its own 3 members
  expect_equal(levels(out$VETACHAT_PER), c("1-Oui", "9-Non"))
  expect_equal(levels(out$VETACHAT_ADU), c("2-Oui", "9-Non"))
  expect_equal(levels(out$VETACHAT_ENF), c("3-Oui", "9-Non"))
  # VETACHATLIEU is a SEPARATE group of 2 -> restarts at 1, not 4
  expect_equal(levels(out$VETACHATLIEU_1), c("1-Oui", "9-Non"))
  expect_equal(levels(out$VETACHATLIEU_2), c("2-Oui", "9-Non"))
})


# ---------------------------------------------------------------------------
# RB5. Idempotency + named prefixes / unmatched reporting
# ---------------------------------------------------------------------------

test_that("RB5: re-running is idempotent (no double prefix)", {
  df <- data.frame(id = 1:4)
  df[["HANDI_VIS"]] <- .rb_bin(c("Oui", "Non", "Oui", "Non"))
  df[["HANDI_MOT"]] <- .rb_bin(c("Non", "Oui", "Non", "Oui"))

  out1 <- suppressMessages(renumber_binary_batteries(df, "HANDI_"))
  out2 <- suppressMessages(renumber_binary_batteries(out1, "HANDI_"))

  expect_equal(levels(out2$HANDI_VIS), c("1-Oui", "9-Non"))
  expect_equal(levels(out2$HANDI_MOT), levels(out1$HANDI_MOT))
})

test_that("RB6: named prefixes use values; unmatched prefix reported", {
  df <- data.frame(id = 1:4)
  df[["HANDI_VIS"]] <- .rb_bin(c("Oui", "Non", "Oui", "Non"))
  df[["HANDI_MOT"]] <- .rb_bin(c("Non", "Oui", "Non", "Oui"))
  batteries <- c("Handicap" = "HANDI_", "Absent" = "ZZZ_")

  expect_message(
    out <- renumber_binary_batteries(df, batteries),
    "sans colonne correspondante"
  )
  expect_equal(levels(out$HANDI_VIS), c("1-Oui", "9-Non"))
  expect_equal(levels(out$HANDI_MOT), c("2-Oui", "9-Non"))
})

test_that("RB7: empty / no-prefix input returns df unchanged", {
  df <- data.frame(id = 1:4)
  df[["HANDI_VIS"]] <- .rb_bin(c("Oui", "Non", "Oui", "Non"))
  out <- suppressMessages(renumber_binary_batteries(df, character(0)))
  expect_equal(levels(out$HANDI_VIS), c("Oui", "Non"))
})
