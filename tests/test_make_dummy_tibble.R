## Quick smoke test for make_dummy_tibble()
source("R/data_formatting_pipeline.R", encoding = "UTF-8")

# Build a small test df with haven_labelled + factor + plain numeric columns
sterile_col <- structure(
  c(0, 0, 1, 0, 0, 1, NA, 0, 1, 0),
  label = "F: Stérile",
  format.spss = "F3.0",
  display_width = 3L,
  labels = c(Non = 0, Oui = 1),
  class = c("haven_labelled", "vctrs_vctr", "double")
)

freq_col <- structure(
  c(1, 2, 3, 9, 1, 2, NA, 3, 1, 2),
  label = "Fréquence",
  labels = c(Toujours = 1, Souvent = 2, Rarement = 3, NSP = 9),
  class = c("haven_labelled", "vctrs_vctr", "double")
)

age_col <- c(25, 30, 35, 40, 45, 50, 55, 60, 65, NA)

cat_col <- factor(c("A", "B", "C", "A", "B", "C", "A", NA, "B", "C"),
                  levels = c("A", "B", "C", "D"))

df_test <- tibble::tibble(
  STERILE = sterile_col,
  FREQ    = freq_col,
  AGE     = age_col,
  CAT     = cat_col
)

cat("=== Test 1: all columns, seed = 42 ===\n")
res <- make_dummy_tibble(df_test, seed = 42)

cat("\n=== Verify attributes ===\n")
cat("STERILE class:", paste(class(res$STERILE), collapse = ", "), "\n")
cat("STERILE label:", attr(res$STERILE, "label"), "\n")
cat("STERILE val_labels:", paste(names(labelled::val_labels(res$STERILE)),
                                  collapse = ", "), "\n")

cat("FREQ class:", paste(class(res$FREQ), collapse = ", "), "\n")
cat("FREQ label:", attr(res$FREQ, "label"), "\n")

cat("CAT class:", paste(class(res$CAT), collapse = ", "), "\n")
cat("CAT levels:", paste(levels(res$CAT), collapse = ", "), "\n")

cat("AGE class:", paste(class(res$AGE), collapse = ", "), "\n")

cat("\nRows:", nrow(res), "\n")
cat("NAs per column:\n")
print(colSums(is.na(res)))

cat("\n=== Test 2: subset cols ===\n")
res2 <- make_dummy_tibble(df_test, cols = c("STERILE", "AGE"), seed = 42)
cat("Columns:", paste(names(res2), collapse = ", "), "\n")
cat("Rows:", nrow(res2), "\n")

cat("\n=== Test 3: roundtrip dput ===\n")
dput_out <- capture.output(dput(res))
roundtrip <- eval(parse(text = paste(dput_out, collapse = "\n")))
cat("Roundtrip identical classes:", identical(
  lapply(res, class), lapply(roundtrip, class)
), "\n")

cat("\n=== Test 4: clipboard = FALSE (no error) ===\n")
res4 <- make_dummy_tibble(df_test, cols = "STERILE", seed = 1, clipboard = FALSE)

cat("\nAll tests passed.\n")
