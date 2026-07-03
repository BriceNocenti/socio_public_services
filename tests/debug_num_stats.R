source("R/data_formatting_pipeline.R", encoding = "UTF-8")
source("tests/testthat.R", encoding = "UTF-8")

# Test INT3 scenario: all numeric, no factors
json_path <- tempfile(fileext = ".json")
df_num <- tibble::tibble(
  AGE    = c(25L, 40L, 9999L, 30L),
  INCOME = c(1500.5, 2000.0, 9999.0, 1800.0)
)
suppressMessages(extract_survey_metadata(
  df_num, json_path,
  missing_num = c(9999L),
  missing_chr = character(0)
))

cat("=== JSON after extract_survey_metadata ===\n")
raw <- .read_meta_json(json_path)
cat("AGE role:", raw$variables$AGE$role, "\n")
cat("INCOME role:", raw$variables$INCOME$role, "\n")
cat("config$missing_num:", paste(raw$config$missing_num, collapse=", "), "\n")

suppressMessages(metadata_add_level_stats(json_path, df_num))

cat("\n=== JSON after metadata_add_level_stats ===\n")
raw2 <- .read_meta_json(json_path)
cat("AGE num_stats:", paste(names(raw2$variables$AGE$num_stats), collapse=", "), "\n")
cat("AGE$num_stats$max:", raw2$variables$AGE$num_stats$max, "\n")
cat("INCOME num_stats:", paste(names(raw2$variables$INCOME$num_stats), collapse=", "), "\n")

unlink(json_path)
