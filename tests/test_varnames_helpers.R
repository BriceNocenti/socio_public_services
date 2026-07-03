# Tests for ai_suggest_varnames() helpers — no API cost
# Run with: source("R/test_varnames_helpers.R")

source("R/data_formatting_pipeline.R")

ok  <- 0L
err <- 0L
chk <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) e)
  if (isTRUE(result)) {
    message("  OK  ", label)
    ok  <<- ok  + 1L
  } else {
    message("FAIL  ", label, if (inherits(result, "error")) paste0(" — ", result$message))
    err <<- err + 1L
  }
}

message("=== .parse_varnames_json_responses ===")

# 1. Pure JSON response (no markdown)
r1 <- .parse_varnames_json_responses(
  list('{"VAR_A": "NEW_A", "VAR_B": "NEW_B"}'),
  c("VAR_A", "VAR_B")
)
chk("pure JSON parsed", identical(r1, c(VAR_A = "NEW_A", VAR_B = "NEW_B")))

# 2. Response wrapped in markdown code fence
r2 <- .parse_varnames_json_responses(
  list('```json\n{"VAR_A": "NEW_A", "VAR_B": "NEW_B"}\n```'),
  c("VAR_A", "VAR_B")
)
chk("markdown fence stripped", identical(r2, c(VAR_A = "NEW_A", VAR_B = "NEW_B")))

# 3. Response with surrounding text (e.g. Haiku adds explanation)
r3 <- .parse_varnames_json_responses(
  list('Here are the suggested names:\n{"VAR_X": "RESULT_X"}\nHope that helps!'),
  c("VAR_X")
)
chk("surrounding text ignored", identical(r3, c(VAR_X = "RESULT_X")))

# 4. Unknown variable names filtered out
r4 <- .parse_varnames_json_responses(
  list('{"VAR_KNOWN": "GOOD", "VAR_UNKNOWN": "BAD"}'),
  c("VAR_KNOWN")
)
chk("unknown vars filtered", identical(r4, c(VAR_KNOWN = "GOOD")))

# 5. Duplicate new names get suffix _2, _3
r5 <- .parse_varnames_json_responses(
  list('{"A": "SAME", "B": "SAME", "C": "SAME"}'),
  c("A", "B", "C")
)
chk("duplicate dedup _2 _3", identical(as.character(r5), c("SAME", "SAME_2", "SAME_3")))

# 6. Empty / null response returns character(0)
r6 <- .parse_varnames_json_responses(list("", NULL), c("X"))
chk("empty response = character(0)", identical(r6, character(0)))

# 7. Large flat JSON (370 dummy variables)
big_vars <- paste0("V", seq_len(370))
big_json <- paste0(
  "{",
  paste(paste0('"', big_vars, '": "NEW_', big_vars, '"'), collapse = ", "),
  "}"
)
r7 <- .parse_varnames_json_responses(list(big_json), big_vars)
chk("large 370-var JSON parsed", length(r7) == 370 && r7[["V1"]] == "NEW_V1")

# 8. Truncated JSON (as received from API when max_tokens hit mid-stream)
# The real Haiku output ends like: ..."insee_NUTS  (no closing })
trunc_json <- paste0(
  '```json\n{\n',
  '  "VAR_A": "NEW_A",\n',
  '  "VAR_B": "NEW_B",\n',
  '  "VAR_C": "NEW_C'  # truncated mid-value — only A and B are complete pairs
)
# Should warn and recover A and B
r8 <- withCallingHandlers(
  .parse_varnames_json_responses(list(trunc_json), c("VAR_A", "VAR_B", "VAR_C")),
  warning = function(w) invokeRestart("muffleWarning")
)
chk("truncated: recovers complete pairs A+B", "VAR_A" %in% names(r8) && "VAR_B" %in% names(r8))
chk("truncated: no partial pair C",           !"VAR_C" %in% names(r8))

# 9. Truncated JSON from actual Haiku response pattern (fence + cut-off)
real_trunc <- paste0(
  '```json\n{\n',
  paste(paste0('  "', big_vars[1:200], '": "NEW_', big_vars[1:200], '"'), collapse = ",\n"),
  ',\n  "V201": "NEW_V20'  # truncated
)
r9 <- withCallingHandlers(
  .parse_varnames_json_responses(list(real_trunc), big_vars),
  warning = function(w) invokeRestart("muffleWarning")
)
chk("truncated real: recovers 200 complete pairs", length(r9) >= 200 && r9[["V1"]] == "NEW_V1")

message("\n=== .build_varnames_map ===")

target_df <- tibble::tibble(
  var_name     = c("V1", "V2"),
  new_labels   = list(c("Oui", "Non"), c("A", "B", "NULL")),
  level_counts = list(c(100L, 200L), c(50L, 60L, 10L)),
  level_freqs  = list(c(33L, 67L), c(45L, 55L, NA_integer_))
)
names_map <- c(V1 = "OUI_NON", V2 = "CHOICE")
m <- .build_varnames_map(target_df, names_map)

chk("build: new_name present",  m[["V1"]][["new_name"]] == "OUI_NON")
chk("build: new_labels present", identical(m[["V1"]][["new_labels"]], as.list(c("Oui", "Non"))))
chk("build: level_counts present", identical(m[["V2"]][["level_counts"]], as.list(c(50L, 60L, 10L))))
chk("build: level_freqs NA kept", is.na(m[["V2"]][["level_freqs"]][[3]]))

message("\n=== .write_varnames_json + metadata_apply_varnames_json roundtrip ===")

tmp <- tempfile(fileext = ".json")

# Stub entry (no new_name) + full entry
test_map <- list(
  ORIG_A = list(new_name = "CLEAN_A",
                new_labels   = list("Cat1", "Cat2"),
                level_counts = list(10L, 20L),
                level_freqs  = list(33L, 67L)),
  ORIG_B = list()   # stub
)
.write_varnames_json(test_map, tmp)
chk("file written", file.exists(tmp))

raw_back <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)
chk("roundtrip new_name",   raw_back[["ORIG_A"]][["new_name"]] == "CLEAN_A")
chk("roundtrip new_labels", identical(unlist(raw_back[["ORIG_A"]][["new_labels"]]),
                                       c("Cat1", "Cat2")))
chk("roundtrip stub empty", length(raw_back[["ORIG_B"]]) == 0)

# metadata_apply_varnames_json
meta_test <- tibble::tibble(
  var_name     = c("ORIG_A", "ORIG_B"),
  new_name     = c("ORIG_A", "ORIG_B"),
  new_labels   = list(c("Old1", "Old2"), character(0)),
  level_counts = list(c(1L, 2L), integer(0)),
  level_freqs  = list(c(30L, 70L), integer(0))
)
meta_updated <- metadata_apply_varnames_json(meta_test, tmp)
chk("apply: new_name updated",   meta_updated$new_name[[1]] == "CLEAN_A")
chk("apply: ORIG_B unchanged",   meta_updated$new_name[[2]] == "ORIG_B")
chk("apply: new_labels restored", identical(meta_updated$new_labels[[1]], c("Cat1", "Cat2")))
chk("apply: level_counts restored", identical(meta_updated$level_counts[[1]], c(10L, 20L)))

unlink(tmp)

message("\n=== Summary ===")
message("Passed: ", ok, " / ", ok + err)
if (err > 0) stop(err, " test(s) FAILED")
