# PURPOSE: Verify the Sonnet-5-vs-Haiku request-body branching and response handling.
# Prefix: MC. Pure-helper tests — no API calls, no mocking needed.
# See: CLAUDE.md > AI Integration.

test_that("MC1: .is_reasoning_tier_model detects the Sonnet-5/Opus-4.8 family", {
  expect_true(.is_reasoning_tier_model("claude-sonnet-5"))
  expect_true(.is_reasoning_tier_model("claude-opus-4-8"))
  expect_true(.is_reasoning_tier_model("claude-opus-4-7"))
  expect_true(.is_reasoning_tier_model("claude-opus-4-6"))
  expect_true(.is_reasoning_tier_model("claude-sonnet-4-6"))
  expect_false(.is_reasoning_tier_model("claude-haiku-4-5"))
  expect_false(.is_reasoning_tier_model("claude-sonnet-4-5"))
  expect_false(.is_reasoning_tier_model("claude-opus-4-5"))
})

test_that("MC2: .build_message_body enables adaptive thinking + effort for reasoning-tier models", {
  body <- .build_message_body("claude-sonnet-5", "hi", 4096L)
  expect_identical(body$model, "claude-sonnet-5")
  expect_identical(body$thinking$type, "adaptive")
  expect_identical(body$output_config$effort, "low")
  expect_identical(body$max_tokens, 4096L + .AI_THINKING_HEADROOM)
  expect_identical(body$messages[[1]]$content, "hi")
})

test_that("MC3: .build_message_body honors a custom effort and clamps to the 128K ceiling", {
  body <- .build_message_body("claude-opus-4-8", "hi", 130000L, effort = "medium")
  expect_identical(body$output_config$effort, "medium")
  expect_identical(body$max_tokens, 128000L)
})

test_that("MC4: .build_message_body leaves Haiku/older models unchanged (no thinking/effort)", {
  body <- .build_message_body("claude-haiku-4-5", "hi", 4096L)
  expect_null(body$thinking)
  expect_null(body$output_config)
  expect_identical(body$max_tokens, 4096L)
})

test_that("MC5: .build_message_body attaches an optional system prompt", {
  body <- .build_message_body("claude-sonnet-5", "hi", 512L, system = "SYS")
  expect_identical(body$system, "SYS")
})

test_that("MC6: .ai_extract_text skips a leading thinking block", {
  resp <- list(content = list(list(thinking = ""), list(text = "hello")))
  expect_identical(.ai_extract_text(resp), "hello")
})

test_that("MC7: .ai_extract_text handles the mock/plain text-first shape", {
  resp <- list(content = list(list(text = "x")))
  expect_identical(.ai_extract_text(resp), "x")
})

test_that("MC8: .warn_if_truncated warns only on stop_reason == max_tokens", {
  expect_warning(.warn_if_truncated(list(stop_reason = "max_tokens")), "truncated")
  expect_warning(.warn_if_truncated(list(stop_reason = "max_tokens"), id = "VAR1"), "VAR1")
  expect_silent(.warn_if_truncated(list(stop_reason = "end_turn")))
  expect_silent(.warn_if_truncated(list()))
})

test_that("MC9: default model is Sonnet 5", {
  expect_identical(.DEFAULT_AI_MODEL, "claude-sonnet-5")
})

# --- Robust JSON recovery (labels/merge) ------------------------------------

test_that("MC10: .parse_var_object_chunk parses well-formed output without recovery", {
  res <- .parse_var_object_chunk('{"V1": {"1": "Oui", "0": "Non"}, "V2": {"1": "A"}}')
  expect_false(res$recovered)
  expect_setequal(names(res$map), c("V1", "V2"))
  expect_identical(res$map$V1$`1`, "Oui")
})

test_that("MC11: .parse_var_object_chunk recovers from a premature outer brace", {
  # The real failure: a stray '}' closes the object early, then more vars follow.
  txt <- '{"V1": {"1": "Oui"}}, "DECL": {"1": "A"}, "FAM_FOOT": {"1": "B"}}'
  res <- .parse_var_object_chunk(txt)
  expect_true(res$recovered)
  expect_setequal(names(res$map), c("V1", "DECL", "FAM_FOOT"))
  expect_identical(res$map$DECL$`1`, "A")
})

test_that("MC12: .parse_var_object_chunk recovers from two concatenated objects", {
  res <- .parse_var_object_chunk('{"V1": {"1": "Oui"}} {"V2": {"1": "A"}}')
  expect_true(res$recovered)
  expect_setequal(names(res$map), c("V1", "V2"))
})

test_that("MC13: recovery ignores braces inside string labels", {
  # Malformed wrapper forces Strategy 2; V's label contains a '}'.
  res <- .parse_var_object_chunk('{"V": {"1": "a}b"}}, "W": {"1": "x"}}')
  expect_true(res$recovered)
  expect_identical(res$map$V$`1`, "a}b")
  expect_identical(res$map$W$`1`, "x")
})

test_that("MC14: .extract_var_objects skips inner level-code keys", {
  entries <- .extract_var_objects('{"FAM_FOOT": {"1": "Oui", "0": "Non"}}')
  expect_identical(names(entries), "FAM_FOOT")
})

test_that("MC15: .convert_label_val handles keyed objects and positional arrays", {
  keyed <- .convert_label_val(list("1" = "Oui", "0" = "Non"))
  expect_identical(keyed[["1"]], "Oui")
  arr <- .convert_label_val(list("A", "B"))
  expect_identical(unname(arr), c("A", "B"))
  expect_null(.convert_label_val(42L))
})
