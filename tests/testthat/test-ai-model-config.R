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
