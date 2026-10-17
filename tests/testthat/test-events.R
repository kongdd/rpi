events <- list(
  list(type = "session", id = "s1"),
  list(type = "message_end", message = list(
    role = "user", content = list(list(type = "text", text = "hi"))
  )),
  list(type = "message_update", assistantMessageEvent = list(type = "text_delta", delta = "he")),
  list(type = "message_end", message = list(
    role = "assistant", model = "test-model",
    content = list(list(type = "thinking", thinking = "hmm"),
                   list(type = "text", text = "hello")),
    usage = list(input = 10, output = 2, cacheRead = 5, cacheWrite = 0,
                 cost = list(total = 0.5)),
    durationMs = 1200, stopReason = "stop"
  )),
  list(type = "agent_settled", aborted = FALSE)
)

test_that("pi_text() returns the final assistant text", {
  expect_equal(piagent:::pi_text(events), "hello")
  expect_equal(piagent:::pi_text(list(list(type = "agent_start"))), "")
})

test_that("pi_content_text() reads text and thinking blocks", {
  content <- events[[4]]$message$content
  expect_equal(piagent:::pi_content_text(content), "hello")
  expect_equal(piagent:::pi_content_text(content, "thinking"), "hmm")
  expect_equal(piagent:::pi_content_text("plain"), "plain")
  expect_equal(piagent:::pi_content_text(NULL), "")
})

test_that("pi_turns() builds one row per message, with usage", {
  turns <- piagent:::pi_turns(events)
  expect_equal(nrow(turns), 2)
  expect_equal(turns$role, c("user", "assistant"))
  expect_equal(turns$text, c("hi", "hello"))
  expect_equal(turns$thinking[2], "hmm")
  expect_equal(turns$input[2], 10)
  expect_equal(turns$cost[2], 0.5)
  expect_true(is.na(turns$cost[1]))
  expect_equal(nrow(piagent:::pi_turns(list())), 0)
})

test_that("pi_check_events() turns a failed run into an error", {
  failed <- list(list(type = "message_end", message = list(
    role = "assistant", content = list(), stopReason = "error",
    errorMessage = "boom"
  )))
  expect_error(piagent:::pi_check_events(failed), "boom")
  expect_silent(piagent:::pi_check_events(events))
})

test_that("prompts are collapsed and validated", {
  expect_equal(piagent:::pi_prompt("a", "b"), "a\nb")
  expect_error(piagent:::pi_prompt())
  expect_error(piagent:::pi_prompt(""))
})
