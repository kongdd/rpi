test_that("turn summaries preserve order, text, and usage", {
  turns <- list(
    ellmer::UserTurn("question"),
    ellmer::AssistantTurn(
      list(ellmer::ContentThinking("private"), ellmer::ContentText("answer")),
      tokens = c(input = 10, output = 3, cached_input = 2), cost = 0.01
    ),
    ellmer::AssistantPartialTurn("partial")
  )
  result <- summary(structure(turns, class = c("pi_turns", "list")))
  expect_s3_class(result, "data.table")
  expect_equal(result$role, c("user", "assistant", "assistant"))
  expect_equal(result$text, c("question", "answer", "partial"))
  expect_equal(result$input, c(NA_real_, 10, NA_real_))
  expect_equal(result$output, c(NA_real_, 3, NA_real_))
  expect_equal(result$cached_input, c(NA_real_, 2, NA_real_))
  expect_equal(result$cost, c(NA_real_, 0.01, NA_real_))
})

test_that("printing turns displays their summary and invisibly returns the list", {
  for (turns in list(list(), list(ellmer::UserTurn("question")))) {
    data <- structure(turns, class = c("pi_turns", "list"))
    out <- capture.output(result <- withVisible(print(data)))
    expect_identical(out, capture.output(print(summary(data))))
    expect_identical(result$value, data)
    expect_false(result$visible)
  }
})

test_that("tool calls and results are visible without duplicating usage", {
  requests <- lapply(c("DESCRIPTION", "README.md"), function(path) {
    ellmer::ContentToolRequest(id = path, name = "read", arguments = list(path = path))
  })
  turns <- list(
    ellmer::AssistantTurn(
      c(list(ellmer::ContentThinking("private")), requests),
      tokens = c(input = 10, output = 7, cached_input = 3), cost = 0.02
    ),
    ellmer::UserTurn(list(ellmer::ContentToolResult("Package: piagent", request = requests[[1]]))),
    ellmer::UserTurn(list(ellmer::ContentToolResult(42, request = requests[[2]])))
  )
  before <- turns
  result <- summary(structure(turns, class = c("pi_turns", "list")))
  expect_equal(nrow(result), 3L)
  expect_equal(result$role, c("assistant", "tool", "tool"))
  expect_equal(result$type, c("thinking,tool_call", "tool_result", "tool_result"))
  expect_equal(result$tool, c("read, read", "read", "read"))
  expect_equal(result$text, c(
    '[read] {"path":"DESCRIPTION"}\n[read] {"path":"README.md"}',
    "Package: piagent", "42"
  ))
  expect_equal(result$input, c(10, NA, NA))
  expect_equal(result$output, c(7, NA, NA))
  expect_equal(sum(result$cost, na.rm = TRUE), 0.02)
  expect_identical(turns, before)
})

test_that("thinking-only turns and tool errors remain visible", {
  turns <- list(
    ellmer::AssistantTurn(list(ellmer::ContentThinking("reasoning"))),
    ellmer::UserTurn(list(ellmer::ContentToolResult(
      error = "missing file", extra = list(pi = list(toolName = "read"))
    ))),
    ellmer::UserTurn(list(ellmer::ContentToolResult(list(answer = 42))))
  )
  result <- summary(structure(turns, class = c("pi_turns", "list")))
  expect_equal(result$role, c("assistant", "tool", "tool"))
  expect_equal(result$tool, c("", "read", ""))
  expect_equal(result$text, c("reasoning", "[error] missing file", '{"answer":42}'))
})

test_that("attachments are identified without exposing image data", {
  turns <- list(ellmer::UserTurn(list(
    ellmer::ContentImageInline("image/png", "opaque-image-data")
  )))
  result <- summary(structure(turns, class = c("pi_turns", "list")))
  expect_equal(result$type, "attachment")
  expect_equal(result$text, "<ImageInline>")
})

test_that("empty summaries retain typed columns", {
  result <- summary(structure(list(), class = c("pi_turns", "list")))
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0L)
  expect_named(result, c("role", "type", "tool", "input", "output", "cached_input", "cost", "text"))
  expect_type(result$role, "character")
  expect_type(result$input, "double")
})

test_that("get_turns returns a list supporting summary dispatch", {
  chat <- chat_pi_ellmer(client = fake_pi_client(), echo = "none")
  on.exit(chat$close())
  expect_equal(nrow(summary(chat$get_turns())), 0L)
  chat$add_turn(ellmer::UserTurn("question"), ellmer::AssistantTurn("answer"))
  data <- chat$get_turns()
  expect_true(is.list(data))
  expect_s3_class(data, "pi_turns")
  expect_s3_class(summary(data), "data.table")
  expect_equal(summary(data)$text, c("question", "answer"))
  expect_equal(length(chat$get_turns(include_system_prompt = TRUE)), 2L)
})

test_that("Codex turns support summary dispatch", {
  chat <- rpi:::ChatPiCodex$new(
    provider = ellmer::Provider(name = "test", base_url = ""),
    model = ellmer::Model(name = "test"), system_prompt = "instructions"
  )
  chat$set_turns(list(ellmer::UserTurn("question"), ellmer::AssistantTurn("answer")))
  data <- chat$get_turns()
  expect_s3_class(summary(data), "data.table")
  expect_equal(summary(data)$text, c("question", "answer"))
  expect_equal(summary(chat$get_turns(TRUE))$role, c("system", "user", "assistant"))
})
