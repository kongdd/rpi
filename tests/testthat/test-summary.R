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

test_that("status accepts turns or their summary table", {
  turns <- list(
    ellmer::AssistantTurn("first",
      tokens = c(input = 456300, output = 88000, cached_input = 26900700), cost = 4
    ),
    ellmer::AssistantTurn("last",
      tokens = c(input = 700, output = 1000, cached_input = 99300), cost = 0.153
    ),
    ellmer::UserTurn("next")
  )
  data <- structure(turns, class = c("pi_turns", "list"))
  status <- "↑457k ↓89k R27M CH99.3% 100k/? $4.153"
  expect_identical(crayon::strip_style(capture.output(show_status(data))), status)
  expect_identical(
    crayon::strip_style(capture.output(show_status(summary(data)))), status
  )
  expect_identical(capture.output(print(data)), capture.output(print(summary(data))))
  old <- options(crayon.enabled = TRUE, cli.num_colors = 8L)
  on.exit(options(old), add = TRUE)
  expect_identical(capture.output(show_status(data)), crayon::green(status))
})

test_that("status compacts token counts with pi-style units", {
  x <- data.table::data.table(
    role = "assistant", input = 1000, output = 9999,
    cached_input = 1e6, cost = 0
  )
  expect_identical(
    crayon::strip_style(capture.output(show_status(x))),
    "↑1.0k ↓10.0k R1.0M CH99.9% 1.0M/? $0.000"
  )
})

test_that("status uses the cached model context limit", {
  turns <- structure(list(ellmer::AssistantTurn(
    "answer", json = list(provider = "test", model = "model"),
    tokens = c(input = 145000, output = 10, cached_input = 0)
  )), class = c("pi_turns", "list"))
  attr(turns, "context_limit") <- "272K"
  expect_identical(crayon::strip_style(capture.output(show_status(turns))),
    "↑145k ↓10 R0 CH0.0% 145k/272k $0.000"
  )
})

test_that("tool calls remain visible while tool results are omitted", {
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
  expect_equal(nrow(result), 1L)
  expect_equal(result$role, "assistant")
  expect_equal(result$type, "thinking,tool_call")
  expect_equal(result$tool, "read, read")
  expect_equal(result$text, '[read] {"path":"DESCRIPTION"}\n[read] {"path":"README.md"}')
  expect_equal(result$input, 10)
  expect_equal(result$output, 7)
  expect_equal(sum(result$cost, na.rm = TRUE), 0.02)
  expect_identical(turns, before)
})

test_that("thinking-only turns remain visible while tool errors are omitted", {
  turns <- list(
    ellmer::AssistantTurn(list(ellmer::ContentThinking("reasoning"))),
    ellmer::UserTurn(list(ellmer::ContentToolResult(
      error = "missing file", extra = list(pi = list(toolName = "read"))
    ))),
    ellmer::UserTurn(list(ellmer::ContentToolResult(list(answer = 42))))
  )
  result <- summary(structure(turns, class = c("pi_turns", "list")))
  expect_equal(result$role, "assistant")
  expect_equal(result$tool, "")
  expect_equal(result$text, "reasoning")
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
  turns <- list(ellmer::UserTurn(list(ellmer::ContentToolResult("result"))))
  hidden <- summary(structure(turns, class = c("pi_turns", "list")))
  expect_equal(hidden, result)
})

test_that("summary dispatches from a chat and prints its token status", {
  chat <- chat_pi(client = fake_pi_client(), echo = "none")
  on.exit(chat$close())
  chat$add_turn(
    ellmer::UserTurn("question"),
    ellmer::AssistantTurn("answer", tokens = c(input = 20, output = 5, cached_input = 10))
  )
  out <- capture.output(result <- withVisible(summary(chat)))
  expected <- summary(chat$get_turns())
  expect_identical(crayon::strip_style(out),
    crayon::strip_style(capture.output(show_status(chat$get_turns())))
  )
  expect_identical(result$value, expected)
  expect_true(result$visible)
})

test_that("get_turns returns a list supporting summary dispatch", {
  chat <- chat_pi(
    client = fake_pi_client(model = "fake/fake-model"), echo = "none"
  )
  on.exit(chat$close())
  expect_equal(nrow(summary(chat$get_turns())), 0L)
  chat$add_turn(ellmer::UserTurn("question"), ellmer::AssistantTurn("answer"))
  data <- chat$get_turns()
  expect_true(is.list(data))
  expect_s3_class(data, "pi_turns")
  expect_equal(attr(data, "context_limit"), "272K")
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
