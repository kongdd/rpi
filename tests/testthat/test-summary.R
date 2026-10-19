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

test_that("empty summaries retain typed columns", {
  result <- summary(structure(list(), class = c("pi_turns", "list")))
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0L)
  expect_named(result, c("role", "text", "input", "output", "cached_input", "cost"))
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
  chat <- piagent:::ChatPiCodex$new(
    provider = ellmer::Provider(name = "test", base_url = ""),
    model = ellmer::Model(name = "test"), system_prompt = "instructions"
  )
  chat$set_turns(list(ellmer::UserTurn("question"), ellmer::AssistantTurn("answer")))
  data <- chat$get_turns()
  expect_s3_class(summary(data), "data.table")
  expect_equal(summary(data)$text, c("question", "answer"))
  expect_equal(summary(chat$get_turns(TRUE))$role, c("system", "user", "assistant"))
})
