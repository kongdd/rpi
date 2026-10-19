test_that("chat replaces the removed stream entry point", {
  chat <- chat_pi_ellmer(client = fake_pi_client(), echo = "text")
  on.exit(chat$close())
  expect_null(chat$stream)
  out <- capture.output(result <- withVisible(chat$chat("hi")))
  expect_equal(out, "Hello from pi")
  expect_identical(result$value, "Hello from pi")
  expect_false(result$visible)
  expect_length(chat$get_turns(), 2)
  expect_equal(chat$get_tokens()$output, 5)
})

test_that("chat errors propagate and release the chat", {
  chat <- chat_pi_ellmer(client = fake_pi_client(env = c(FAKE_PI_ERROR = "1")))
  on.exit(chat$close())
  expect_error(chat$chat("hi"), "boom")
  expect_length(chat$get_turns(), 0)
  expect_error(chat$chat("again"), "boom")
})
