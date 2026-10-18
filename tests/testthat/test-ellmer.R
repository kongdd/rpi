test_that("chat_pi_ellmer() answers through pi and records usage", {
  chat <- chat_pi_ellmer(client = fake_pi_client())
  expect_equal(chat$chat("hi"), "Hello from pi", ignore_attr = TRUE)

  tokens <- chat$get_tokens()
  expect_equal(nrow(tokens), 1)
  expect_equal(tokens$input, 100)
  expect_equal(tokens$output, 5)
  expect_equal(tokens$cached_input, 20)
  expect_equal(sum(chat$get_cost()), 0.25)

  expect_s7_class(chat$last_turn(), ellmer::AssistantTurn)
  expect_equal(chat$last_turn()@finish_reason, "success")
  expect_equal(length(chat$get_turns()), 2)
})

map_chr_text <- function(chunks) {
  vapply(chunks, function(x) if (is.character(x)) x else x@text, character(1))
}

test_that("stream() yields the deltas of one answer", {
  chat <- chat_pi_ellmer(client = fake_pi_client())
  chunks <- coro::collect(chat$stream("hi"))
  expect_equal(paste(unlist(chunks), collapse = ""), "Hello from pi")
})

test_that("stream(stream = \"content\") yields ellmer content objects", {
  chat <- chat_pi_ellmer(client = fake_pi_client())
  chunks <- coro::collect(chat$stream("hi", stream = "content"))
  expect_s7_class(chunks[[1]], ellmer::ContentText)
  expect_equal(paste(map_chr_text(chunks), collapse = ""), "Hello from pi")
})

test_that("a slow run is stopped by the client timeout", {
  chat <- chat_pi_ellmer(
    client = fake_pi_client(timeout = 1, env = c(FAKE_PI_SLEEP = "5"))
  )
  expect_error(chat$chat("hi"), "timeout")
})

test_that("async methods resolve", {
  chat <- chat_pi_ellmer(client = fake_pi_client())
  expect_equal(pi_sync(chat$chat_async("hi")), "Hello from pi", ignore_attr = TRUE)

  chunks <- pi_sync(coro::async_collect(chat$stream_async("hi")))
  expect_equal(paste(unlist(chunks), collapse = ""), "Hello from pi")
})

test_that("a failing run raises the pi error", {
  chat <- chat_pi_ellmer(
    client = fake_pi_client(env = c(FAKE_PI_ERROR = "1"))
  )
  expect_error(chat$chat("hi"), "boom")
  expect_equal(length(chat$get_turns()), 0)
})

test_that("structured output is refused with a pointer to chat_pi()", {
  chat <- chat_pi_ellmer(client = fake_pi_client())
  expect_error(
    chat$chat_structured("hi", type = ellmer::type_string()),
    "chat_structured"
  )
})
