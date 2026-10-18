test_that("pi histories select the active branch and preserve raw extension records", {
  cl <- pi_client(bin = "pi", dir = ".", session = FALSE)
  store <- piagent:::pi_history_store(cl)
  entries <- piagent:::pi_history_read(store$path)
  entry <- function(id, parent, text, type = "message") {
    list(type = type, id = id, parentId = parent,
         message = list(role = "user", content = text))
  }
  entries <- c(entries, list(
    entry("a", NULL, "root"), entry("b", "a", "abandoned"),
    entry("c", "a", "active"),
    list(type = "custom", id = "d", parentId = "c", customType = "my-extension", data = list(x = 42))
  ))
  lines <- vapply(entries, function(x) as.character(jsonlite::toJSON(
    x, auto_unbox = TRUE, null = "null"
  )), character(1))
  writeLines(lines, store$path)
  messages <- piagent:::pi_history_messages(store$path)
  expect_equal(vapply(messages, function(x) x$content, character(1)), c("root", "active"))
  expect_equal(piagent:::pi_history_read(store$path)[[5]]$data$x, 42)
})

test_that("provider signatures and cache accounting survive conversion", {
  msg <- list(role = "assistant", content = list(
    list(type = "thinking", thinking = "Thought", thinkingSignature = "opaque"),
    list(type = "text", text = "Answer", textSignature = "opaque-text")
  ), provider = "anthropic", model = "test", api = "anthropic-messages",
  stopReason = "stop", usage = list(input = 10, output = 3, cacheRead = 100,
                                    cacheWrite = 50, cost = list(total = 0)))
  turn <- piagent:::pi_message_turns(list(msg))[[1]]
  expect_equal(turn@tokens, c(input = 60, output = 3, cached_input = 100))
  expect_identical(piagent:::pi_turn_message(turn), msg)
})

test_that("pre-cancelled controllers never launch pi", {
  capture <- tempfile(fileext = ".rds")
  chat <- chat_pi_ellmer(client = fake_pi_client(env = c(FAKE_PI_CAPTURE = capture)))
  controller <- ellmer::stream_controller()
  controller$cancel()
  coro::collect(chat$stream("hi", controller = controller))
  expect_false(file.exists(capture))
  expect_s7_class(chat$last_turn(), ellmer::AssistantPartialTurn)
  expect_error(chat$stream("hi", controller = list()), "stream_controller")
})

test_that("a rejected concurrent request does not release the running request", {
  chat <- chat_pi_ellmer(client = fake_pi_client(env = c(FAKE_PI_PAUSE = "5")))
  controller <- ellmer::stream_controller()
  iterator <- chat$stream("hi", controller = controller)
  expect_equal(iterator(), "Hello from pi")
  expect_error(chat$chat("second"), "already running")
  expect_error(chat$chat("third"), "already running")
  expect_error(chat$set_turns(list()), "already running")
  controller$cancel()
  coro::collect(iterator)
  chat$set_turns(list())
  expect_length(chat$get_turns(), 0)
})

test_that("all echo includes tool progress and defaults echo live text", {
  chat <- chat_pi_ellmer(client = fake_pi_client(env = c(FAKE_PI_TOOLS = "1")))
  out <- capture.output(chat$chat("hi", echo = "all"))
  expect_match(paste(out, collapse = " "), "\\[tool\\] read")
  chat <- chat_pi_ellmer(client = fake_pi_client())
  expect_match(paste(capture.output(chat$chat("hi")), collapse = " "), "Hello from pi")
})

test_that("empty token tables and explicitly added turns remain usable", {
  chat <- chat_pi_ellmer(client = fake_pi_client(), echo = "none")
  expect_named(chat$get_tokens(), c("input", "output", "cached_input", "cost"))
  chat$add_turn(ellmer::UserTurn("remember"), ellmer::AssistantTurn("yes"))
  chat$chat("hi")
  expect_length(chat$get_turns(), 4)
})

test_that("an ephemeral chat cleans up its session file", {
  chat <- chat_pi_ellmer(client = fake_pi_client())
  entries <- chat$get_events()
  expect_equal(entries[[1]]$version, 3L)
  chat$close()
  expect_error(chat$get_events(), "missing")
})
