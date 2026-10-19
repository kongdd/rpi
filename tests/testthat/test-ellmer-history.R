test_that("pi histories select the active branch and preserve raw extension records", {
  cl <- pi_client(bin = "pi", dir = ".", session = FALSE)
  store <- rpi:::pi_history_store(cl)
  entries <- rpi:::pi_history_read(store$path)
  entry <- function(id, parent, text, type = "message") {
    list(
      type = type, id = id, parentId = parent,
      message = list(role = "user", content = text)
    )
  }
  entries <- c(entries, list(
    entry("a", NULL, "root"), entry("b", "a", "abandoned"),
    entry("c", "a", "active"),
    list(type = "custom", id = "d", parentId = "c", customType = "my-extension", data = list(x = 42))
  ))
  lines <- vapply(entries, function(x) {
    as.character(jsonlite::toJSON(
      x,
      auto_unbox = TRUE, null = "null"
    ))
  }, character(1))
  writeLines(lines, store$path)
  messages <- rpi:::pi_history_messages(store$path)
  expect_equal(vapply(messages, function(x) x$content, character(1)), c("root", "active"))
  expect_equal(rpi:::pi_history_read(store$path)[[5]]$data$x, 42)
})

test_that("provider signatures and cache accounting survive conversion", {
  msg <- list(
    role = "assistant", content = list(
      list(type = "thinking", thinking = "Thought", thinkingSignature = "opaque"),
      list(type = "text", text = "Answer", textSignature = "opaque-text")
    ), provider = "anthropic", model = "test", api = "anthropic-messages",
    stopReason = "stop", usage = list(
      input = 10, output = 3, cacheRead = 100,
      cacheWrite = 50, cost = list(total = 0)
    )
  )
  turn <- rpi:::pi_message_turns(list(msg))[[1]]
  expect_equal(turn@tokens, c(input = 60, output = 3, cached_input = 100))
  expect_identical(rpi:::pi_turn_message(turn), msg)
})

test_that("pre-cancelled controllers never launch pi", {
  capture <- tempfile(fileext = ".rds")
  chat <- chat_pi(client = fake_pi_client(env = c(FAKE_PI_CAPTURE = capture)))
  controller <- ellmer::stream_controller()
  controller$cancel()
  pi_sync(coro::async_collect(chat$stream_async("hi", controller = controller)))
  expect_false(file.exists(capture))
  expect_s7_class(chat$last_turn(), ellmer::AssistantPartialTurn)
  expect_error(chat$stream_async("hi", controller = list()), "stream_controller")
})

test_that("a rejected concurrent request does not release the running request", {
  chat <- chat_pi(client = fake_pi_client(env = c(FAKE_PI_PAUSE = "5")))
  controller <- ellmer::stream_controller()
  iterator <- chat$stream_async("hi", controller = controller)
  expect_equal(pi_sync(iterator()), "Hello from pi")
  expect_error(chat$chat("second"), "already running")
  expect_error(chat$chat("third"), "already running")
  expect_error(chat$set_turns(list()), "already running")
  controller$cancel()
  pi_sync(coro::async_collect(iterator))
  chat$set_turns(list())
  expect_length(chat$get_turns(), 0)
})

test_that("all echo includes tool progress and defaults echo live text", {
  chat <- chat_pi(client = fake_pi_client(env = c(FAKE_PI_TOOLS = "1")))
  out <- capture.output(chat$chat("hi", echo = "all"))
  expect_match(paste(out, collapse = " "), "\\[tool\\] read")
  chat <- chat_pi(client = fake_pi_client())
  expect_match(paste(capture.output(chat$chat("hi")), collapse = " "), "Hello from pi")
})

test_that("tool progress is green without coloring the answer", {
  old <- options(crayon.enabled = TRUE, cli.num_colors = 8L)
  on.exit(options(old), add = TRUE)
  chat <- chat_pi(client = fake_pi_client(env = c(FAKE_PI_TOOLS = "1")))
  on.exit(chat$close(), add = TRUE)
  out <- capture.output(chat$chat("hi", echo = "all"))
  tool <- grep("\\[tool\\]", out, value = TRUE)
  expect_match(tool, "\033[32m", fixed = TRUE)
  expect_match(crayon::strip_style(tool), "\\[tool\\] read:.*R/a.R")
  expect_false(crayon::has_style(out[[length(out) - 1L]]))
  expect_true(crayon::has_style(tail(out, 1)))
})

test_that("thinking is blue only with all echo", {
  old <- options(crayon.enabled = TRUE, cli.num_colors = 8L)
  on.exit(options(old), add = TRUE)
  chat <- chat_pi(client = fake_pi_client(env = c(FAKE_PI_THINKING = "1")))
  on.exit(chat$close(), add = TRUE)
  out <- capture.output(answer <- chat$chat("hi", echo = "all"))
  expect_match(
    paste(out, collapse = "\n"),
    paste0(crayon::blue("Let me think"), "\nHello from pi"), fixed = TRUE
  )
  expect_identical(answer, "Hello from pi")
  out <- capture.output(chat$chat("hi", echo = "text"))
  status <- capture.output(show_status(chat$get_turns()))
  expect_identical(tail(out, 2L), c("Hello from pi", status))
})

test_that("thinking chunks stay joined with one newline before answer chunks", {
  st <- rpi:::pi_stream_state(NULL, "all", FALSE, Inf)
  types <- c("thinking_delta", "thinking_delta", "text_delta", "text_delta")
  deltas <- c("Let ", "me think", "Hello", " world")
  index <- 0L
  st$p <- list(
    read_output_lines = function() {
      index <<- index + 1L
      jsonlite::toJSON(list(
        type = "message_update",
        assistantMessageEvent = list(type = types[index], delta = deltas[index])
      ), auto_unbox = TRUE)
    },
    read_error_lines = function() character()
  )
  out <- capture.output(for (i in seq_along(deltas)) rpi:::pi_stream_poll(st))
  expect_identical(crayon::strip_style(out), c("Let me think", "Hello world"))
  expect_identical(st$text, "Hello world")
})

test_that("empty token tables and explicitly added turns remain usable", {
  chat <- chat_pi(client = fake_pi_client(), echo = "none")
  expect_named(chat$get_tokens(), c("input", "output", "cached_input", "cost"))
  chat$add_turn(ellmer::UserTurn("remember"), ellmer::AssistantTurn("yes"))
  chat$chat("hi")
  expect_length(chat$get_turns(), 4)
})

test_that("an ephemeral chat cleans up its session file", {
  chat <- chat_pi(client = fake_pi_client())
  entries <- chat$get_events()
  expect_equal(entries[[1]]$version, 3L)
  chat$close()
  expect_error(chat$get_events(), "missing")
})
