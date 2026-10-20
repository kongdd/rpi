layout_stream <- function(events, echo = "all") {
  reset_console()
  st <- rpi:::pi_stream_state(NULL, echo, FALSE, Inf)
  index <- 0L
  st$p <- list(
    read_output_lines = function() {
      index <<- index + 1L
      jsonlite::toJSON(events[[index]], auto_unbox = TRUE)
    },
    read_error_lines = function() character()
  )
  st
}

layout_update <- function(type, ...) {
  list(type = "message_update", assistantMessageEvent = list(type = type, ...))
}

layout_tool <- function(name) {
  list(type = "tool_execution_start", toolName = name, args = list(path = "a"))
}

test_that("thinking_end finishes the line before any answer arrives", {
  for (suffix in c("", "\n")) {
    st <- layout_stream(list(
      layout_update("thinking_delta", delta = paste0("Thinking", suffix)),
      layout_update("thinking_end", content = paste0("Thinking", suffix))
    ))
    out <- capture.output(for (i in 1:2) rpi:::pi_stream_poll(st))
    expect_identical(crayon::strip_style(out), "Thinking")
    expect_null(rpi:::pi_console$owner)
    expect_false(st$thinking)
  }
})

test_that("tools and thinking are separated without extra blank lines", {
  events <- list(
    layout_tool("read"),
    layout_tool("bash"),
    layout_update("text_delta", delta = "Checking"),
    layout_tool("read"),
    layout_update("thinking_delta", delta = "Thinking"),
    layout_update("text_delta", delta = ""),
    layout_update("thinking_delta", delta = " more"),
    layout_update("thinking_end", content = "Thinking more"),
    layout_update("text_delta", delta = "Answer\n")
  )
  st <- layout_stream(events)
  out <- capture.output(for (i in seq_along(events)) rpi:::pi_stream_poll(st))
  expect_identical(crayon::strip_style(out), c(
    '[tool] read: {"path":"a"}',
    '[tool] bash: {"path":"a"}',
    "Checking",
    '[tool] read: {"path":"a"}',
    "Thinking more",
    "Answer"
  ))
})

test_that("hidden thinking and tools do not introduce newlines", {
  events <- list(
    layout_update("thinking_delta", delta = "Thinking"),
    layout_update("thinking_end", content = "Thinking"),
    layout_tool("read"),
    layout_update("text_delta", delta = "Answer")
  )
  for (echo in c("text", "none")) {
    st <- layout_stream(events, echo)
    out <- capture.output(for (i in seq_along(events)) rpi:::pi_stream_poll(st))
    expect_identical(out, if (echo == "text") "Answer" else character())
  }
})

test_that("finishing a chat does not add a blank line after an existing newline", {
  reset_console()
  chat <- chat_pi(client = fake_pi_client(env = c(
    FAKE_PI_TOOLS = "1", FAKE_PI_THINKING = "1", FAKE_PI_JSON = "Answer\n"
  )))
  on.exit(chat$close())
  out <- capture.output(answer <- chat$chat("hi", echo = "all"))
  status <- capture.output(show_status(chat$get_turns()))
  expect_identical(crayon::strip_style(out), c(
    '[tool] read: {"path":"R/a.R"}', "Let me think", "Answer", status
  ))
  expect_identical(answer, "Answer\n")
})
