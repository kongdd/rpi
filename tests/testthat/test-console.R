test_that("concurrent chats never share a line", {
  reset_console()
  a <- rpi:::pi_stream_state(NULL, "text", FALSE, Inf, 1L, rpi:::pi_tag("A"))
  b <- rpi:::pi_stream_state(NULL, "text", FALSE, Inf, 2L, rpi:::pi_tag("B"))
  out <- capture.output({
    rpi:::pi_console_write(a$id, a$tag, "hello")
    rpi:::pi_console_write(b$id, b$tag, "world")
    rpi:::pi_console_write(a$id, a$tag, "again")
  })
  expect_identical(crayon::strip_style(out), c("[A] hello", "[B] world", "[A] again"))
})

test_that("finishing another chat does not break the current line", {
  reset_console()
  out <- capture.output({
    rpi:::pi_console_write(1L, "[A] ", "hello")
    rpi:::pi_console_newline(2L)
    rpi:::pi_console_write(1L, "[A] ", " again\n")
  })
  expect_identical(out, "[A] hello again")
})

test_that("every line of a chunk carries the label", {
  reset_console()
  st <- rpi:::pi_stream_state(NULL, "text", FALSE, Inf, 1L, rpi:::pi_tag("A"))
  out <- capture.output(rpi:::pi_console_write(st$id, st$tag, "one\ntwo\n"))
  expect_identical(crayon::strip_style(out), c("[A] one", "[A] two"))
})

test_that("consecutive and trailing blank lines are preserved", {
  for (chunk in c("\n\n", "one\n\n", "one\n\nthree\n\n")) {
    reset_console()
    out <- capture.output(rpi:::pi_console_write(1L, "", chunk))
    expect_identical(paste0(paste(out, collapse = "\n"), "\n"), chunk)
  }
})

test_that("a chat without label keeps the plain output", {
  reset_console()
  st <- rpi:::pi_stream_state(NULL, "text", FALSE, Inf, 1L, "")
  out <- capture.output(rpi:::pi_console_write(st$id, st$tag, "plain"))
  expect_identical(out, "plain")
})

test_that("the status line of a labelled chat is tagged", {
  reset_console()
  chat <- chat_pi(client = fake_pi_client(), label = "A")
  on.exit(chat$close())
  out <- capture.output(chat$chat("hi"))
  expect_match(crayon::strip_style(out[[1]]), "^\\[A\\] Hello from pi$")
  expect_match(crayon::strip_style(out[[2]]), "^\\[A\\] ↑")
  status <- capture.output(show_status(chat$get_turns(), label = "A"))
  expect_identical(crayon::strip_style(status), crayon::strip_style(out[[2]]))
})
