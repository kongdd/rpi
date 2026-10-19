test_that("ChatPi inherits the public Chat API without overriding private generators", {
  chat <- chat_pi_ellmer(client = fake_pi_client(), echo = "none")
  expect_s3_class(chat, "Chat")
  expect_equal(chat$chat("hi"), "Hello from pi")
  expect_equal(chat$get_tokens()$input, 100)
  expect_equal(chat$get_tokens()$output, 5)
  expect_equal(chat$get_tokens()$cached_input, 20)
  expect_equal(sum(chat$get_cost()), 0.25)
  expect_equal(chat$last_turn()@duration, 0.5)
  expect_equal(chat$last_turn()@finish_reason, "success")
  expect_length(chat$get_turns(), 2)
})

test_that("chat and asynchronous methods yield only answers", {
  chat <- chat_pi_ellmer(client = fake_pi_client(), echo = "none")
  expect_equal(chat$chat("hi"), "Hello from pi")
  expect_equal(pi_sync(chat$chat_async("hi")), "Hello from pi")
  chunks <- pi_sync(coro::async_collect(chat$stream_async("hi")))
  expect_equal(paste(unlist(chunks), collapse = ""), "Hello from pi")
  expect_length(chat$get_turns(), 6)
})

test_that("content streams include thinking even when echo is none", {
  chat <- chat_pi_ellmer(client = fake_pi_client(env = c(FAKE_PI_THINKING = "1")))
  chunks <- pi_sync(coro::async_collect(chat$stream_async("hi", stream = "content")))
  expect_s7_class(chunks[[1]], ellmer::ContentThinking)
  expect_s7_class(chunks[[2]], ellmer::ContentText)
  expect_equal(chunks[[1]]@thinking, "Let me think")
})

test_that("the client timeout works in both modes", {
  chat <- chat_pi_ellmer(client = fake_pi_client(timeout = 0.5, env = c(FAKE_PI_SLEEP = "5")))
  expect_error(chat$chat("hi"), "timeout")
  result <- pi_sync(chat$chat_async("hi"))
  expect_s3_class(result, "error")
  expect_match(conditionMessage(result), "timeout")
})

test_that("unused streams do not start a process or consume timeout", {
  capture <- tempfile(fileext = ".rds")
  chat <- chat_pi_ellmer(client = fake_pi_client(timeout = 1, env = c(FAKE_PI_CAPTURE = capture)))
  iterator <- chat$stream_async("hi")
  expect_false(file.exists(capture))
  Sys.sleep(1.1)
  chunks <- pi_sync(coro::async_collect(iterator))
  expect_equal(paste(unlist(chunks), collapse = ""), "Hello from pi")
})

test_that("tool messages and per-request usage survive round trips", {
  capture <- tempfile(fileext = ".rds")
  cl <- fake_pi_client(env = c(FAKE_PI_TOOLS = "1", FAKE_PI_CAPTURE = capture))
  chat <- chat_pi_ellmer(client = cl, echo = "none")
  expect_equal(chat$chat("hi"), "Hello from pi")
  turns <- chat$get_turns()
  expect_length(turns, 4)
  expect_s7_class(turns[[2]]@contents[[2]], ellmer::ContentToolRequest)
  expect_s7_class(turns[[3]]@contents[[1]], ellmer::ContentToolResult)
  expect_equal(turns[[3]]@contents[[1]]@request@id, "call-1")
  expect_equal(nrow(chat$get_tokens()), 2)
  expect_equal(sum(chat$get_cost()), 0.5)
  original <- chat$get_events()
  chat$set_turns(turns)
  chat$chat("again")
  sent <- readRDS(capture)$entries
  expect_equal(sent[[3]]$message, original[[3]]$message)
  expect_equal(sent[[4]]$message, original[[4]]$message)
})

test_that("set_turns changes model history and preserves the original session", {
  capture <- tempfile(fileext = ".rds")
  cl <- fake_pi_client(
    session = TRUE, session_dir = tempfile(),
    env = c(FAKE_PI_CAPTURE = capture)
  )
  chat <- chat_pi_ellmer(client = cl, echo = "none")
  chat$chat("old history")
  old_id <- chat$get_session_id()
  old_file <- readRDS(capture)$args
  old_file <- old_file[[match("--session", old_file) + 1L]]
  old_bytes <- readBin(old_file, "raw", file.info(old_file)$size)
  chat$set_turns(list(ellmer::UserTurn("new history"), ellmer::AssistantTurn("OK")))
  expect_false(identical(chat$get_session_id(), old_id))
  chat$chat("question")
  sent <- readRDS(capture)$entries
  expect_equal(sent[[2]]$message$content[[1]]$text, "new history")
  expect_equal(sent[[3]]$message$content[[1]]$text, "OK")
  expect_identical(readBin(old_file, "raw", file.info(old_file)$size), old_bytes)
  resumed <- chat_pi_ellmer(client = cl, session_id = chat$get_session_id(), echo = "none")
  expect_equal(length(resumed$get_turns()), length(chat$get_turns()))
})

test_that("set_turns rolls back invalid content and empty history resets context", {
  chat <- chat_pi_ellmer(client = fake_pi_client(), echo = "none")
  chat$chat("hi")
  old <- chat$get_turns()
  expect_error(chat$set_turns(list(ellmer::UserTurn(list(
    ellmer::ContentImageRemote("https://example.com/image.png")
  )))), "restore")
  expect_identical(chat$get_turns(), old)
  chat$set_turns(list())
  expect_length(chat$get_turns(), 0)
  chat$chat("hi")
  expect_length(chat$get_turns(), 2)
})

test_that("structured output handles scalars, objects and asynchronous conversion", {
  chat <- chat_pi_ellmer(client = fake_pi_client(env = c(FAKE_PI_JSON = "42")))
  expect_equal(chat$chat_structured("hi", type = ellmer::type_integer()), 42L)
  expect_equal(pi_sync(chat$chat_structured_async("hi", type = ellmer::type_integer())), 42L)
  expect_equal(chat$chat_structured("hi", type = ellmer::type_integer(), convert = FALSE), 42)
  expect_equal(chat$chat_structured(type = ellmer::type_integer()), 42L)
  chat <- chat_pi_ellmer(client = fake_pi_client(env = c(FAKE_PI_JSON = '{"answer":42}')))
  expect_equal(
    chat$chat_structured("hi", type = ellmer::type_object(answer = ellmer::type_integer())),
    list(answer = 42L)
  )
})

test_that("inline and remote image inputs arrive as individual file arguments", {
  capture <- tempfile(fileext = ".rds")
  chat <- chat_pi_ellmer(client = fake_pi_client(env = c(FAKE_PI_CAPTURE = capture)), echo = "none")
  image <- ellmer::ContentImageInline("image/png", jsonlite::base64_enc(as.raw(c(1, 2, 3))))
  chat$chat("inspect", image)
  args <- readRDS(capture)$args
  file <- sub("^@", "", args[startsWith(args, "@")])
  expect_length(file, 1)
  expect_false(file.exists(file))
  source <- tempfile(fileext = ".png")
  writeBin(as.raw(c(1, 2, 3)), source)
  input <- piagent:::pi_turn_input(ellmer::UserTurn(list(
    ellmer::ContentImageRemote(paste0("file:///", normalizePath(source, winslash = "/")))
  )))
  expect_identical(readBin(input$files, "raw", 3), as.raw(c(1, 2, 3)))
  unlink(input$files)
})

test_that("cancellation records a partial turn and does not charge zero tokens", {
  chat <- chat_pi_ellmer(client = fake_pi_client(env = c(FAKE_PI_PAUSE = "5")))
  controller <- ellmer::stream_controller()
  iterator <- chat$stream_async("hi", controller = controller)
  expect_equal(pi_sync(iterator()), "Hello from pi")
  controller$cancel()
  pi_sync(coro::async_collect(iterator))
  expect_s7_class(chat$last_turn(), ellmer::AssistantPartialTurn)
  expect_equal(chat$last_turn()@text, "Hello from pi")
  expect_length(chat$get_turns(), 2)
  expect_true(is.na(chat$get_tokens()$input))
  expect_true(is.na(chat$get_tokens()$cost))
})

test_that("explicit errors, unsupported attachments and R tools are not silently ignored", {
  chat <- chat_pi_ellmer(client = fake_pi_client(env = c(FAKE_PI_ERROR = "1")))
  expect_error(chat$chat("hi"), "boom")
  expect_length(chat$get_turns(), 0)
  expect_error(chat$register_tool(identity), "pi extensions")
  expect_error(chat$register_tools(identity), "pi extensions")
  chat$close()
  expect_error(chat$chat("hi"), "closed")
})
