codex_auth_fixture <- function(...) {
  path <- tempfile(fileext = ".json")
  cred <- utils::modifyList(list(
    type = "oauth", access = "fake-access", refresh = "fake-refresh",
    accountId = "fake-account", expires = (as.numeric(Sys.time()) + 3600) * 1000
  ), list(...), keep.null = TRUE)
  jsonlite::write_json(list(`openai-codex` = cred), path, auto_unbox = TRUE)
  path
}

codex_events <- function(text = "OK") {
  list(
    list(type = "response.output_text.delta", delta = text),
    list(type = "response.completed", response = list(
      status = "completed", output = list(list(
        type = "message", role = "assistant",
        content = list(list(type = "output_text", text = text))
      )), usage = list(input_tokens = 10, output_tokens = 2)
    ))
  )
}

test_that("Codex reuses OAuth headers without modifying authorization", {
  path <- codex_auth_fixture()
  on.exit(unlink(path))
  before <- readBin(path, "raw", file.info(path)$size)
  testthat::local_mocked_bindings(pi_auth_file = function() path, .package = "rpi")
  chat <- chat_codex(model = "test-codex", system_prompt = "Be terse.")
  expect_s3_class(chat, "Chat")
  provider <- chat$get_provider()
  expect_equal(provider@base_url, "https://chatgpt.com/backend-api/codex")
  expect_equal(provider@credentials(), list(
    Authorization = "Bearer fake-access", `chatgpt-account-id` = "fake-account"
  ))
  expect_equal(chat$get_system_prompt(), "Be terse.")
  expect_equal(readBin(path, "raw", file.info(path)$size), before)
  jsonlite::write_json(list(`openai-codex` = list(
    type = "oauth", access = "updated", accountId = "account-2",
    expires = (as.numeric(Sys.time()) + 3600) * 1000
  )), path, auto_unbox = TRUE)
  expect_equal(provider@credentials()$Authorization, "Bearer updated")
})

test_that("missing, invalid and expired credentials fail without leaking secrets", {
  expect_error(pi_codex_credentials(tempfile()), "Cannot read")
  path <- codex_auth_fixture(expires = 0)
  on.exit(unlink(path))
  expect_error(pi_codex_credentials(path), "expired.*login")
  writeLines('{"secret": "DO_NOT_PRINT", invalid}', path)
  err <- tryCatch(pi_codex_credentials(path), error = identity)
  expect_match(conditionMessage(err), "Cannot read")
  expect_false(grepl("DO_NOT_PRINT", conditionMessage(err)))
  writeLines('{"openai-codex":{"type":"api_key","key":"secret"}}', path)
  expect_error(pi_codex_credentials(path), "No Codex OAuth")
  writeLines('{"openai-codex":{"type":"oauth","access":"secret"}}', path)
  expect_error(pi_codex_credentials(path), "No Codex OAuth")
})

test_that("pi's authorization path is fixed", {
  expect_equal(pi_auth_file(), path.expand("~/.pi/agent/auth.json"))
})

test_that("silent and async chats force SSE while keeping ellmer history and usage", {
  path <- codex_auth_fixture()
  on.exit(unlink(path))
  requests <- list()
  capture <- function(req) {
    requests[[length(requests) + 1L]] <<- req
    codex_events()
  }
  testthat::local_mocked_bindings(pi_auth_file = function() path, .package = "rpi")
  testthat::local_mocked_bindings(
    chat_perform_stream = function(provider, req, ...) {
      events <- capture(req)
      coro::generator(function() for (event in events) yield(event))()
    },
    chat_perform_async_stream = function(provider, req, ...) {
      events <- capture(req)
      coro::async_generator(function() for (event in events) yield(event))()
    },
    .package = "ellmer"
  )
  chat <- chat_codex(model = "test-codex", system_prompt = "Be terse.")
  expect_equal(as.character(chat$chat("hello")), "OK")
  expect_equal(as.character(pi_sync(chat$chat_async("again"))), "OK")
  expect_null(chat$stream)
  chunks <- pi_sync(coro::async_collect(chat$stream_async("async stream")))
  expect_equal(paste(unlist(chunks), collapse = ""), "OK\n")
  expect_equal(length(chat$get_turns()), 6)
  expect_equal(chat$get_tokens()$input, rep(10, 3))
  expect_equal(chat$get_tokens()$output, rep(2, 3))
  for (req in requests) {
    expect_equal(req$url, "https://chatgpt.com/backend-api/codex/responses")
    expect_identical(req$body$data$stream, TRUE)
    expect_identical(req$body$data$store, FALSE)
    expect_equal(req$body$data$instructions, "You are a helpful assistant.")
    expect_null(req$body$data$service_tier)
    expect_type(req$headers[["chatgpt-account-id"]], "weakref")
  }
  expect_equal(requests[[1]]$body$data$input[[1]]$role, "system")
  expect_equal(requests[[1]]$body$data$input[[1]]$content[[1]]$text, "Be terse.")
})

test_that("Codex answers through ellmer with real pi authorization", {
  skip_if_no_pi()
  skip_if_not(file.exists(pi_auth_file()), "no pi authorization file")
  model <- Sys.getenv("PI_MODEL")
  skip_if_not(nzchar(model), "set PI_MODEL to an authorized Codex model ID")
  chat <- chat_codex(model = model, system_prompt = "Answer briefly.")
  before <- readBin(pi_auth_file(), "raw", file.info(pi_auth_file())$size)
  called <- FALSE
  chat$register_tool(ellmer::tool(
    function(x, y) {
      called <<- TRUE
      x + y
    }, "Add two numbers in R",
    arguments = list(x = ellmer::type_number(), y = ellmer::type_number()),
    name = "piagent_test_add"
  ))
  answer <- chat$chat("Call piagent_test_add with x=17 and y=25. Reply with the result only.")
  expect_true(called)
  expect_match(answer, "42")
  expect_match(pi_sync(chat$chat_async("Reply only PIAGENT_ASYNC.")),
    "PIAGENT_ASYNC",
    ignore.case = TRUE
  )
  expect_gt(sum(chat$get_tokens()$output), 0)
  expect_true(any(vapply(chat$get_turns(), function(x) {
    any(vapply(x@contents, function(y) inherits(y, "ellmer::ContentToolRequest"), logical(1)))
  }, logical(1))))
  expect_equal(readBin(pi_auth_file(), "raw", file.info(pi_auth_file())$size), before)
})
