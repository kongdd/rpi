pi_client_test <- function(...) pi_client(bin = "pi", dir = ".", ...)

flag <- function(args, name) {
  i <- which(args == name)
  if (length(i) == 0) {
    return(NA_character_)
  }
  args[i + 1]
}

test_that("pi_client() records the settings used by every call", {
  x <- pi_client_test(model = "sonnet", thinking = "low", tools = "read,grep", args = "-a")
  expect_s3_class(x, "pi_client")
  expect_equal(x$model, "sonnet")
  expect_true(x$session)
  expect_output(print(x), "sonnet")
})

test_that("pi_cli_args() maps the client onto pi flags", {
  x <- pi_client_test(model = "sonnet", thinking = "low", tools = "read,grep")
  args <- rpi:::pi_cli_args(x, session_id = "abc", append = c("be terse", ""))
  expect_equal(args[1:2], c("--mode", "json"))
  expect_equal(flag(args, "--model"), "sonnet")
  expect_equal(flag(args, "--thinking"), "low")
  expect_equal(flag(args, "--tools"), "read,grep")
  expect_equal(flag(args, "--session-id"), "abc")
  expect_equal(sum(args == "--append-system-prompt"), 1)
  expect_equal(flag(args, "--append-system-prompt"), "be terse")
})

test_that("models_pi can limit results to a selected model", {
  models <- models_pi(fake_pi_client(), "fake/fake-model")
  expect_equal(nrow(models), 1L)
  expect_equal(models$context, "272K")
})

test_that("a stateless client never passes a session id", {
  x <- pi_client_test(session = FALSE)
  args <- rpi:::pi_cli_args(x, session_id = "abc")
  expect_true("--no-session" %in% args)
  expect_false("--session-id" %in% args)
})

test_that("client arguments are validated", {
  expect_error(pi_client(bin = "pi", dir = "no-such-dir-xyz"))
  expect_error(pi_client(bin = "pi", model = c("a", "b")))
})
