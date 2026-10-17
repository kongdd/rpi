test_that("a live chat remembers earlier turns and reports its cost", {
  skip_if_no_pi()
  chat <- chat_pi(client = pi_client(bin = Sys.which("pi"), dir = tempdir(),
                                     thinking = "off"),
                  system_prompt = "Answer with a single word.")
  first <- chat$chat("Remember the word 'pelican'. Reply: ok")
  second <- chat$chat("Which word did I ask you to remember?")
  expect_match(second, "pelican", ignore.case = TRUE)

  turns <- chat$get_turns()
  expect_equal(sum(turns$role == "assistant"), 2)
  expect_true(all(turns$input[turns$role == "assistant"] > 0))
  expect_gt(chat$get_cost(), 0)
  expect_match(first, "ok", ignore.case = TRUE)
})

test_that("a streamed answer is echoed and returned", {
  skip_if_no_pi()
  out <- capture.output(
    text <- chat_pi(client = pi_client(bin = Sys.which("pi"), dir = tempdir(),
                                       thinking = "off", session = FALSE))$chat(
      "Reply with exactly: stream ok"
    )
  )
  expect_match(paste(out, collapse = " "), "stream", ignore.case = TRUE)
  expect_match(text, "stream ok", ignore.case = TRUE)
})

test_that("chat_async() resolves to the answer", {
  skip_if_no_pi()
  chat <- chat_pi(client = pi_client(bin = Sys.which("pi"), dir = tempdir(),
                                     thinking = "off", session = FALSE))
  value <- NULL
  promises::then(chat$chat_async("Reply with exactly: async ok"),
                 function(x) value <<- x)
  while (is.null(value)) later::run_now(0.1)
  expect_match(value, "async ok", ignore.case = TRUE)
})

test_that("chat_structured() returns parsed JSON", {
  skip_if_no_pi()
  chat <- chat_pi(client = pi_client(bin = Sys.which("pi"), dir = tempdir(),
                                     thinking = "off", tools = "",
                                     session = FALSE))
  out <- chat$chat_structured(
    "Country and capital of France.",
    type = list(
      type = "object",
      properties = list(
        country = list(type = "string"),
        capital = list(type = "string")
      ),
      required = I(list("country", "capital"))
    )
  )
  expect_match(out$country, "france", ignore.case = TRUE)
  expect_match(out$capital, "paris", ignore.case = TRUE)
})

test_that("model errors surface as R errors", {
  skip_if_no_pi()
  chat <- chat_pi(client = pi_client(bin = Sys.which("pi"), dir = tempdir(),
                                     model = "no-such-model-xyz", session = FALSE))
  expect_error(chat$chat("hi"), "no-such-model-xyz|exited")
})
