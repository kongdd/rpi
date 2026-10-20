test_that("collect drives the event loop and preserves a single value", {
  value <- list(text = "hello", empty = NULL)
  p <- promises::then(rpi:::pi_sleep(0.01), function(...) value)
  expect_identical(collect(p), value)
  expect_null(collect(promises::promise_resolve(NULL)))
})

test_that("collect preserves list order, names and NULL values", {
  ps <- list(
    first = promises::then(rpi:::pi_sleep(0.02), function(...) "first"),
    second = promises::promise_resolve("second"),
    empty = promises::promise_resolve(NULL)
  )
  expect_identical(collect(ps), list(first = "first", second = "second", empty = NULL))
  expect_identical(collect(list()), list())
  expect_identical(collect(), list())
})

test_that("collect rethrows the original rejection", {
  error <- simpleError("async failed")
  p <- promises::then(rpi:::pi_sleep(0.01), function(...) stop(error))
  expect_identical(tryCatch(collect(p), error = identity), error)
  expect_error(collect(list(promises::promise_reject(error))), "async failed")
})

test_that("collect supports synchronous and asynchronous generators", {
  g <- coro::generator(function() {
    yield("one")
    yield("two")
  })
  a <- coro::async_generator(function() {
    await(rpi:::pi_sleep(0.01))
    yield("one")
    yield("two")
  })
  expect_identical(collect(g()), list("one", "two"))
  expect_identical(collect(a()), list("one", "two"))
})

test_that("collect completes concurrent chats and releases them", {
  chats <- lapply(LETTERS[1:3], function(label) {
    chat_pi(client = fake_pi_client(timeout = 10), echo = "none", label = label)
  })
  on.exit(lapply(chats, function(chat) chat$close()), add = TRUE)
  ps <- lapply(chats, function(chat) chat$chat_async("hi"))
  capture.output(result <- collect(ps))
  expect_identical(result, rep(list("Hello from pi"), 3))
  for (chat in chats) {
    expect_length(chat$get_turns(), 2)
    expect_silent(chat$set_turns(list()))
  }
})
