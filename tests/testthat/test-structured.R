test_that("a plain JSON Schema is passed through", {
  schema <- list(type = "object", properties = list(a = list(type = "number")))
  expect_identical(pi_schema(schema), schema)
})

unlisted <- function(x) lapply(x, function(y) if (is.list(y)) unlist(y) else y)

test_that("ellmer types are converted to a JSON Schema", {
  skip_if_not_installed("ellmer")

  type <- ellmer::type_object(
    name = ellmer::type_string(),
    score = ellmer::type_number(description = "0-1"),
    kind = ellmer::type_enum(c("a", "b")),
    tags = ellmer::type_array(ellmer::type_string(), required = FALSE)
  )
  schema <- pi_schema(type)

  expect_identical(schema$type, "object")
  expect_identical(names(schema$properties), c("name", "score", "kind", "tags"))
  expect_identical(schema$properties$score$description, "0-1")
  expect_identical(unlisted(schema$required), list("name", "score", "kind"))
  expect_identical(unlisted(schema$properties$kind$enum), list("a", "b"))
  expect_identical(schema$properties$tags$items$type, "string")
})

test_that("unsupported types fail clearly", {
  expect_error(pi_schema(1), "Can't find method")
})

test_that("collect preserves concurrent structured results and field types", {
  type <- ellmer::type_object(
    name = ellmer::type_string(),
    age = ellmer::type_integer()
  )
  chats <- lapply(1:3, function(i) {
    chat_pi(client = fake_pi_client(
      timeout = 10,
      env = c(FAKE_PI_JSON = sprintf('{"name":"person","age":%d}', i))
    ))
  })
  on.exit(lapply(chats, function(chat) chat$close()), add = TRUE)
  ps <- lapply(chats, function(chat) {
    chat$chat_structured_async("Extract name and age", type = type)
  })
  capture.output(result <- collect(ps))
  expect_identical(result, lapply(1:3, function(i) list(name = "person", age = i)))
})
