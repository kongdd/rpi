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

test_that("JSON is extracted from prose and code fences", {
  expect_identical(pi_json_parse('{"a": 1}'), list(a = 1L))
  expect_identical(pi_json_parse('ok:\n```json\n{"a": 1}\n```'), list(a = 1L))
  expect_identical(
    pi_json_parse('Here you go:\n[{"a": 1}, {"a": 2}]\nDone.'),
    data.frame(a = c(1L, 2L))
  )
})

test_that("a JSON-free or broken answer fails clearly", {
  expect_error(pi_json_parse("no data"), "no JSON value")
  expect_error(pi_json_parse("{broken"), "Cannot parse")
})
