# Structured output ----------------------------------------------------------

#' Coerce a type specification to a JSON Schema
#'
#' `type` is either a JSON Schema written as a plain list, which needs no
#' extra package, or a `type_*()` object of \pkg{ellmer}, whose S7 properties
#' are read directly. `pi` has no schema flag, so the result is only used to
#' instruct the model in the prompt.
#' @noRd
pi_schema <- function(type) {
  if (is.list(type) && !is.null(type$type)) return(type)
  pi_type_schema(type)
}

pi_type_schema <- function(x) {
  cls <- sub("^.*::", "", class(x)[[1]])
  out <- switch(cls,
    TypeObject = {
      props <- lapply(x@properties, pi_type_schema)
      required <- names(props)[vapply(x@properties, pi_is_required, logical(1))]
      list(
        type = "object",
        properties = props,
        required = I(as.list(required))
      )
    },
    TypeArray = list(type = "array", items = pi_type_schema(x@items)),
    TypeEnum = list(type = "string", enum = I(as.list(x@values))),
    TypeBasic = list(type = x@type),
    TypeJsonSchema = x@json,
    stop(
      "`type` must be a JSON Schema list or an {ellmer} `type_*()` object.",
      call. = FALSE
    )
  )
  description <- if (identical(cls, "TypeJsonSchema")) NULL else x@description
  if (!is.null(description)) out$description <- description
  out
}

pi_is_required <- function(prop) isTRUE(prop@required)

#' Parse the JSON value of an assistant answer
#'
#' The answer may wrap the value in a code fence or in prose, so the outermost
#' bracketed region is extracted before parsing.
#' @noRd
pi_json_parse <- function(text) {
  txt <- sub("^```[[:alnum:]]*", "", trimws(text))
  txt <- sub("```$", "", txt)
  start <- regexpr("[\\[{]", txt)
  if (start < 0) {
    stop("The answer contains no JSON value:\n", pi_short(text), call. = FALSE)
  }
  txt <- substring(txt, start)
  end <- max(c(gregexpr("\\}", txt)[[1]], gregexpr("\\]", txt)[[1]]))
  if (end > 0) txt <- substr(txt, 1, end)
  tryCatch(
    jsonlite::fromJSON(txt, simplifyVector = TRUE),
    error = function(e) {
      stop(
        "Cannot parse the JSON answer: ", conditionMessage(e), "\n",
        pi_short(txt),
        call. = FALSE
      )
    }
  )
}
