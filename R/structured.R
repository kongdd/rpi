# Structured output ----------------------------------------------------------

#' Coerce a type specification to a JSON Schema for the prompt
#'
#' A JSON Schema list passes through unchanged; a `type_*()` object of
#' \pkg{ellmer} is converted by the S7 `as_json()` generic's provider method.
#' `pi` has no schema flag, so the result only instructs the model.
#' @noRd
pi_schema <- function(type) {
  if (is.list(type) && !is.null(type$type)) {
    return(type)
  }
  as_json <- get("as_json", asNamespace("ellmer"))
  as_json(Provider(name = "pi", base_url = ""), type)
}

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
    fromJSON(txt, simplifyVector = TRUE),
    error = function(e) {
      stop(
        "Cannot parse the JSON answer: ", conditionMessage(e), "\n",
        pi_short(txt),
        call. = FALSE
      )
    }
  )
}
