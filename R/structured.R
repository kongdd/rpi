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
