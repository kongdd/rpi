# Shared small helpers -------------------------------------------------------
# Internal helpers -----------------------------------------------------------

`%||%` <- function(x, y) if (is.null(x)) y else x

#' A session id accepted by `pi --session-id`
#' @noRd
pi_session_id <- function() {
  paste0(
    "r", format(Sys.time(), "%Y%m%d%H%M%S"), "-",
    paste(sample(c(letters, 0:9), 6, replace = TRUE), collapse = "")
  )
}

pi_short <- function(x, n = 200) {
  if (is.null(x)) {
    return("")
  }
  txt <- jsonlite::toJSON(x, auto_unbox = TRUE)
  if (nchar(txt) > n) txt <- paste0(substr(txt, 1, n), "...")
  txt
}

pi_elapsed <- function(start) {
  as.numeric(difftime(Sys.time(), start, units = "secs"))
}
