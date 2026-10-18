# Shared small helpers -------------------------------------------------------
# Internal helpers -----------------------------------------------------------

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Locate the pi executable
#' @noRd
pi_bin <- function(bin = NULL) {
  bin <- bin %||% getOption("piagent.bin")
  if (is.null(bin)) {
    bin <- unname(Sys.which("pi"))
    if (!nzchar(bin)) {
      file <- if (.Platform$OS.type == "windows") "pi.exe" else "pi"
      cand <- file.path(Sys.getenv("USERPROFILE", "~"), ".bun", "bin", file)
      bin <- cand[file.exists(cand)]
    }
  }
  if (length(bin) == 0 || !nzchar(bin[[1]])) {
    stop("Cannot find the `pi` executable; pass `pi_client(bin = )`.")
  }
  bin[[1]]
}

#' A session id accepted by `pi --session-id`
#' @noRd
pi_session_id <- function() {
  paste0(
    "r", format(Sys.time(), "%Y%m%d%H%M%S"), "-",
    paste(sample(c(letters, 0:9), 6, replace = TRUE), collapse = "")
  )
}

pi_check_string <- function(x, arg) {
  if (!is.null(x) && (!is.character(x) || length(x) != 1 || is.na(x))) {
    stop("`", arg, "` must be a single string or NULL.", call. = FALSE)
  }
  x
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

#' @noRd
pi_echo <- function(echo) {
  if (is.null(echo)) {
    echo <- getOption("piagent.echo", "text")
  }
  echo <- match.arg(echo, c("none", "text", "all"))
  if (length(echo) == 0) echo <- "none"
  echo
}
