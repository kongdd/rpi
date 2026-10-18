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
    stop("Cannot find the `pi` executable; pass `pi_client(bin = )`.", call. = FALSE)
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
  if (is.null(x)) return("")
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

# Command line arguments -----------------------------------------------------

#' Build the `pi` command line for one call
#'
#' `append` is a character vector of prompts, one `--append-system-prompt` flag
#' each.
#' @noRd
pi_cli_args <- function(client, session_id = NULL, model = NULL,
                        append = character()) {
  c(
    "--mode", "json",
    pi_arg("--model", model %||% client$model),
    pi_arg("--provider", client$provider),
    pi_arg("--thinking", client$thinking),
    pi_arg("--tools", client$tools),
    pi_arg("--exclude-tools", client$exclude_tools),
    pi_arg("--system-prompt", client$system_prompt),
    pi_arg_rep("--append-system-prompt", append),
    pi_arg("--session-dir", client$session_dir),
    if (client$session && !is.null(session_id)) {
      c("--session-id", session_id)
    } else {
      "--no-session"
    },
    client$args
  )
}

pi_arg <- function(flag, value) {
  if (is.null(value) || length(value) == 0) return(NULL)
  c(flag, as.character(value))
}

pi_arg_rep <- function(flag, values) {
  values <- values[!is.na(values) & nzchar(values)]
  if (length(values) == 0) return(NULL)
  as.vector(rbind(flag, values))
}

# Event stream ---------------------------------------------------------------

#' @noRd
pi_parse_event <- function(line) {
  if (!nzchar(line)) return(NULL)
  tryCatch(
    jsonlite::fromJSON(line, simplifyVector = FALSE),
    error = function(e) NULL
  )
}

pi_content_text <- function(content, type = "text") {
  field <- if (type == "text") "text" else "thinking"
  if (is.null(content)) return("")
  if (is.character(content)) return(paste(content, collapse = ""))
  parts <- vapply(
    content,
    function(x) if (identical(x$type, type)) x[[field]] %||% "" else "",
    character(1)
  )
  paste(parts, collapse = "")
}

#' Turn the assistant stop reason into an R error
#' @noRd
pi_check_events <- function(events, stderr = "") {
  msg <- NULL
  for (i in rev(seq_along(events))) {
    ev <- events[[i]]
    if (identical(ev$type, "message_end") && identical(ev$message$role, "assistant")) {
      msg <- ev$message
      break
    }
  }
  if (is.null(msg)) {
    stop("The `pi` run produced no assistant message.\n", stderr, call. = FALSE)
  }
  reason <- msg$stopReason
  if (identical(reason, "error") || identical(reason, "aborted")) {
    stop(
      "The `pi` run ended with stop reason `", reason, "`",
      if (!is.null(msg$errorMessage)) paste0(": ", msg$errorMessage),
      call. = FALSE
    )
  }
  if (identical(reason, "length")) {
    warning("The `pi` response was cut off at the model output token limit.", call. = FALSE)
  }
  invisible(events)
}

# Running pi -----------------------------------------------------------------
#' Split a prompt into command line arguments
#'
#' A `@file` reference must be its own `pi` argument: `pi` attaches everything
#' after `@` up to the end of the argument. Whitespace-delimited tokens that
#' start with `@` therefore leave the prompt text, and the remaining text is
#' joined back with single spaces.
#' @noRd
pi_prompt_args <- function(prompt) {
  tokens <- strsplit(prompt, "[[:space:]]+")[[1]]
  if (!any(startsWith(tokens, "@"))) return(prompt)
  out <- character(); txt <- ""
  for (tok in tokens) {
    if (startsWith(tok, "@")) {
      if (nzchar(txt)) { out <- c(out, txt); txt <- "" }
      out <- c(out, tok)
    } else {
      txt <- if (nzchar(txt)) paste(txt, tok, sep = " ") else tok
    }
  }
  c(out, if (nzchar(txt)) txt)
}

#' Start a `pi` process in JSON mode
#'
#' The prompt travels on the command line so that `@file` references are
#' resolved by `pi`; stdin is an empty, closed pipe so that `pi` never waits
#' for terminal input.
#' @noRd
pi_start <- function(client, args, prompt, files = character()) {
  file_args <- if (length(files)) paste0("@", files)
  p <- processx::process$new(
    client$bin, c(args, "--", pi_prompt_args(prompt), file_args),
    stdin = "|", stdout = "|", stderr = "|",
    wd = client$dir, env = client$env
  )
  close(p$get_input_connection())
  p
}
