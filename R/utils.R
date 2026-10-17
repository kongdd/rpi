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

pi_check_prompt <- function(x) {
  if (!is.character(x) || length(x) != 1 || is.na(x) || !nzchar(x)) {
    stop("`prompt` must be a single non-empty string.", call. = FALSE)
  }
  invisible(x)
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

#' Final assistant text of a run
#' @noRd
pi_text <- function(events) {
  for (i in rev(seq_along(events))) {
    ev <- events[[i]]
    if (identical(ev$type, "message_end") && identical(ev$message$role, "assistant")) {
      return(pi_content_text(ev$message$content))
    }
  }
  ""
}

pi_last_assistant <- function(events) {
  for (i in rev(seq_along(events))) {
    ev <- events[[i]]
    if (identical(ev$type, "message_end") && identical(ev$message$role, "assistant")) {
      return(ev$message)
    }
  }
  NULL
}

#' Turn the assistant stop reason into an R error
#' @noRd
pi_check_events <- function(events, stderr = "") {
  msg <- pi_last_assistant(events)
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

#' Turn the accumulated events into a message data table
#' @noRd
pi_turns <- function(events) {
  rows <- list()
  for (ev in events) {
    if (!identical(ev$type, "message_end")) next
    msg <- ev$message
    if (!msg$role %in% c("user", "assistant")) next
    usage <- msg$usage %||% list()
    rows[[length(rows) + 1]] <- data.table::data.table(
      role = msg$role,
      text = pi_content_text(msg$content),
      thinking = pi_content_text(msg$content, "thinking"),
      model = msg$model %||% NA_character_,
      input = as.numeric(usage$input %||% NA),
      output = as.numeric(usage$output %||% NA),
      cache_read = as.numeric(usage$cacheRead %||% NA),
      cache_write = as.numeric(usage$cacheWrite %||% NA),
      reasoning = as.numeric(usage$reasoning %||% NA),
      total = as.numeric(usage$totalTokens %||% NA),
      cost = as.numeric(usage$cost$total %||% NA),
      duration = as.numeric(msg$durationMs %||% NA)
    )
  }
  if (length(rows) == 0) {
    out <- data.table::data.table(
      role = character(), text = character(), thinking = character(),
      model = character(), input = numeric(), output = numeric(),
      cache_read = numeric(), cache_write = numeric(), reasoning = numeric(),
      total = numeric(), cost = numeric(),
      duration = numeric()
    )
  } else {
    out <- data.table::rbindlist(rows)
  }
  out
}

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
pi_start <- function(client, args, prompt) {
  p <- processx::process$new(
    client$bin, c(args, "--", pi_prompt_args(prompt)),
    stdin = "|", stdout = "|", stderr = "|",
    wd = client$dir, env = client$env
  )
  close(p$get_input_connection())
  p
}

#' Collect the event stream of one run, blocking until it finishes
#' @noRd
pi_run <- function(client, args, prompt, on_event = NULL, timeout = NULL) {
  p <- pi_start(client, args, prompt)
  on.exit(if (p$is_alive()) p$kill(), add = TRUE)
  events <- list()
  stderr <- character()
  start <- Sys.time()

  while (p$is_alive() || p$is_incomplete_output() || p$is_incomplete_error()) {
    p$poll_io(200)
    for (line in p$read_output_lines()) {
      ev <- pi_parse_event(line)
      if (!is.null(ev)) {
        events[[length(events) + 1]] <- ev
        if (!is.null(on_event)) on_event(ev)
      }
    }
    stderr <- c(stderr, p$read_error_lines())

    if (!is.null(timeout) && pi_elapsed(start) > timeout) {
      p$kill()
      stop("The `pi` run exceeded `timeout` (", timeout, " seconds).", call. = FALSE)
    }
  }

  status <- p$get_exit_status()
  stderr <- paste(stderr, collapse = "\n")
  if (!identical(status, 0L)) {
    stop("`pi` exited with status ", status, ".\n", stderr, call. = FALSE)
  }
  pi_check_events(events, stderr)
  events
}

#' The asynchronous flavour of `pi_run()`
#' @noRd
pi_run_async <- function(client, args, prompt, on_event = NULL, timeout = NULL) {
  p <- pi_start(client, args, prompt)
  events <- list()
  stderr <- character()
  start <- Sys.time()

  promises::promise(function(resolve, reject) {
    step <- function() {
      ok <- tryCatch({
        for (line in p$read_output_lines()) {
          ev <- pi_parse_event(line)
          if (!is.null(ev)) {
            events[[length(events) + 1]] <<- ev
            if (!is.null(on_event)) on_event(ev)
          }
        }
        stderr <<- c(stderr, p$read_error_lines())
        TRUE
      }, error = function(e) {
        reject(e)
        FALSE
      })
      if (!ok) return(invisible(NULL))

      if (!is.null(timeout) && pi_elapsed(start) > timeout) {
        p$kill()
        reject(stop("The `pi` run timed out.", call. = FALSE))
      } else if (p$is_alive() || p$is_incomplete_output() || p$is_incomplete_error()) {
        later::later(step, 0.02)
      } else if (!identical(p$get_exit_status(), 0L)) {
        reject(stop(
          "`pi` exited with status ", p$get_exit_status(), ".\n",
          paste(stderr, collapse = "\n"), call. = FALSE
        ))
      } else {
        err <- tryCatch(
          { pi_check_events(events, paste(stderr, collapse = "\n")); NULL },
          error = function(e) e
        )
        if (is.null(err)) resolve(events) else reject(err)
      }
    }
    step()
  })
}
