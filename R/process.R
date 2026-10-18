# Running `pi` and parsing its JSONL event stream ----------------------------

#' @noRd
pi_parse_event <- function(line) {
  if (!nzchar(line)) {
    return(NULL)
  }
  tryCatch(
    jsonlite::fromJSON(line, simplifyVector = FALSE),
    error = function(e) NULL
  )
}

pi_content_text <- function(content, type = "text") {
  field <- if (type == "text") "text" else "thinking"
  if (is.null(content)) {
    return("")
  }
  if (is.character(content)) {
    return(paste(content, collapse = ""))
  }
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
    is_assistant <- identical(ev$type, "message_end") &&
      identical(ev$message$role, "assistant")
    if (is_assistant) {
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
    warning("The `pi` response was cut off at the model output token limit.")
  }
  invisible(events)
}

#' Split a prompt into command line arguments
#'
#' A `@file` reference must be its own `pi` argument: `pi` attaches everything
#' after `@` up to the end of the argument. Whitespace-delimited tokens that

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
