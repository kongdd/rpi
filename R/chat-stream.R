pi_stream_state <- function(controller, echo, content, timeout) {
  st <- new.env(parent = emptyenv())
  st$controller <- controller
  st$echo <- echo
  st$content <- content
  st$timeout <- timeout
  st$start <- Sys.time()
  st$events <- list()
  st$stderr <- character()
  st$text <- ""
  st$cancelled <- FALSE
  st$files <- character()
  st$p <- NULL
  st
}

pi_stream_stop <- function(st) {
  if (isTRUE(st$controller$cancelled)) {
    st$cancelled <- TRUE
    if (!is.null(st$p) && st$p$is_alive()) st$p$kill_tree()
    return(TRUE)
  }
  if (is.finite(st$timeout) && pi_elapsed(st$start) > st$timeout) {
    stop(
      "The `pi` run exceeded `timeout` (", st$timeout, " seconds).",
      call. = FALSE
    )
  }
  FALSE
}

pi_stream_poll <- function(st, wait = FALSE) {
  if (wait) st$p$poll_io(50)
  values <- list()
  for (line in st$p$read_output_lines()) {
    ev <- pi_parse_event(line)
    if (is.null(ev)) next
    st$events[[length(st$events) + 1L]] <- ev
    if (identical(ev$type, "message_update")) {
      ame <- ev$assistantMessageEvent
      thinking <- identical(ame$type, "thinking_delta")
      text <- identical(ame$type, "text_delta")
      if (!thinking && !text) next
      if (text) st$text <- paste0(st$text, ame$delta)
      if ((text && st$echo != "none") || (thinking && st$echo == "all")) {
        cat(ame$delta, sep = "")
        utils::flush.console()
      }
      if (!text && !st$content) next
      value <- if (!st$content) {
        ame$delta
      } else if (thinking) {
        ContentThinking(ame$delta)
      } else {
        ContentText(ame$delta)
      }
      values[[length(values) + 1L]] <- value
    } else if (identical(ev$type, "tool_execution_start") && st$echo == "all") {
      cat("\n[tool] ", ev$toolName, ": ", pi_short(ev$args), "\n", sep = "")
      utils::flush.console()
    }
  }
  st$stderr <- c(st$stderr, st$p$read_error_lines())
  values
}

pi_stream_check <- function(st) {
  if (st$cancelled) {
    return(invisible(NULL))
  }
  stderr <- paste(st$stderr, collapse = "\n")
  status <- st$p$get_exit_status()
  if (!identical(status, 0L)) {
    stop("`pi` exited with status ", status, ".\n", stderr, call. = FALSE)
  }
  pi_check_events(st$events, stderr)
}

pi_stream_iterator <- function(start, cleanup, finish, async) {
  if (!async) {
    return(generator(function() {
      st <- NULL
      on.exit(cleanup(st), add = TRUE)
      st <- start()
      while (pi_running(st$p)) {
        if (pi_stream_stop(st)) break
        values <- pi_stream_poll(st, wait = TRUE)
        for (value in values) {
          if (pi_stream_stop(st)) break
          yield(value)
        }
      }
      finish(st)
      exhausted()
    })())
  }
  async_generator(function() {
    st <- NULL
    on.exit(cleanup(st), add = TRUE)
    st <- start()
    while (pi_running(st$p)) {
      if (pi_stream_stop(st)) break
      values <- pi_stream_poll(st)
      for (value in values) {
        if (pi_stream_stop(st)) break
        yield(value)
      }
      if (pi_running(st$p)) await(pi_sleep())
    }
    finish(st)
    exhausted()
  })()
}

pi_sleep <- function(seconds = 0.05) {
  promises::promise(function(resolve, reject) {
    later::later(\() resolve(NULL), seconds)
  })
}

pi_running <- function(p) {
  !is.null(p) && (
    p$is_alive() || p$is_incomplete_output() || p$is_incomplete_error()
  )
}
