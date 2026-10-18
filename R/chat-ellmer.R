#' Chat with `pi` through the ellmer interface
#'
#' `ChatPi` is an `ellmer::Chat` whose requests are answered by a `pi`
#' subprocess instead of an HTTP API. Models that `pi` reaches with a
#' subscription login, and therefore without an API key, thus work with the
#' ellmer interface: `$chat()`, `$stream()`, `$chat_async()`,
#' `$stream_async()`, `$get_turns()`, `$get_tokens()` and `$get_cost()`.
#'
#' The conversation lives in a `pi` session: each call sends only the new user
#' text, while history, tools and extensions stay on the `pi` side. Two
#' consequences: `$set_turns()` changes what R reports but not what the model
#' sees, and `$register_tool()` cannot reach `pi` -- select tools with
#' [pi_client()] or a `pi` extension. Structured output is not supported; use
#' [chat_pi()] and `$chat_structured()` for that.
#'
#' @param client A [pi_client()]: binary, working directory, tools, timeout and
#'   extra `pi` arguments.
#' @param model Model name or pattern passed to `pi --model`; `NULL` keeps the
#'   `pi` default.
#' @param system_prompt Appended to the built-in system prompt of `pi`.
#' @param session_id Session id, so that several chat objects share one `pi`
#'   session. A fresh id is used by default.
#' @param echo Echo mode of `ellmer::Chat`: `"none"`, `"output"` or `"all"`.
#' @return A `ChatPi` object, usable wherever an `ellmer::Chat` is expected.
#' @seealso [chat_pi()]
#' @export
#' @examples
#' \dontrun{
#' chat <- chat_pi_ellmer()
#' chat$chat("Summarise DESCRIPTION")
#' chat$stream("Count the files in R/")
#' chat$get_tokens()
#' }
chat_pi_ellmer <- function(client = pi_client(), model = NULL,
                           system_prompt = NULL, session_id = NULL,
                           echo = "none") {
  ChatPi$new(
    client = client,
    model = model,
    system_prompt = system_prompt,
    session_id = session_id,
    echo = echo
  )
}

#' @noRd
ChatPi <- R6::R6Class(
  "ChatPi",
  inherit = ellmer::Chat,
  public = list(
    initialize = function(client = pi_client(), model = NULL,
                          system_prompt = NULL, session_id = NULL,
                          echo = "none") {
      super$initialize(
        provider = ellmer::Provider(
          name = "pi",
          base_url = "https://github.com/earendil-works/pi"
        ),
        model = ellmer::Model(
          name = model %||% "",
          params = list(),
          extra_args = list()
        ),
        system_prompt = system_prompt,
        echo = echo
      )
      private$client <- client
      private$session_id <- if (client$session) session_id %||% pi_session_id()
    }
  ),
  private = list(
    client = NULL,
    session_id = NULL,

    args = function() {
      pi_cli_args(
        private$client,
        session_id = private$session_id,
        model = if (nzchar(private$model@name)) private$model@name,
        append = self$get_system_prompt()
      )
    },

    # ellmer streams via a generator: yield the deltas, then record the turn.
    submit_turns = function(user_turn, stream, echo, type = NULL,
                            yield_as_content = FALSE, controller = NULL,
                            otel_span = NULL) {
      private$check_type(type)
      ctx <- private$context(stream, echo, yield_as_content, controller)
      st <- pi_state()

      coro::generator(function() {
        p <- private$start(user_turn)
        on.exit(if (p$is_alive()) p$kill(), add = TRUE)
        while (pi_running(p)) {
          p$poll_io(50)
          for (line in p$read_output_lines()) {
            value <- private$absorb(pi_absorb(line, st), ctx)
            if (!is.null(value)) yield(value)
          }
          st$stderr <- c(st$stderr, p$read_error_lines())
          private$check_deadline(p, st)
          if (isTRUE(ctx$controller$cancelled)) {
            p$kill()
            break
          }
        }
        private$finish(p, st, user_turn, ctx)
        coro::exhausted()
      })()
    },

    submit_turns_async = function(user_turn, stream, echo, type = NULL,
                                  yield_as_content = FALSE, controller = NULL,
                                  otel_span = NULL) {
      private$check_type(type)
      ctx <- private$context(stream, echo, yield_as_content, controller)
      st <- pi_state()

      coro::async_generator(function() {
        p <- private$start(user_turn)
        on.exit(if (p$is_alive()) p$kill(), add = TRUE)
        while (pi_running(p)) {
          for (line in p$read_output_lines()) {
            value <- private$absorb(pi_absorb(line, st), ctx)
            if (!is.null(value)) yield(value)
          }
          st$stderr <- c(st$stderr, p$read_error_lines())
          private$check_deadline(p, st)
          if (isTRUE(ctx$controller$cancelled)) {
            p$kill()
            break
          }
          if (pi_running(p)) await(pi_sleep())
        }
        private$finish(p, st, user_turn, ctx)
        coro::exhausted()
      })()
    },

    start = function(user_turn) {
      pi_start(private$client, private$args(), user_turn@text)
    },

    context = function(stream, echo, yield_as_content, controller) {
      list(
        stream = stream,
        echo = echo,
        content = isTRUE(yield_as_content),
        controller = controller
      )
    },

    # Echo one delta and return the value the generator should yield, if any.
    absorb = function(ev, ctx) {
      if (is.null(ev)) return(NULL)
      if (identical(ev$type, "thinking") && !identical(ctx$echo, "all")) {
        return(NULL)
      }
      if (!identical(ctx$echo, "none")) cat(ev$delta, sep = "")
      if (!isTRUE(ctx$stream)) return(NULL)
      if (!ctx$content) return(ev$delta)
      if (identical(ev$type, "thinking")) {
        ellmer::ContentThinking(ev$delta)
      } else {
        ellmer::ContentText(ev$delta)
      }
    },

    check_deadline = function(p, st) {
      timeout <- private$client$timeout
      if (!is.finite(timeout)) return(invisible(NULL))
      if (pi_elapsed(st$start) > timeout) {
        p$kill()
        stop(
          "The `pi` run exceeded `timeout` (", timeout, " seconds).",
          call. = FALSE
        )
      }
    },

    check_type = function(type) {
      if (!is.null(type)) {
        stop(
          "`chat_structured()` is not supported by `chat_pi_ellmer()`; ",
          "use `chat_pi()` and `$chat_structured()` instead.",
          call. = FALSE
        )
      }
    },

    # A cancelled run keeps what it produced; a failed one raises.
    finish = function(p, st, user_turn, ctx) {
      if (!isTRUE(ctx$controller$cancelled)) {
        stderr <- paste(st$stderr, collapse = "\n")
        status <- p$get_exit_status()
        if (!identical(status, 0L)) {
          stop("`pi` exited with status ", status, ".\n", stderr, call. = FALSE)
        }
        pi_check_events(st$events, stderr)
      }
      if (!identical(ctx$echo, "none")) cat("\n")
      self$add_turn(user_turn, private$assistant_turn(st))
    },

    assistant_turn = function(st) {
      contents <- list()
      if (nzchar(st$thinking)) {
        contents <- c(contents, list(ellmer::ContentThinking(st$thinking)))
      }
      if (nzchar(st$text)) {
        contents <- c(contents, list(ellmer::ContentText(st$text)))
      }
      ellmer::AssistantTurn(
        contents = contents,
        json = st$last %||% list(),
        tokens = c(
          input = st$input,
          output = st$output,
          cached_input = st$cache_read
        ),
        cost = st$cost,
        duration = st$duration,
        finish_reason = pi_finish_reason(st$last$stopReason)
      )
    }
  )
)

#' Wait a little inside an async generator
#' @noRd
pi_sleep <- function(seconds = 0.05) {
  promises::promise(function(resolve, reject) {
    later::later(function() resolve(NULL), seconds)
  })
}

#' Is the `pi` process still producing output?
#' @noRd
pi_running <- function(p) {
  p$is_alive() || p$is_incomplete_output() || p$is_incomplete_error()
}

#' Accumulator for one `pi` run
#' @noRd
pi_state <- function() {
  st <- new.env(parent = emptyenv())
  st$events <- list()
  st$stderr <- character()
  st$text <- ""
  st$thinking <- ""
  st$last <- NULL
  st$input <- 0
  st$output <- 0
  st$cache_read <- 0
  st$cost <- 0
  st$duration <- 0
  st$start <- Sys.time()
  st
}

#' Absorb one JSONL line, returning the text delta to stream
#'
#' Deltas drive the live output; the authoritative text, thinking and usage
#' come from `message_end`. Tool rounds inside one run are joined, and their
#' usage is summed.
#' @noRd
pi_absorb <- function(line, st) {
  ev <- pi_parse_event(line)
  if (is.null(ev)) return(NULL)
  st$events[[length(st$events) + 1]] <- ev

  if (identical(ev$type, "message_update")) {
    ame <- ev$assistantMessageEvent
    return(switch(ame$type,
      text_delta = list(type = "text", delta = ame$delta),
      thinking_delta = list(type = "thinking", delta = ame$delta),
      NULL
    ))
  }
  if (!identical(ev$type, "message_end")) return(NULL)
  msg <- ev$message
  if (!identical(msg$role, "assistant")) return(NULL)

  usage <- msg$usage %||% list()
  st$last <- msg
  st$text <- pi_join(st$text, pi_content_text(msg$content))
  st$thinking <- pi_join(st$thinking, pi_content_text(msg$content, "thinking"))
  st$input <- pi_add(st$input, usage$input)
  st$output <- pi_add(st$output, usage$output)
  st$cache_read <- pi_add(st$cache_read, usage$cacheRead)
  st$cost <- pi_add(st$cost, usage$cost$total)
  st$duration <- pi_add(st$duration, msg$durationMs)
  NULL
}

#' Join two blocks of text, skipping empty ones
#' @noRd
pi_join <- function(a, b) {
  if (is.null(b) || !nzchar(b)) return(a)
  if (nzchar(a)) paste(a, b, sep = "\n\n") else b
}

pi_add <- function(a, b) if (is.null(b) || is.na(b)) a else a + b

#' Standard ellmer finish reasons
#' @noRd
pi_finish_reason <- function(reason) {
  switch(reason %||% "",
    end_turn = "success",
    stop = "success",
    length = "max_tokens",
    tool_use = "tool_use",
    NA_character_
  )
}
