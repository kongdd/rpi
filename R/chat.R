#' Chat with the pi coding agent
#'
#' `PiChat` is an R6 class that mirrors the `Chat` class of \pkg{ellmer}, but
#' each conversation is executed by the `pi` command line tool in a working
#' directory. `pi` keeps the conversation history in a session file, so the
#' assistant remembers previous turns and can call its own tools (read, grep,
#' bash, edit, ...) between turns.
#'
#' @param system_prompt Instruction appended to the built-in system prompt of
#'   `pi`, e.g. `"Answer in Chinese."`. `NULL` uses the
#'   `append_system_prompt` of the client. The built-in prompt cannot be
#'   replaced from a chat; use `pi_client(system_prompt = )` for that.
#' @param client A [pi_client()] describing how to start `pi`.
#' @param echo One of `"none"`, `"text"` or `"all"`. `"none"` prints nothing
#'   (the default outside an interactive session), `"text"` streams the
#'   assistant answer, `"all"` also streams reasoning and tool calls.
#' @param session_id Session id to use. Defaults to a fresh id, so every
#'   `PiChat` starts a new `pi` session. Ignored when the client has
#'   `session = FALSE`.
#'
#' @return A `PiChat` object.
#' @seealso [pi_client()]
#' @export
#' @examples
#' \dontrun{
#' chat <- chat_pi("You are a concise assistant.", echo = "text")
#' chat$chat("List the R packages used in this project")
#'
#' # a fresh, tool-less, stateless question
#' chat <- chat_pi(client = pi_client(tools = "read,grep,find,ls", session = FALSE))
#' chat$stream("Summarise DESCRIPTION")
#'
#' chat$get_tokens()
#' chat$get_cost()
#' }
chat_pi <- function(system_prompt = NULL, client = pi_client(), echo = NULL,
                    session_id = NULL) {
  PiChat$new(
    client = client,
    system_prompt = system_prompt,
    echo = echo,
    session_id = session_id
  )
}

#' PiChat class
#'
#' An R6 class that drives a single `pi` conversation. Create it with
#' [chat_pi()]; the methods are documented in the source and listed by
#' `PiChat$new` below.
#'
#' @field client The `pi_client()` behind the conversation.
#' @export
PiChat <- R6Class(
  "PiChat",
  public = list(
    client = NULL,

    #' @description Create a new chat. Prefer `chat_pi()`.
    #' @param client A `pi_client()`.
    #' @param system_prompt System prompt of the conversation, or `NULL`.
    #' @param echo Echo mode: `"none"`, `"text"` or `"all"`.
    #' @param session_id Existing `pi` session id, or `NULL` for a new session.
    initialize = function(client = pi_client(), system_prompt = NULL,
                          echo = NULL, session_id = NULL) {
      if (!inherits(client, "pi_client")) {
        stop("`client` must be created by `pi_client()`.", call. = FALSE)
      }
      self$client <- client
      private$echo <- pi_echo(echo %||% client$echo)
      private$system_prompt <- system_prompt
      private$model <- client$model
      private$session_id <- if (client$session) session_id %||% pi_session_id()
    },

    #' @description Send a prompt and return the answer as a string, after the
    #'   run has finished. `pi` may call tools in between; only the final
    #'   assistant text is returned.
    #' @param ... Prompt parts, collapsed with newlines.
    #' @param echo Echo mode for this call, see [chat_pi()].
    chat = function(..., echo = NULL) {
      prompt <- pi_prompt(...)
      echo <- pi_echo(echo %||% private$echo)
      events <- private$submit(prompt, echo)
      text <- pi_text(events)
      if (identical(echo, "none")) text else invisible(text)
    },

    #' @description Send a prompt and echo the answer while it is produced.
    #'   Unlike the generator returned by `ellmer::Chat$stream()`, this blocks
    #'   and returns the complete answer.
    #' @param ... Prompt parts, collapsed with newlines.
    #' @param on_event Optional function called with every raw `pi` event.
    #' @param echo Echo mode for this call, see [chat_pi()].
    stream = function(..., on_event = NULL, echo = NULL) {
      prompt <- pi_prompt(...)
      echo <- pi_echo(echo %||% "text")
      events <- private$submit(prompt, echo, on_event = on_event)
      invisible(pi_text(events))
    },

    #' @description Like `$chat()`, but returns a promise that resolves to the
    #'   answer.
    #' @param ... Prompt parts, collapsed with newlines.
    chat_async = function(...) {
      prompt <- pi_prompt(...)
      promises::then(private$submit(prompt, "none", async = TRUE), pi_text)
    },

    #' @description Like `$stream()`, but returns a promise that resolves to the
    #'   answer once the run has finished.
    #' @param ... Prompt parts, collapsed with newlines.
    #' @param on_event Optional function called with every raw `pi` event.
    #' @param echo Echo mode for this call, see [chat_pi()].
    stream_async = function(..., on_event = NULL, echo = NULL) {
      prompt <- pi_prompt(...)
      echo <- pi_echo(echo %||% "text")
      promises::then(
        private$submit(prompt, echo, on_event = on_event, async = TRUE),
        pi_text
      )
    },

    #' @description All raw events received so far, one list per JSONL record
    #'   of the `pi` event stream.
    get_events = function() {
      private$events
    },

    #' @description The messages of the conversation as a data table with one
    #'   row per message, including token usage and cost.
    get_turns = function() {
      pi_turns(private$events)
    },

    #' @description Token usage of every assistant turn, in the shape used by
    #'   \pkg{ellmer}: `input`, `output`, `cached_input` and `cost`.
    get_tokens = function() {
      turns <- self$get_turns()
      keep <- turns$role == "assistant"
      data.table::data.table(
        input = turns$input[keep],
        output = turns$output[keep],
        cached_input = turns$cache_read[keep],
        cost = turns$cost[keep]
      )
    },

    #' @description Total cost in USD of the conversation so far.
    get_cost = function() {
      sum(self$get_tokens()$cost, na.rm = TRUE)
    },

    #' @description Instructions appended to the built-in system prompt, or
    #'   `NULL`.
    get_system_prompt = function() {
      private$system_prompt
    },

    #' @description Replace the appended instruction. It applies to the next
    #'   call.
    #' @param value New instruction, or `NULL` to drop it.
    set_system_prompt = function(value) {
      pi_check_string(value, "system_prompt")
      private$system_prompt <- value
      invisible(self)
    },

    #' @description The model used by the next call.
    get_model = function() {
      private$model
    },

    #' @description Switch the model of the conversation.
    #' @param model Model name or pattern.
    set_model = function(model) {
      pi_check_string(model, "model")
      private$model <- model
      invisible(self)
    },

    #' @description The `pi` session id backing the conversation, or `NULL`
    #'   when the client runs without sessions.
    get_session_id = function() {
      private$session_id
    },

    #' @description The directory the agent works in.
    get_dir = function() {
      self$client$dir
    },

    #' @description Prompt for the next `pi` command, for debugging.
    get_command = function() {
      paste(c(shQuote(self$client$bin), private$args()), collapse = " ")
    },

    #' @description Print a short summary of the conversation.
    #' @param ... Ignored.
    print = function(...) {
      turns <- self$get_turns()
      cat("<pi_chat>\n")
      cat("  model:   ", private$model %||% "(pi default)", "\n", sep = "")
      cat("  dir:     ", self$client$dir, "\n", sep = "")
      cat("  session: ", private$session_id %||% "off", "\n", sep = "")
      cat("  turns:   ", sum(turns$role == "assistant"), "\n", sep = "")
      invisible(self)
    }
  ),
  private = list(
    model = NULL,
    system_prompt = NULL,
    session_id = NULL,
    echo = NULL,
    events = list(),

    args = function() {
      pi_cli_args(
        self$client,
        session_id = private$session_id,
        model = private$model,
        append = c(private$system_prompt, self$client$append_system_prompt)
      )
    },

    submit = function(prompt, echo = "none", on_event = NULL, async = FALSE) {
      args <- private$args()
      cb <- private$display(echo, on_event)
      timeout <- if (is.finite(self$client$timeout)) self$client$timeout

      if (async) {
        promise <- pi_run_async(self$client, args, prompt, cb, timeout)
        return(promises::then(promise, function(events) {
          private$events <- c(private$events, events)
          events
        }))
      }

      events <- pi_run(self$client, args, prompt, cb, timeout)
      private$events <- c(private$events, events)
      if (!identical(echo, "none") && nzchar(pi_text(events))) cat("\n")
      events
    },

    # Combine the echo display with an optional user callback.
    display = function(echo, on_event = NULL) {
      if (identical(echo, "none")) return(on_event)
      show <- function(ev) {
        if (identical(ev$type, "message_update")) {
          ame <- ev$assistantMessageEvent
          if (identical(ame$type, "text_delta")) {
            cat(ame$delta, sep = "")
          } else if (identical(echo, "all") && identical(ame$type, "thinking_delta")) {
            cat(ame$delta, sep = "")
          }
        } else if (identical(echo, "all") && identical(ev$type, "tool_execution_start")) {
          cat("\n[tool] ", ev$toolName, ": ", pi_short(ev$args), "\n", sep = "")
        }
      }
      if (is.null(on_event)) show else function(ev) { show(ev); on_event(ev) }
    }
  )
)

pi_prompt <- function(...) {
  parts <- list(...)
  if (length(parts) == 0) {
    stop("A prompt is required.", call. = FALSE)
  }
  prompt <- paste(vapply(parts, as.character, character(1)), collapse = "\n")
  pi_check_prompt(prompt)
  prompt
}

pi_short <- function(x, n = 200) {
  if (is.null(x)) return("")
  txt <- jsonlite::toJSON(x, auto_unbox = TRUE)
  if (nchar(txt) > n) txt <- paste0(substr(txt, 1, n), "...")
  txt
}
