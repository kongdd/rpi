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
#' @param echo One of `"none"`, `"text"` or `"all"`. `"text"` (the default)
#'   streams the assistant answer as it is produced, `"all"` also streams
#'   reasoning and tool calls, `"none"` prints nothing.
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
#' chat$chat("Summarise DESCRIPTION")
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

    #' @description Send a prompt, echo the answer as it is produced, and
    #'   return it once the run has finished. `pi` may call tools in between;
    #'   only the final assistant text is returned.
    #' @param ... Prompt parts, collapsed with newlines.
    #' @param echo Echo mode for this call, see [chat_pi()].
    #' @param on_event Optional function called with every raw `pi` event.
    chat = function(..., echo = NULL, on_event = NULL) {
      prompt <- pi_prompt(...)
      echo <- pi_echo(echo %||% private$echo)
      events <- private$submit(prompt, echo, on_event = on_event)
      text <- pi_text(events)
      if (identical(echo, "none")) text else invisible(text)
    },

    #' @description Like `$chat()`, but returns a promise that resolves to the
    #'   answer.
    #' @param ... Prompt parts, collapsed with newlines.
    #' @param echo Echo mode for this call, see [chat_pi()].
    #' @param on_event Optional function called with every raw `pi` event.
    chat_async = function(..., echo = NULL, on_event = NULL) {
      prompt <- pi_prompt(...)
      echo <- pi_echo(echo %||% private$echo)
      promises::then(
        private$submit(prompt, echo, on_event = on_event, async = TRUE),
        pi_text
      )
    },

    #' @description Ask for structured data. The prompt is answered with a
    #'   single JSON value, which is parsed back into R: arrays of objects
    #'   become data frames, arrays of scalars become vectors, missing
    #'   optional fields become `NA`.
    #' @param ... Prompt parts, collapsed with newlines.
    #' @param type A JSON Schema written as a plain list, or a `type_*()`
    #'   object from \pkg{ellmer}.
    #' @param echo Echo mode for this call, see [chat_pi()].
    #' @return The parsed answer.
    chat_structured = function(..., type, echo = NULL) {
      prompt <- pi_prompt(...)
      echo <- pi_echo(echo %||% private$echo)
      rule <- paste0(
        "Answer with a single JSON value and nothing else: no prose, no ",
        "code fences. The value must validate against this JSON Schema:\n",
        jsonlite::toJSON(pi_schema(type), auto_unbox = TRUE)
      )
      pi_json_parse(pi_text(private$submit(prompt, echo, extra = rule)))
    },

    #' @description All raw events received so far, one list per JSONL record
    #'   of the `pi` event stream.
    get_events = function() {
      private$events
    },

    #' @description The messages of the conversation as a data table with one
    #'   row per message, including token usage and cost. `input` is the whole
    #'   prompt of that turn -- system prompt, tool descriptions, `AGENTS.md`,
    #'   history and the user text -- because `pi` reports it as one number;
    #'   `cache_read` is the part served from the provider cache.
    get_turns = function() {
      pi_turns(private$events)
    },

    #' @description Token usage of every assistant turn, in the shape used by
    #'   \pkg{ellmer}: `input`, `output`, `cached_input` and `cost`. `input`
    #'   covers the full prompt, the system prompt included; use
    #'   `$get_turns()` to see `output`, `reasoning`, `cache_write` and
    #'   `total` as well.
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

    args = function(extra = character()) {
      pi_cli_args(
        self$client,
        session_id = private$session_id,
        model = private$model,
        append = c(private$system_prompt, self$client$append_system_prompt, extra)
      )
    },

    submit = function(prompt, echo = "none", on_event = NULL, async = FALSE,
                      extra = character()) {
      args <- private$args(extra)
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
          cat(pi_display(ame$type, ame$delta, echo), sep = "")
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

#' Echo one delta of the assistant stream
#'
#' In `text` mode only the answer is printed; in `all` mode answer and debate
#' blocks are separated by small markers.
#' @noRd
pi_display <- function(type, delta, echo) {
  if (identical(echo, "text")) return(if (identical(type, "text_delta")) delta else "")
  switch(type,
    thinking_start = "\n-- thinking --\n",
    thinking_delta = delta,
    text_start = "\n-- answer --\n",
    text_delta = delta,
    ""
  )
}

pi_short <- function(x, n = 200) {
  if (is.null(x)) return("")
  txt <- jsonlite::toJSON(x, auto_unbox = TRUE)
  if (nchar(txt) > n) txt <- paste0(substr(txt, 1, n), "...")
  txt
}
