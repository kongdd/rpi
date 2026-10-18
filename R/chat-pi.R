#' Chat with pi through the ellmer interface
#'
#' `ChatPi` inherits from [ellmer::Chat], but runs pi instead of calling an
#' HTTP API. Authentication, tools and extensions are managed by pi.
#'
#' History is read from the active branch of the pi session, including tool
#' requests and results. `$set_turns()` forks a new session containing the
#' supplied history; the original file is never overwritten. Compaction and
#' extension metadata remain available through `$get_events()`.
#'
#' Structured output uses prompt instructions, not provider-enforced schemas.
#' Inline and remote images are supported. Other attachments should be passed
#' as `@file` references for pi tools to read. R tools are not supported.
#' With `client$session = FALSE`, temporary session files are used only within
#' this R object and removed by `$close()` (or garbage collection).
#'
#' @param client A [pi_client()].
#' @param model Model pattern; `NULL` uses the client or pi default.
#' @param system_prompt Instructions appended to pi's built-in prompt.
#' @param session_id A pi session ID to resume, or `NULL` for a new session.
#' @param echo `"none"`, `"text"`, `"output"` or `"all"`; defaults to live text.
#' @return A `ChatPi`, inheriting from `ellmer::Chat`.
#' @export
#' @examples
#' \dontrun{
#' chat <- chat_pi_ellmer()
#' chat$chat("Summarise DESCRIPTION")
#' chat$get_turns()
#' chat$get_tokens()
#' }
chat_pi_ellmer <- function(client = pi_client(), model = NULL,
                           system_prompt = NULL, session_id = NULL,
                           echo = "text") {
  ChatPi$new(client, model, system_prompt, session_id, echo)
}

#' @noRd
#' Chat with pi through the ellmer interface
#'
#' `ChatPi` inherits from [ellmer::Chat], but runs pi instead of calling an
#' HTTP API. Authentication, tools and extensions are managed by pi.
#'
#' History is read from the active branch of the pi session, including tool
#' requests and results. `$set_turns()` forks a new session containing the
#' supplied history; the original file is never overwritten. Compaction and
#' extension metadata remain available through `$get_events()`.
#'
#' Structured output uses prompt instructions, not provider-enforced schemas.
#' Inline and remote images are supported. Other attachments should be passed
#' as `@file` references for pi tools to read. R tools are not supported.
#' With `client$session = FALSE`, temporary session files are used only within
#' this R object and removed by `$close()` (or garbage collection).
#'
#' @param client A [pi_client()].
#' @param model Model pattern; `NULL` uses the client or pi default.
#' @param system_prompt Instructions appended to pi's built-in prompt.
#' @param session_id A pi session ID to resume, or `NULL` for a new session.
#' @param echo `"none"`, `"text"`, `"output"` or `"all"`; defaults to live text.
#' @return A `ChatPi`, inheriting from `ellmer::Chat`.
#' @export
#' @examples
#' \dontrun{
#' chat <- chat_pi_ellmer()
#' chat$chat("Summarise DESCRIPTION")
#' chat$get_turns()
#' chat$get_tokens()
#' }
chat_pi_ellmer <- function(client = pi_client(), model = NULL,
                           system_prompt = NULL, session_id = NULL,
                           echo = "text") {
  ChatPi$new(client, model, system_prompt, session_id, echo)
}

#' @noRd
# nolint start: object_name_linter, object_usage_linter.
ChatPi <- R6::R6Class(
  "ChatPi",
  inherit = ellmer::Chat,
  public = list(
    initialize = function(client = pi_client(), model = NULL,
                          system_prompt = NULL, session_id = NULL,
                          echo = "text") {
      if (!inherits(client, "pi_client")) {
        stop("`client` must be a pi_client()", call. = FALSE)
      }
      pi_flags <- "^--(no-session|session|session-id|session-dir)(=|$)"
      if (any(grepl(pi_flags, client$args))) {
        stop(
          "Configure sessions with pi_client() and session_id,",
          "not client$args.",
          call. = FALSE
        )
      }

      super$initialize(
        provider = ellmer::Provider(name = "pi", base_url = ""),
        model = ellmer::Model(name = model %||% client$model %||% ""),
        system_prompt = system_prompt, echo = echo
      )
      private$client <- client
      private$echo_mode <- match.arg(echo, c("none", "text", "output", "all"))
      private$store <- pi_history_store(client, session_id)
      private$sync()
    },
    chat = function(..., echo = NULL) {
      echo <- echo %||% private$echo_mode
      coro::collect(private$run(pi_user_turn(...), echo = echo))
      text <- self$last_turn()@text
      if (echo == "none") text else invisible(text)
    },
    stream = function(..., type = NULL, stream = c("text", "content"),
                      controller = NULL) {
      stream <- match.arg(stream)
      private$run(pi_user_turn(...),
        type = type,
        content = stream == "content", controller = controller
      )
    },
    chat_async = function(..., tool_mode = c("concurrent", "sequential")) {
      match.arg(tool_mode)
      done <- coro::async_collect(private$run(pi_user_turn(...),
        async = TRUE,
        echo = private$echo_mode
      ))
      promises::then(done, function(x) self$last_turn()@text)
    },
    stream_async = function(..., type = NULL,
                            tool_mode = c("concurrent", "sequential"),
                            stream = c("text", "content"), controller = NULL) {
      match.arg(tool_mode)
      stream <- match.arg(stream)
      private$run(pi_user_turn(...),
        type = type, async = TRUE,
        content = stream == "content", controller = controller
      )
    },
    chat_structured = function(..., type, echo = "none", convert = TRUE) {
      coro::collect(private$run(pi_user_turn(...), type = type, echo = echo))
      pi_extract(self$last_turn()@text, type, convert)
    },
    chat_structured_async = function(..., type, echo = "none", convert = TRUE) {
      done <- coro::async_collect(private$run(
        pi_user_turn(...),
        type = type, echo = echo, async = TRUE
      ))
      promises::then(done, function(x) {
        pi_extract(self$last_turn()@text, type, convert)
      })
    },
    set_turns = function(value) {
      private$assert_idle()
      old <- super$get_turns(TRUE)
      prompt <- self$get_system_prompt()
      super$set_turns(value)
      ok <- FALSE
      on.exit(if (!ok) {
        super$set_system_prompt(prompt)
        super$set_turns(old)
      }, add = TRUE)
      messages <- lapply(super$get_turns(), pi_turn_message)
      path <- pi_history_fork(private$store, private$client$dir, messages)
      private$store$path <- path
      ok <- TRUE
      invisible(self)
    },
    add_turn = function(user, assistant, log_tokens = TRUE) {
      self$set_turns(c(self$get_turns(), list(user, assistant)))
    },
    get_cost = function() sum(self$get_tokens()$cost, na.rm = TRUE),
    get_tokens = function(include_system_prompt = FALSE) {
      turns <- Filter(function(x) x@role == "assistant", self$get_turns())
      if (!length(turns)) {
        return(data.table::data.table(
          input = numeric(), output = numeric(),
          cached_input = numeric(), cost = numeric()
        ))
      }
      data.table::rbindlist(lapply(turns, function(x) {
        data.table::data.table(
          input = x@tokens[[1]], output = x@tokens[[2]],
          cached_input = x@tokens[[3]], cost = x@cost
        )
      }))
    },
    get_events = function() pi_history_read(private$store$path),
    get_session_id = function() pi_history_read(private$store$path)[[1]]$id,
    register_tool = function(...) {
      stop(
        "Configure tools through pi_client() or pi extensions,",
        "not R functions.",
        call. = FALSE
      )
    },
    register_tools = function(...) self$register_tool(...),
    close = function() {
      private$assert_idle()
      if (!private$client$session) unlink(private$store$files)
      private$closed <- TRUE
      invisible(self)
    }
  ),
  private = list(
    client = NULL, store = NULL, echo_mode = NULL, busy = FALSE, closed = FALSE,
    assert_idle = function() {
      if (private$closed) stop("This chat has been closed.", call. = FALSE)
      if (private$busy) stop("A pi request is already running.", call. = FALSE)
    },
    sync = function() {
      messages <- pi_history_messages(private$store$path)
      turns <- pi_message_turns(messages)
      super$set_turns(turns)
    },
    run = function(user, type = NULL, echo = "none", content = FALSE,
                   controller = NULL, async = FALSE) {
      echo <- match.arg(echo, c("none", "text", "output", "all"))
      has_controller <- !is.null(controller) &&
        !inherits(controller, "ellmer_stream_controller")
      if (has_controller) {
        stop("`controller` must be an ellmer::stream_controller().")
      }
      force(user)
      if (!is.null(type) && !length(user@contents)) {
        user <- ellmer::UserTurn(paste(
          "Extract the structured answer",
          "from the conversation above."
        ))
      }
      force(content)
      started <- FALSE
      rule <- if (!is.null(type)) {
        paste0(
          "Return only one JSON value matching this JSON Schema: ",
          jsonlite::toJSON(pi_schema(type), auto_unbox = TRUE)
        )
      }
      # Construct the process inside the iterator: unused streams do no work.
      start <- function() {
        private$assert_idle()
        private$sync()
        private$busy <- TRUE
        started <<- TRUE
        cl <- private$client
        st <- pi_stream_state(controller, echo, content, cl$timeout)
        st$turns <- super$get_turns()
        if (isTRUE(controller$cancelled)) {
          st$cancelled <- TRUE
          return(st)
        }
        input <- pi_turn_input(user)
        st$files <- input$files
        ok <- FALSE
        on.exit(if (!ok) unlink(input$files), add = TRUE)
        cl$session <- FALSE
        args <- pi_cli_args(cl,
          model = if (nzchar(self$get_model())) self$get_model(),
          append = c(self$get_system_prompt(), cl$append_system_prompt, rule)
        )
        args <- args[args != "--no-session"]
        st$p <- pi_start(cl, c(args, "--session", private$store$path),
          input$text,
          files = input$files
        )
        ok <- TRUE
        st
      }
      cleanup <- function(st) {
        if (!is.null(st)) {
          if (!is.null(st$p) && st$p$is_alive()) st$p$kill_tree()
          unlink(st$files)
        }
        if (started) private$busy <- FALSE
      }
      finish <- function(st) {
        pi_stream_stop(st)
        pi_stream_check(st)
        if (st$cancelled) {
          partial <- ellmer::AssistantPartialTurn(contents = st$text)
          super$set_turns(c(st$turns, list(user, partial)))
        } else {
          private$sync()
        }
        if (echo != "none") cat("\n")
      }
      pi_stream_iterator(start, cleanup, finish, async)
    }
  )
)

#' Chat with the pi coding agent
#'
#' [chat_pi()] is a thin alias for [chat_pi_ellmer()]: the same `ChatPi`
#' object, driven by a `pi` subprocess. See [chat_pi_ellmer()] for the
#' available methods.
#'
#' @param system_prompt Instructions appended to pi's built-in prompt.
#' @param client A [pi_client()].
#' @param echo `"none"`, `"text"`, `"output"` or `"all"`.
#' @param session_id A pi session ID to resume, or `NULL` for a new session.
#' @return A `ChatPi`, inheriting from `ellmer::Chat`.
#' @export
#' @examples
#' \dontrun{
#' chat <- chat_pi("Answer briefly.")
#' chat$chat("Which functions in R/ are unused?")
#' chat$get_tokens()
#' }
chat_pi <- function(system_prompt = NULL, client = pi_client(), echo = NULL,
                    session_id = NULL) {
  ChatPi$new(
    client, NULL, system_prompt, session_id,
    echo %||% client$echo %||% "text"
  )
}
# nolint end.
