#' Chat directly with Codex using pi's saved OAuth credentials
#'
#' Unlike [chat_pi()], this uses ellmer's HTTP transport and R tools,
#' not a pi subprocess. Log in to `openai-codex` in pi first. Credentials are
#' read before each request from `~/.pi/agent/auth.json`, the path used by pi.
#'
#' The authorization file is never modified. Expired credentials must be
#' refreshed in pi before retrying. Only Codex OAuth credentials are supported;
#' these are not API keys for `api.openai.com`.
#'
#' Codex requires streaming, including when `echo = "none"`. The usual ellmer
#' chat, asynchronous and R tool interfaces remain available.
#' Use `$chat()` to send prompts; `$stream()` is disabled.
#' `$stream_async()` remains available as an asynchronous generator.
#' Parameters must be supported by Codex; for example, `max_tokens` is not.
#' OpenAI-specific file upload and batch endpoints are not supported by Codex.
#'
#' @param system_prompt System instructions.
#' @param model Codex model ID.
#' @param params Model parameters from [ellmer::params()].
#' @param echo `"none"`, `"output"` or `"all"`.
#' @return An object inheriting from [ellmer::Chat].
#' @export
#' @examples
#' \dontrun{
#' chat <- chat_codex(model = "gpt-6-luna", system_prompt = "Be concise.")
#' chat$chat("What is 1 + 1?")
#' }
chat_codex <- function(model, system_prompt = NULL, params = NULL,
                        echo = c("none", "output", "all")) {
  echo <- match.arg(echo)
  chat <- ellmer::chat_openai(
    base_url = "https://chatgpt.com/backend-api/codex",
    credentials = \() pi_codex_credentials(pi_auth_file()),
    model = model, params = params, echo = echo,
    api_args = list(
      instructions = "You are a helpful assistant.",
      service_tier = NULL
    ),
    api_headers = c(originator = "pi", `OpenAI-Beta` = "responses=experimental")
  )
  ChatPiCodex$new(
    provider = chat$get_provider(), model = chat$get_model_object(),
    system_prompt = system_prompt, echo = echo
  )
}

# Codex only returns SSE; force ellmer's stream parser even for silent calls.
# nolint start: object_name_linter, object_usage_linter.
ChatPiCodex <- R6Class(
  "ChatPiCodex",
  inherit = Chat,
  public = list(
    # Block ellmer's inherited method; use chat() for console output.
    stream = NULL,
    get_turns = function(include_system_prompt = FALSE) {
      structure(super$get_turns(include_system_prompt), class = c("pi_turns", "list"))
    }
  ),
  private = list(
    submit_turns = function(user_turn, stream, ...) {
      super$submit_turns(user_turn, stream = TRUE, ...)
    },
    submit_turns_async = function(user_turn, stream, ...) {
      super$submit_turns_async(user_turn, stream = TRUE, ...)
    }
  )
)

pi_auth_file <- function() path.expand("~/.pi/agent/auth.json")

pi_codex_credentials <- function(path) {
  fail <- function(message) {
    stop(paste0(message, " Run `/login openai-codex` in pi."), call. = FALSE)
  }
  cred <- tryCatch(
    fromJSON(path, simplifyVector = FALSE)[["openai-codex"]],
    error = \(e) fail("Cannot read pi's authorization file.")
  )
  ok <- identical(cred$type, "oauth") &&
    all(c("access", "accountId", "expires") %in% names(cred))
  if (!ok) fail("No Codex OAuth credentials.")
  if (cred$expires <= as.numeric(Sys.time()) * 1000) {
    fail("Codex OAuth credentials expired.")
  }
  list(
    Authorization = paste("Bearer", cred$access),
    `chatgpt-account-id` = cred$accountId
  )
}
# nolint end.
