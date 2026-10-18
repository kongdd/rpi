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
  ChatPi$new(client, NULL, system_prompt, session_id,
             echo %||% client$echo %||% "text")
}
