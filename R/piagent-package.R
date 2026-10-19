#' piagent: chat with the pi coding agent from R
#'
#' An \pkg{ellmer}-style interface to the
#' [pi](https://github.com/earendil-works/pi) coding agent: [chat_pi()]
#' turns a working directory over to a `pi` subprocess that can read files,
#' run shell commands, and edit code:
#'
#' ```r
#' chat <- chat_pi(client = pi_client(dir = "~/my-project"))
#' chat$chat("Which functions in R/ are unused?")
#' ```
#'
#' @keywords internal
#' @importFrom coro async_collect async_generator await collect exhausted
#' @importFrom coro generator loop yield
#' @importFrom ellmer AssistantTurn Chat ContentText ContentThinking
#' @importFrom ellmer Model Provider
#' @importFrom jsonlite fromJSON
#' @importFrom R6 R6Class
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
