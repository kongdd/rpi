#' piagent: chat with the pi coding agent from R
#'
#' An \pkg{ellmer}-style interface to the [pi](https://github.com/earendil-works/pi)
#' coding agent. Configure the executable once with [pi_client()], then talk to
#' an agent that can read files, run shell commands, and edit code inside a
#' working directory:
#'
#' ```r
#' chat <- chat_pi(client = pi_client(dir = "~/my-project"))
#' chat$chat("Which functions in R/ are unused?")
#' ```
#'
#' @keywords internal
#' @importFrom R6 R6Class
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
