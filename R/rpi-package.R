#' rpi: chat with the pi coding agent from R
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
#' @importFrom crayon blue green
#' @importFrom ellmer AssistantTurn Chat ContentText ContentThinking
#' @importFrom ellmer Model Provider
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom R6 R6Class
#' @importFrom data.table data.table rbindlist
#' @importFrom magrittr %>% %<>%
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

options(tibble.width = 80) # tibble 默认宽度

format_tokens <- function(n) {
  if (n < 1e3) return(as.character(n))
  if (n < 1e4) return(sprintf("%.1fk", n / 1e3))
  if (n < 1e6) return(sprintf("%.0fk", n / 1e3))
  if (n < 1e7) return(sprintf("%.1fM", n / 1e6))
  sprintf("%.0fM", n / 1e6)
}
