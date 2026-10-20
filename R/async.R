#' Drive the event loop until results are ready
#'
#' Asynchronous methods (`$chat_async()`, `$stream_async()`) return promises
#' or generators. `collect()` runs the `later` event loop until they finish,
#' then returns their values, so they read like ordinary calls.
#'
#' @param x A promise, a list of promises, or a generator.
#' @return A promise yields its value, a list of promises a list of values in
#'   the same order, a generator the list of yielded values. A rejected
#'   promise rethrows its error.
#' @export
#' @examples
#' \dontrun{
#' chats <- lapply(1:3, \(i) chat_pi(client = pi_client(dir = ".")))
#' ps <- lapply(chats, \(chat) chat$chat_async("你是什么模型"))
#' unlist(collect(ps))
#' }
collect <- function(x = NULL) {
  if (is.null(x)) return(list())
  if (inherits(x, "coro_generator_instance")) x <- coro::async_collect(x)
  single <- promises::is.promise(x)
  done <- FALSE
  out <- error <- NULL
  promises::then(
    promises::promise_all(.list = if (single) list(x) else x),
    function(value) {
      out <<- value
      done <<- TRUE
      NULL
    },
    function(err) {
      error <<- err
      done <<- TRUE
      NULL
    }
  )
  while (!done) later::run_now(0.05)
  if (!is.null(error)) stop(error)
  if (single) out[[1]] else out
}
