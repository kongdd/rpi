#' Summarise conversation turns as a data.table
#'
#' @param object A list of turns returned by `$get_turns()`.
#' @param ... Unused.
#' @return A `data.table` with one row per turn: role, text, input/output
#'   tokens, cached input tokens, and cost in USD. Unavailable usage is `NA`.
#'   Text excludes thinking, tool calls, and other non-text content.
#' @export
summary.pi_turns <- function(object, ...) {
  if (!length(object)) {
    return(data.table::data.table(
      role = character(), text = character(), input = numeric(),
      output = numeric(), cached_input = numeric(), cost = numeric()
    ))
  }
  data.table::rbindlist(lapply(object, function(turn) {
    assistant <- inherits(turn, "ellmer::AssistantTurn")
    tokens <- if (assistant) turn@tokens else rep(NA_real_, 3L)
    data.table::data.table(
      role = turn@role, 
      input = tokens[[1]], output = tokens[[2]], cached_input = tokens[[3]],
      cost = if (assistant) turn@cost else NA_real_, 
      text = turn@text
    )
  }))
}

#' @rdname summary.pi_turns
#' @param x A list of turns returned by `$get_turns()`.
#' @export
print.pi_turns <- function(x, ...) {
  print(summary(x), ...)
  invisible(x)
}

options(tibble.width = 80) # tibble 默认宽度
