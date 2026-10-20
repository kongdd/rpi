#' Summarise conversation turns as a data.table
#'
#' @param object A list of turns returned by `$get_turns()`.
#' @param ... Unused.
#' @return A `data.table` with one row per non-tool turn: role, content type,
#'   tool names, input/output tokens, cached input tokens, cost in USD, and text.
#'   Tool result rows are omitted. Text includes tool calls;
#'   thinking is shown only when no other nonempty content is available.
#'   Unavailable usage is `NA`; summarising does not alter the conversation.
#' @export
summary.pi_turns <- function(object, ...) {
  if (!length(object)) {
    return(data.table(
      role = character(), type = character(), tool = character(),
      input = numeric(), output = numeric(), cached_input = numeric(),
      cost = numeric(), text = character()
    ))
  }
  result <- rbindlist(lapply(object, pi_summary_turn))
  result[result$role != "tool"]
}

#' Summarise a chat's token usage
#'
#' @param object A chat object with a `$get_turns()` method.
#' @param ... Unused.
#' @return A `data.table` summarising the chat turns.
#' @export
summary.Chat <- function(object, ...) {
  turns <- object$get_turns()
  show_status(turns, label = if (inherits(object, "ChatPi")) object$label)
  summary(turns)
}

pi_summary_turn <- function(turn) {
  blocks <- rbindlist(lapply(turn@contents, pi_summary_block))
  types <- blocks$type
  text <- blocks$text[types != "thinking" & nzchar(blocks$text)]
  if (!length(text)) text <- blocks$text[types == "thinking"]
  assistant <- inherits(turn, "ellmer::AssistantTurn")
  tokens <- if (assistant) turn@tokens else rep(NA_real_, 3L)
  data.table(
    role = if ("tool_result" %in% types) "tool" else turn@role,
    type = paste(unique(types), collapse = ","),
    tool = paste(blocks$tool[nzchar(blocks$tool)], collapse = ", "),
    input = tokens[[1]], output = tokens[[2]], cached_input = tokens[[3]],
    cost = if (assistant) turn@cost else NA_real_,
    text = paste(text, collapse = "\n")
  )
}

# 每个turn的内容快
pi_summary_block <- function(x) {
  kind <- sub("^ellmer::Content", "", class(x)[[1]])
  type <- switch(kind,
    Text = "text", Thinking = "thinking",
    ToolRequest = "tool_call", ToolResult = "tool_result", "attachment"
  )
  tool <- switch(type,
    tool_call = x@name,
    tool_result = if (!is.null(x@request)) x@request@name else x@extra$pi$toolName %||% "",
    ""
  )
  text <- switch(type,
    text = x@text,
    thinking = x@thinking,
    tool_call = paste0(
      "[", tool, "] ", toJSON(x@arguments, auto_unbox = TRUE, null = "null", digits = NA)
    ),
    tool_result = {
      value <- x@error %||% x@value
      value <- if (is.character(value)) paste(value, collapse = "\n") else
        as.character(toJSON(value, auto_unbox = TRUE, null = "null", digits = NA))
      if (!is.null(x@error)) paste0("[error] ", value) else value
    },
    paste0("<", kind, ">")
  )
  list(type = type, tool = tool, text = text)
}

#' Format the conversation usage summary
#'
#' @param s A `pi_turns` object or its `summary()` table.
#' @param label Optional tag printed before the line, useful to tell apart
#'   the output of several chats running at the same time.
#' @return Prints token totals, cache hit rate, context used/limit, and cost.
#' @export
show_status <- function(s, label = NULL) {
  pi_console_write(
    pi_console_id(), pi_tag(label),
    paste0(green(pi_status_line(s)), "\n")
  )
}

# 状态行文本：解析会话并算出上下文上限
pi_status_line <- function(s) {
  limit <- attr(s, "context_limit")
  turns <- if (inherits(s, "pi_turns")) s else NULL
  if (!is.null(turns)) {
    s <- summary(turns)
    if (is.null(limit)) {
      assistant <- utils::tail(Filter(
        \(turn) inherits(turn, "ellmer::AssistantTurn"), turns
      ), 1L)
      if (length(assistant)) {
        message <- assistant[[1]]@json
        if (!is.null(message$provider) && !is.null(message$model)) {
          model <- paste(message$provider, message$model, sep = "/")
          limit <- ContextLenMax(pi_client(), model)
        }
      }
    }
  }
  columns <- c("input", "output", "cached_input", "cost")
  totals <- vapply(columns, \(name) sum(s[[name]], na.rm = TRUE), numeric(1))
  latest <- utils::tail(which(s$role == "assistant"), 1L)
  prompt <- s$input[latest] + s$cached_input[latest]
  hit <- if (length(prompt) && is.finite(prompt) && prompt > 0) {
    sprintf("%.1f%%", 100 * s$cached_input[latest] / prompt)
  } else "-"
  used <- if (length(prompt) && is.finite(prompt)) format_tokens(prompt) else "0"
  limit <- tolower(limit %||% "?")
  sprintf("↑%s ↓%s R%s CH%s %s/%s $%.3f",
    format_tokens(totals[["input"]]), format_tokens(totals[["output"]]),
    format_tokens(totals[["cached_input"]]), hit, used, limit, totals[["cost"]]
  )
}

#' @rdname summary.pi_turns
#' @details Printing turns appends the line returned by `show_status()`.
#' @param x A list of turns returned by `$get_turns()`.
#' @export
print.pi_turns <- function(x, ...) {
  print(summary(x), ...)
  invisible(x)
}
