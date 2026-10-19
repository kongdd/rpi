#' Summarise conversation turns as a data.table
#'
#' @param object A list of turns returned by `$get_turns()`.
#' @param ... Unused.
#' @return A `data.table` with one row per turn: role, content type, tool
#'   names, input/output tokens, cached input tokens, cost in USD, and text.
#'   Tool results have role `"tool"`. Text includes tool calls and results;
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
  rbindlist(lapply(object, function(turn) {
    blocks <- rbindlist(lapply(turn@contents, pi_summary_block))
    text <- blocks$text[blocks$type != "thinking" & nzchar(blocks$text)]
    if (!length(text)) text <- blocks$text[blocks$type == "thinking"]
    assistant <- inherits(turn, "ellmer::AssistantTurn")
    tokens <- if (assistant) turn@tokens else rep(NA_real_, 3L)
    data.table(
      role = if (any(blocks$type == "tool_result")) "tool" else turn@role,
      type = paste(unique(blocks$type), collapse = ","),
      tool = paste(blocks$tool[nzchar(blocks$tool)], collapse = ", "),
      input = tokens[[1]], output = tokens[[2]], cached_input = tokens[[3]],
      cost = if (assistant) turn@cost else NA_real_,
      text = paste(text, collapse = "\n")
    )
  }))
}

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

#' @rdname summary.pi_turns
#' @param x A list of turns returned by `$get_turns()`.
#' @export
print.pi_turns <- function(x, ...) {
  print(summary(x), ...)
  invisible(x)
}

options(tibble.width = 80) # tibble 默认宽度
