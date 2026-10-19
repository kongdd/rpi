pi_user_turn <- function(...) {
  contents <- lapply(list(...), function(x) {
    if (is.character(x)) ContentText(paste(x, collapse = "\n")) else x
  })
  ellmer::UserTurn(contents)
}

pi_turn_input <- function(turn) {
  files <- character()
  ok <- FALSE
  on.exit(if (!ok) unlink(files), add = TRUE)
  text <- character()
  for (x in turn@contents) {
    if (inherits(x, "ellmer::ContentText")) {
      text <- c(text, x@text)
    } else if (inherits(x, "ellmer::ContentImageInline")) {
      ext <- sub("^image/", "", x@type)
      path <- tempfile("pi-image-", fileext = paste0(".", ext))
      files <- c(files, path)
      writeBin(jsonlite::base64_dec(x@data), path)
    } else if (inherits(x, "ellmer::ContentImageRemote")) {
      path <- tempfile("pi-image-", fileext = ".png")
      files <- c(files, path)
      utils::download.file(x@url, path, mode = "wb", quiet = TRUE)
    } else {
      stop(
        "Unsupported attachment. Use an @file reference for pi tools to read.",
        call. = FALSE
      )
    }
  }
  text <- paste(text, collapse = "\n")
  if (!nzchar(trimws(text)) && !length(files)) {
    stop("A prompt is required.", call. = FALSE)
  }
  ok <- TRUE
  list(text = text, files = files)
}

pi_block_content <- function(block) {
  switch(block$type,
    text = ContentText(block$text %||% ""),
    thinking = ContentThinking(block$thinking %||% ""),
    image = ellmer::ContentImageInline(block$mimeType, block$data),
    toolCall = ellmer::ContentToolRequest(
      id = block$id, name = block$name, arguments = block$arguments,
      extra = list(pi = block)
    ),
    stop("Unknown pi content block: ", block$type, call. = FALSE)
  )
}

pi_message_turns <- function(messages) {
  requests <- list()
  lapply(messages, function(msg) {
    if (identical(msg$role, "toolResult")) {
      request <- requests[[msg$toolCallId]]
      result <- ellmer::ContentToolResult(
        value = pi_content_text(msg$content),
        error = if (isTRUE(msg$isError)) pi_content_text(msg$content),
        request = request, extra = list(pi = msg)
      )
      return(ellmer::UserTurn(list(result)))
    }
    blocks <- if (is.character(msg$content)) {
      list(list(type = "text", text = msg$content))
    } else {
      msg$content
    }
    contents <- lapply(blocks, pi_block_content)
    if (identical(msg$role, "user")) {
      return(ellmer::UserTurn(contents))
    }
    for (x in contents) {
      if (inherits(x, "ellmer::ContentToolRequest")) requests[[x@id]] <<- x
    }
    usage <- msg$usage %||% list()
    AssistantTurn(
      contents = contents, json = msg,
      tokens = c(
        input = (usage$input %||% NA_real_) + (usage$cacheWrite %||% 0),
        output = usage$output %||% NA_real_,
        cached_input = usage$cacheRead %||% 0
      ),
      cost = as.numeric(usage$cost$total %||% NA_real_),
      duration = (msg$durationMs %||% NA_real_) / 1000,
      finish_reason = switch(msg$stopReason %||% "",
        stop = "success",
        end_turn = "success",
        length = "max_tokens",
        toolUse = "tool_use",
        NA_character_
      )
    )
  })
}

pi_content_block <- function(x) {
  if (inherits(x, "ellmer::ContentText")) {
    return(list(type = "text", text = x@text))
  }
  if (inherits(x, "ellmer::ContentThinking")) {
    return(list(type = "thinking", thinking = x@thinking))
  }
  if (inherits(x, "ellmer::ContentImageInline")) {
    mime <- if (startsWith(x@type, "image/")) {
      x@type
    } else {
      paste0("image/", x@type)
    }
    return(list(type = "image", mimeType = mime, data = x@data))
  }
  if (inherits(x, "ellmer::ContentToolRequest")) {
    return(x@extra$pi %||% list(
      type = "toolCall", id = x@id, name = x@name,
      arguments = x@arguments
    ))
  }
  stop("Cannot restore this content to pi history.", call. = FALSE)
}

pi_turn_message <- function(turn) {
  contents <- turn@contents
  if (length(contents) == 1L && inherits(
    contents[[1]], "ellmer::ContentToolResult"
  )) {
    result <- contents[[1]]
    if (!is.null(result@extra$pi)) {
      return(result@extra$pi)
    }
    return(list(
      role = "toolResult", toolCallId = result@request@id,
      toolName = result@request@name,
      content = list(list(type = "text", text = as.character(result@value))),
      isError = !is.null(result@error),
      timestamp = as.numeric(Sys.time()) * 1000
    ))
  }
  if (!turn@role %in% c("user", "assistant")) {
    stop("Only user/assistant/tool turns can be restored to pi.", call. = FALSE)
  }
  # Preserve provider replay metadata and signatures for untouched turns.
  if (turn@role == "assistant" && identical(turn@json$role, "assistant")) {
    original <- pi_message_turns(list(turn@json))[[1]]
    if (identical(original@contents, turn@contents)) {
      return(turn@json)
    }
  }
  msg <- list(
    role = turn@role, content = lapply(turn@contents, pi_content_block),
    timestamp = as.numeric(Sys.time()) * 1000
  )
  if (turn@role == "assistant") {
    msg$api <- "openai-responses"
    msg$provider <- "piagent"
    msg$model <- "imported"
    msg$stopReason <- "stop"
    msg$usage <- list(
      input = 0, output = 0, cacheRead = 0, cacheWrite = 0,
      totalTokens = 0, cost = list(
        input = 0, output = 0,
        cacheRead = 0, cacheWrite = 0, total = 0
      )
    )
  }
  msg
}

# Parsing and type conversion are shared with ellmer's own structured output;
# `as_json()` (generic) and `convert_from_type()` (internal) cover both halves.
pi_extract <- function(text, type, convert) {
  if (!is.logical(convert) || length(convert) != 1L || is.na(convert)) {
    stop("`convert` must be TRUE or FALSE.", call. = FALSE)
  }
  parse <- function(simplify) {
    txt <- sub("\\s*```$", "", sub("^```[[:alnum:]]*\\s*", "", trimws(text)))
    fromJSON(txt, simplifyVector = simplify)
  }
  if (!convert) {
    return(parse(FALSE))
  }
  convert <- get("convert_from_type", asNamespace("ellmer"))
  convert(parse(TRUE), type)
}
