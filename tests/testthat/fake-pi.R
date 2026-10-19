# Offline CLI fixture: persists a real v3 session and exposes received context.
args <- commandArgs(trailingOnly = TRUE)
if ("--list-models" %in% args) {
  cat("provider model context max-out thinking images\n")
  cat("fake fake-model 272K 1K no no\n")
  quit(save = "no", status = 0)
}
flag <- function(name) {
  i <- match(name, args)
  if (!is.na(i)) args[[i + 1L]] else NULL
}
path <- flag("--session")
entries <- if (!is.null(path)) {
  lapply(
    readLines(path, warn = FALSE), jsonlite::fromJSON,
    simplifyVector = FALSE
  )
} else {
  list()
}
if (nzchar(Sys.getenv("FAKE_PI_CAPTURE"))) {
  saveRDS(list(args = args, entries = entries), Sys.getenv("FAKE_PI_CAPTURE"))
}
to_json <- function(x) {
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"))
}
emit <- function(x) {
  cat(to_json(x), "\n", sep = "")
  flush(stdout())
}
record <- function(msg) {
  emit(list(type = "message_end", message = msg))
  if (!is.null(path)) {
    n <- length(entries)
    entry <- list(
      type = "message", id = sprintf("%08x", n),
      parentId = if (n > 1L) entries[[n]]$id else NULL,
      timestamp = "2026-01-01T00:00:00Z", message = msg
    )
    entries[[n + 1L]] <<- entry
    cat(as.character(jsonlite::toJSON(entry, auto_unbox = TRUE, null = "null")),
      "\n",
      sep = "", file = path, append = TRUE
    )
  }
}
assistant <- function(text, tool = FALSE) {
  list(
    role = "assistant", content = if (tool) {
      list(
        list(type = "text", text = "Reading"),
        list(
          type = "toolCall", id = "call-1", name = "read",
          arguments = list(path = "R/a.R")
        )
      )
    } else {
      list(list(type = "text", text = text))
    },
    api = "openai-responses", provider = "fake", model = "fake-model",
    usage = list(
      input = 100, output = 5, cacheRead = 20, cacheWrite = 0,
      totalTokens = 125, cost = list(total = 0.25)
    ),
    durationMs = 500, stopReason = if (tool) "toolUse" else "stop"
  )
}
if (identical(Sys.getenv("FAKE_PI_ERROR"), "1")) {
  emit(list(type = "message_end", message = list(
    role = "assistant", content = list(),
    stopReason = "error", errorMessage = "boom"
  )))
  quit(status = 0)
}
sleep <- function() {
  if (nzchar(Sys.getenv("FAKE_PI_SLEEP"))) {
    Sys.sleep(as.numeric(Sys.getenv("FAKE_PI_SLEEP")))
  }
}
sleep()
prompt <- args[seq.int(match("--", args) + 1L, length(args))]
record(list(role = "user", content = paste(prompt, collapse = " "), timestamp = 0))
if (identical(Sys.getenv("FAKE_PI_TOOLS"), "1")) {
  record(assistant("", tool = TRUE))
  emit(list(
    type = "tool_execution_start", toolName = "read",
    args = list(path = "R/a.R")
  ))
  record(list(
    role = "toolResult", toolCallId = "call-1", toolName = "read",
    content = list(list(type = "text", text = "file contents")), isError = FALSE
  ))
}
text <- Sys.getenv("FAKE_PI_JSON", "Hello from pi")
if (identical(Sys.getenv("FAKE_PI_THINKING"), "1")) {
  emit(list(type = "message_update", assistantMessageEvent = list(
    type = "thinking_delta", delta = "Let me think"
  )))
  emit(list(type = "message_update", assistantMessageEvent = list(
    type = "thinking_end", content = "Let me think"
  )))
}
emit(list(type = "message_update", assistantMessageEvent = list(
  type = "text_delta", delta = text
)))
sleep()
record(assistant(text))
