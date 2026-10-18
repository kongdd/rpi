# Pi v3 JSONL is the boundary; ellmer's private generator methods are not used.
pi_home <- function() {
  if (.Platform$OS.type == "windows") {
    Sys.getenv("USERPROFILE")
  } else {
    path.expand("~")
  }
}

pi_history_store <- function(client, id = NULL) {
  store <- new.env(parent = emptyenv())
  store$persistent <- client$session
  store$files <- character()
  if (client$session) {
    dir <- client$session_dir
    if (is.null(dir)) {
      root <- Sys.getenv(
        "PI_CODING_AGENT_DIR", file.path(pi_home(), ".pi", "agent")
      )
      key <- gsub("[/\\\\:]", "-", sub("^[/\\\\]", "", client$dir))
      dir <- Sys.getenv(
        "PI_CODING_AGENT_SESSION_DIR",
        file.path(root, "sessions", paste0("--", key, "--"))
      )
    }
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    store$dir <- normalizePath(dir, winslash = "/", mustWork = TRUE)
    candidates <- list.files(dir, pattern = "\\.jsonl$", full.names = TRUE)
    if (!is.null(id)) {
      found <- Filter(function(path) {
        tryCatch(
          {
            header <- jsonlite::fromJSON(readLines(path, n = 1L, warn = FALSE))
            identical(header$id, id)
          },
          error = function(e) FALSE
        )
      }, candidates)
      if (length(found) > 1L) stop("Ambiguous pi session ID.", call. = FALSE)
      if (length(found)) {
        store$path <- found[[1]]
        return(store)
      }
    }
  } else {
    dir <- tempdir()
  }
  store$dir <- normalizePath(dir, winslash = "/", mustWork = TRUE)
  store$path <- pi_history_fork(store, client$dir, list(), id = id)
  if (!client$session) {
    reg.finalizer(store, function(x) unlink(x$files), onexit = TRUE)
  }
  store
}

pi_history_read <- function(path) {
  if (!file.exists(path)) {
    stop("Pi session file is missing: ", path, call. = FALSE)
  }
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  entries <- lapply(
    lines[nzchar(lines)], jsonlite::fromJSON,
    simplifyVector = FALSE
  )
  v3 <- identical(entries[[1]]$type, "session") &&
    identical(as.integer(entries[[1]]$version), 3L)
  if (!length(entries) || !v3) {
    stop("Expected a pi v3 session file: ", path, call. = FALSE)
  }
  entries
}

# Walk the active leaf, not every branch. Raw messages retain signatures and
# tool metadata; compaction/context edits are left for pi to apply to context.
pi_history_messages <- function(path) {
  entries <- pi_history_read(path)[-1]
  if (!length(entries)) {
    return(list())
  }
  ids <- vapply(entries, function(x) x$id, character(1))
  current <- length(entries)
  branch <- list()
  seen <- integer()
  while (!is.na(current)) {
    if (current %in% seen) stop("Cycle in pi session history.", call. = FALSE)
    seen <- c(seen, current)
    entry <- entries[[current]]
    branch <- c(list(entry), branch)
    current <- match(entry$parentId %||% "", ids)
  }
  messages <- lapply(
    Filter(function(x) identical(x$type, "message"), branch),
    function(x) x$message
  )
  Filter(function(x) x$role %in% c("user", "assistant", "toolResult"), messages)
}

# Fork on replacement: never destroy the original session or extensions.
pi_history_fork <- function(store, cwd, messages, id = NULL) {
  id <- id %||% pi_session_id()
  if (length(id) != 1L || is.na(id) || !grepl("^[A-Za-z0-9_-]+$", id)) {
    stop("Invalid pi session ID.", call. = FALSE)
  }
  timestamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
  header <- list(
    type = "session", version = 3L, id = id,
    timestamp = timestamp, cwd = cwd
  )
  if (!is.null(store$path)) header$parentSession <- store$path
  entries <- list(header)
  parent <- NULL
  for (i in seq_along(messages)) {
    entry_id <- sprintf("%08x", i)
    entries[[length(entries) + 1L]] <- list(
      type = "message", id = entry_id, parentId = parent,
      timestamp = timestamp, message = messages[[i]]
    )
    parent <- entry_id
  }
  name <- paste0(gsub(":", "-", timestamp), "_", id, ".jsonl")
  path <- file.path(store$dir, name)
  if (file.exists(path)) {
    stop("Refusing to overwrite an existing pi session.", call. = FALSE)
  }
  lines <- vapply(entries, function(x) {
    as.character(jsonlite::toJSON(
      x,
      auto_unbox = TRUE, null = "null", digits = NA
    ))
  }, character(1))
  writeLines(enc2utf8(lines), path, useBytes = TRUE)
  Sys.chmod(path, "0600")
  store$files <- c(store$files, path)
  path
}
