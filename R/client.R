#' Create a pi client
#'
#' A client records how to start the `pi` command line tool: working directory,
#' model, and extra options. Like a provider in \pkg{ellmer}, it is the stateless part
#' of the interface from which any number of [chat_pi()] conversations can be
#' created.
#'
#' @param dir Working directory of the agent. `pi` reads project instructions,
#'   resolves `@file` references, and groups sessions by this directory.
#' @param model Model name or a pattern such as `"provider/model:thinking"`.
#'   `NULL` uses the model configured in `pi` settings.
#' @param provider Restrict model lookup to one provider. Requires `model`.
#' @param thinking Thinking level: `"off"`, `"minimal"`, `"low"`, `"medium"`,
#'   `"high"`, `"xhigh"`, or `"max"`.
#' @param tools Comma separated tool allowlist, e.g. `"read,grep,find,ls"`.
#'   Entries starting with `+` or `-` adjust the default selection instead.
#' @param exclude_tools Comma separated tool names or patterns to disable.
#' @param system_prompt Replaces the built-in system prompt of `pi`. This is
#'   rarely what you want, because the built-in prompt teaches the model to use
#'   the tools; use `append_system_prompt` instead.
#' @param append_system_prompt Extra instructions appended to the built-in
#'   system prompt, without disabling the agent tools.
#' @param session If `TRUE` (default) the conversation is persisted as a `pi`
#'   session and remembered across calls; if `FALSE` every call starts fresh.
#' @param session_dir Directory used to store and look up sessions.
#' @param env Named character vector of environment variables for the `pi`
#'   process, or `NULL` to inherit the current environment.
#' @param args Character vector of extra command line arguments passed to `pi`
#'   verbatim, e.g. `c("-a", "--no-mcp")`.
#' @param timeout Seconds allowed for one run, or `Inf` for no limit.
#' @param echo Default echo mode for [chat_pi()]: `"none"`, `"text"`,
#'   `"output"` or `"all"`.
#'
#' @return An object of class `pi_client`.
#' @seealso [chat_pi()]
#' @export
#' @examples
#' \dontrun{
#' client <- pi_client(dir = "~/projects/demo", model = "sonnet")
#' chat <- chat_pi("You are a terse R reviewer.", client = client)
#' chat$chat("What does this repository do?")
#' }
pi_client <- function(
  thinking = NULL,
  dir = ".", model = NULL, provider = NULL,
  tools = NULL, exclude_tools = NULL,
  system_prompt = NULL, append_system_prompt = NULL,
  session = TRUE, session_dir = NULL, env = NULL,
  args = character(), timeout = Inf, echo = "text"
) {
  dir <- normalizePath(dir, mustWork = TRUE)

  structure(
    list(
      dir = dir,
      model = model,
      provider = provider,
      thinking = thinking,
      tools = tools,
      exclude_tools = exclude_tools,
      system_prompt = system_prompt,
      append_system_prompt = append_system_prompt,
      session = isTRUE(session),
      session_dir = session_dir,
      env = env,
      args = as.character(args),
      timeout = timeout,
      echo = match.arg(echo, c("none", "text", "output", "all"))
    ),
    class = "pi_client"
  )
}

#' Create a minimal pi client
#'
#' Uses only read, bash, edit, write, and codemode. Disables automatic loading of
#' extensions, skills, and project instructions; explicitly enables codemode.
#'
#' @inheritParams pi_client
#' @param ... Additional arguments passed to [pi_client()].
#' @return An object of class `pi_client`.
pi_mini <- function(thinking = NULL, ...) {
  pi_client(
    ...,
    thinking = thinking,
    tools = "read,bash,edit,write,codemode",
    args = c(
      "--no-extensions",
      "--extension", "builtin:codemode", # 显式启用 codemode
      "--no-skills",
      "--no-context-files"
    )
  )
}

#' @export
print.pi_client <- function(x, ...) {
  label <- function(name, value) {
    if (!is.null(value) && length(value) && !identical(value, "")) {
      cat("  ", name, ": ", paste(value, collapse = " "), "\n", sep = "")
    }
  }
  cat("<pi_client>\n")
  label("dir", x$dir)
  label("model", x$model)
  label("provider", x$provider)
  label("thinking", x$thinking)
  label("tools", x$tools)
  label("args", x$args)
  label(
    "session",
    if (x$session) paste("persistent", x$session_dir %||% "") else "off"
  )
  invisible(x)
}
