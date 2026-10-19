# Building the `pi` command line ---------------------------------------------

#' Build the `pi` command line for one call
#'
#' `append` is a character vector of prompts, one `--append-system-prompt` flag
#' each.
#' @noRd
pi_cli_args <- function(client, session_id = NULL, model = NULL,
                        append = character()) {
  c(
    "--mode", "json",
    arg("--model", model %||% client$model),
    arg("--provider", client$provider),
    arg("--thinking", client$thinking),
    arg("--tools", client$tools),
    arg("--exclude-tools", client$exclude_tools),
    arg("--system-prompt", client$system_prompt),
    arg_multi("--append-system-prompt", append),
    arg("--session-dir", client$session_dir),
    if (client$session && !is.null(session_id)) {
      c("--session-id", session_id)
    } else {
      "--no-session"
    },
    client$args
  )
}

#' start with `@` therefore leave the prompt text, and the remaining text is
#' joined back with single spaces.
#' @noRd
pi_prompt_args <- function(prompt) {
  tokens <- strsplit(prompt, "[[:space:]]+")[[1]]
  if (!any(startsWith(tokens, "@"))) {
    return(prompt)
  }
  out <- character()
  txt <- ""
  for (tok in tokens) {
    if (startsWith(tok, "@")) {
      if (nzchar(txt)) {
        out <- c(out, txt)
        txt <- ""
      }
      out <- c(out, tok)
    } else {
      txt <- if (nzchar(txt)) paste(txt, tok, sep = " ") else tok
    }
  }
  c(out, if (nzchar(txt)) txt)
}
