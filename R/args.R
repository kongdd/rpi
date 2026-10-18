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
    pi_arg("--model", model %||% client$model),
    pi_arg("--provider", client$provider),
    pi_arg("--thinking", client$thinking),
    pi_arg("--tools", client$tools),
    pi_arg("--exclude-tools", client$exclude_tools),
    pi_arg("--system-prompt", client$system_prompt),
    pi_arg_rep("--append-system-prompt", append),
    pi_arg("--session-dir", client$session_dir),
    if (client$session && !is.null(session_id)) {
      c("--session-id", session_id)
    } else {
      "--no-session"
    },
    client$args
  )
}

pi_arg <- function(flag, value) {
  if (is.null(value) || length(value) == 0) {
    return(NULL)
  }
  c(flag, as.character(value))
}

pi_arg_rep <- function(flag, values) {
  values <- values[!is.na(values) & nzchar(values)]
  if (length(values) == 0) {
    return(NULL)
  }
  as.vector(rbind(flag, values))
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
