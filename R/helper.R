
#' List the models available to pi
#'
#' Runs `pi --list-models` and parses the table it prints.
#'
#' @param client A [pi_client()].
#' @param model Optional exact model ID, optionally qualified by provider.
#' @return A [data.table][data.table] with one row per matching model.
#' @export
#' @importFrom data.table as.data.table
#' @examples
#' \dontrun{
#' models_pi()
#' }
models_pi <- function(client = pi_client(), model = NULL) {
  search <- if (is.null(model)) NULL else sub(":.*$", "", model)
  res <- processx::run(
    client$bin, c("--list-models", search),
    wd = client$dir, env = client$env, error_on_status = FALSE
  )
  if (res$status != 0L) stop("`pi --list-models` failed.\n", res$stderr, call. = FALSE)
  models <- utils::read.table(
    text = res$stdout, header = TRUE, check.names = FALSE,
    stringsAsFactors = FALSE
  ) %>% as.data.table()
  if (is.null(model)) return(models)

  parts <- strsplit(search, "/", fixed = TRUE)[[1]]
  name <- parts[[length(parts)]]
  provider <- if (length(parts) > 1L) parts[[1]] else client$provider
  models <- models[models$model == name]
  if (!is.null(provider)) models <- models[models$provider == provider]
  models
}

ContextLenMax <- function(client, model) {
  if (is.null(model)) return(NULL)
  tryCatch({
    models <- models_pi(client, model)
    if (nrow(models) == 1L) models$context[[1]]
  }, error = function(e) NULL)
}


arg <- function(flag, value) {
  if (is.null(value) || length(value) == 0) {
    return(NULL)
  }
  c(flag, as.character(value))
}

# --append-system-prompt a --append-system-prompt b
arg_multi <- function(flag, values) {
  values <- values[!is.na(values) & nzchar(values)]
  if (length(values) == 0) {
    return(NULL)
  }
  as.vector(rbind(flag, values))
}
