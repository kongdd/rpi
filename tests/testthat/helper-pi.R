#' Skip when the `pi` executable is not installed
#'
#' Live tests call a real model and are slow, so they run only when `pi` is
#' available and the user opted in.
#' @noRd
skip_if_no_pi <- function() {
  skip_on_cran()
  if (!nzchar(unname(Sys.which("pi")))) {
    skip("the `pi` executable is not on the PATH")
  }
  if (!isTRUE(getOption("piagent.test", FALSE))) {
    skip("set `options(piagent.test = TRUE)` to run live pi tests")
  }
}
