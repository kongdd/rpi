#' A fake `pi` executable that replays a fixed stream
#'
#' The wrapper drops the `pi` arguments, so a plain script is enough to drive
#' the ellmer class offline.
#' @noRd
fake_pi_client <- function(...) {
  script <- normalizePath(testthat::test_path("fake-pi.R"), winslash = "/")
  rscript <- file.path(R.home("bin"), "Rscript")
  dir <- tempfile("fakepi")
  dir.create(dir)

  if (.Platform$OS.type == "windows") {
    bin <- file.path(dir, "fake-pi.cmd")
    writeLines(
      c("@echo off", sprintf('"%s.exe" "%s"', rscript, script)),
      bin
    )
  } else {
    bin <- file.path(dir, "fake-pi")
    writeLines(
      c("#!/bin/sh", sprintf('exec "%s" "%s"', rscript, script)),
      bin
    )
    Sys.chmod(bin, "0755")
  }
  pi_client(bin = bin, dir = dir, session = FALSE, ...)
}

#' Run the event loop until a promise settles
#' @noRd
pi_sync <- function(promise) {
  done <- FALSE
  out <- NULL
  promises::then(
    promise,
    function(value) { out <<- value; done <<- TRUE },
    function(err) { out <<- err; done <<- TRUE }
  )
  while (!done) later::run_now(0.05)
  out
}
