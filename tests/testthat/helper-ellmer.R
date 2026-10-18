#' A fake `pi` executable that replays a fixed stream
#'
#' The wrapper forwards the pi arguments to a portable R fixture.
#' @noRd
fake_pi_client <- function(..., session = FALSE) {
  script <- normalizePath(testthat::test_path("fake-pi.R"), winslash = "/")
  rscript <- file.path(R.home("bin"), "Rscript")
  dir <- tempfile("fakepi")
  dir.create(dir)

  if (.Platform$OS.type == "windows") {
    bin <- file.path(dir, "fake-pi.cmd")
    writeLines(
      c("@echo off", sprintf('"%s.exe" "%s" %%*', rscript, script)),
      bin
    )
  } else {
    bin <- file.path(dir, "fake-pi")
    writeLines(
      c("#!/bin/sh", sprintf('exec "%s" "%s" "$@"', rscript, script)),
      bin
    )
    Sys.chmod(bin, "0755")
  }
  pi_client(bin = bin, dir = dir, session = session, ...)
}

#' Run the event loop until a promise settles
#' @noRd
pi_sync <- function(promise) {
  done <- FALSE
  out <- NULL
  promises::then(
    promise,
    function(value) {
      out <<- value
      done <<- TRUE
    },
    function(err) {
      out <<- err
      done <<- TRUE
    }
  )
  while (!done) later::run_now(0.05)
  out
}
