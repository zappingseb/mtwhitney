expect <- function(ok, failure_message, info = NULL, srcref = NULL) {
  type <- if (ok) "success" else "failure"

  # Preserve existing API which appear to be used in package test code
  # Can remove in next major release
  if (missing(failure_message)) {
    warn("`failure_message` is missing, with no default.")
    message <- "unknown failure"
  } else {
    # A few packages include code in info that errors on evaluation
    message <- paste(c(failure_message, info), collapse = "\n")
  }

  exp <- expectation(type, message, srcref = srcref)

  withRestarts(
      if (ok) signalCondition(exp) else stop(exp),
      continue_test = function(e) NULL
  )

  invisible(exp)
}
assignInNamespace("expect", expect, ns="testthat", pos="package:testthat")

Sys.setenv("PATH" = paste0("C:/Program Files/ImageMagick-6.9.10-47-portable-Q16-x64",";",Sys.getenv("PATH")))
