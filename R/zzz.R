.onLoad <- function(libname, pkgname) {
  op <- options()

  op.tipnet <- list(
    tipnet.dev = FALSE,
    tipnet.auto_update = TRUE,
    tipnet.force_update = FALSE,
    tipnet.max_days_update = 28
  )

  toset <- !(names(op.tipnet) %in% names(op))
  if (any(toset)) options(op.tipnet[toset])
  invisible(TRUE)
}
