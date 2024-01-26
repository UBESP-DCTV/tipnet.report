#' Descriptives module
#'
#' @param .db (data frame) data
#' @param what (chr, default "gender") variable to describe. One of
#'   "gender", or "etnicity"
#'
#' @name funs-descriptivesReport
NULL


#'
#' @describeIn funs-descriptivesReport plot
#' @export
#' @examples
#'
#' descriptives_Plot(full_records) # default what = "gender"
#' descriptives_Plot(full_records, "gender") # same as before
#' descriptives_Plot(full_records, "etnia")
descriptives_Plot <- function(
    .db,
    what = c("gender", "etnia", "age_class")
) {
  what <- match.arg(what)

  reported_name <- switch(what,
    "gender" = "Genere",
    "etnia" = "Etnia",
    "age_class" = "Età in classi"
  )

  centervar_plot(.db, what, reported_name)
}



#' @describeIn funs-descriptivesReport data
#' @export
#' @examples
#'
#' descriptives_dataToUse(full_records, "Completed only")
#' descriptives_dataToUse(full_records, "overall")
descriptives_dataToUse <- function(
  .db,
  .which = c("Completed only", "Overall")
) {

  .which <- match.arg(.which)

  if (.which == "Completed only") {
    return(
      filter(.db, .data$complete)
    )
  }
  .db
}


descriptives_dataTbl <- function(.db, strat) {
  checkmate::assert_subset(
    strat,
    c("gender", "etnia", "age"),
    empty.ok = FALSE
  )

  strat <- stringr::str_replace(strat, "age", "age_class")

  centervar_tbl(.db, strat)
}
