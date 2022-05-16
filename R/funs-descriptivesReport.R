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
    what = c("gender", "etnia")
) {
  what <- match.arg(what)

  reported_name <- switch (what,
    "gender" = "gender",
    "etnia" = "ethnicity"
  )

  .db %>%
    ggplot(aes(x = .data$center, fill = .data[[what]])) +
    geom_bar(position = "dodge") +
    coord_flip() +
    labs(
      x = "Center",
      y = "Counts",
      fill = stringr::str_to_sentence(reported_name)
    )
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
