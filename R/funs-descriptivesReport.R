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
#' descriptives_genderPlot(full_records)
descriptives_genderPlot <- function(
    .db,
    what = c("gender", "etnia")
) {
  what <- match.arg(what)

  .db %>%
    ggplot(aes(x = .data$center, fill = .data[[what]])) +
    geom_bar(position = "dodge") +
    coord_flip()
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
