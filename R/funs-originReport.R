#' Origin module
#'
#' @param .db (data frame) data
#' @name funs-OriginReport
NULL

#' @describeIn funs-OriginReport plot
#' @export
#' @examples
#'
#'#' @describeIn funs-OriginReport data
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







#' descriptives_originPlot(full_records)
descriptives_originPlot <- function(.db) {

  .db %>%
    ggplot(aes(x = .data$center, fill = .data$provenienza)) +
    geom_bar(position = "dodge") +
    coord_flip()
}


