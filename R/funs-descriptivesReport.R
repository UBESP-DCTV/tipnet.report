#' Descriptives module
#'
#' @param .db (data frame) data
#' @name funs-descriptivesReport
NULL

#' @describeIn funs-descriptivesReport plot
#' @export
#' @examples
#'
#'#' @describeIn funs-descriptivesReport data
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

#' descriptives_genderPlot(full_records)
descriptives_genderPlot <- function(.db) {

  .db %>%
    ggplot(aes(x = .data$center, fill = .data$gender)) +
    geom_bar(position = "dodge") +
    coord_flip()
}




#' descriptives_etniaPlot(full_records)
descriptives_etniaPlot <- function(.db) {

  .db %>%
    ggplot(aes(x = .data$center, fill = .data$etnia)) +
    geom_bar(position = "dodge") +
    coord_flip()
}

#' descriptives_etniaPlot(full_records)
descriptives_ageclassPlot <- function(.db) {

  .db %>%
    ggplot(aes(x = .data$center, fill = .data$age_class)) +
    geom_bar(position = "dodge") +
    coord_flip()
}

