#' Quality module
#'
#' @param .db (data frame) data
#' @param completed (chr) "Completed" or "Not-completed"
#' @param type (chr) "Total" or "Proportion"
#' @param summary_fun (chr) "sum" or "mean"
#' @param are_out_age (lgl) logical vector of out of range values
#' @name funs-qualityReport
NULL


#' @describeIn funs-qualityReport data to use
#' @export
quality_dataToUse <- function(.db, completed) {
  if (completed == "Completed") {
    .db
  } else {
    dplyr::mutate(.db, complete = !.data$complete)
  }
}


#' @describeIn funs-qualityReport summary function
#' @export
quality_summaryFun <- function(type) {
  switch(type, Total = "sum", Proportion = "mean")
}


#' @describeIn funs-qualityReport check for out of range
#' @export
quality_areOutAge <- function(.db) {
  .db[["age_class"]] %in% c("[wrong/missing age]", "eta > 18")
}


#' @describeIn funs-qualityReport message for global statistics
#' @export
quality_statGlobal <- function(.db, completed) {
  are_out_age <- quality_areOutAge(.db)

  glue::glue("Out of {sum(!are_out_age)}, there are ",
    "{sum(.db[['complete']][!are_out_age], na.rm = TRUE)} records ",
    "with {completed} data (proportion of {completed} data = ",
    "{round(mean(.db[['complete']][!are_out_age], na.rm = TRUE), 2)})."
  )
}


#' @describeIn funs-qualityReport message for out of age
#' @export
quality_outOfAge <- function(are_out_age) {
  glue::glue("There are {sum(are_out_age)} people to exclude because ",
  "of their age is missing or more than 18 (strictly)")
}


#' @describeIn funs-qualityReport plot
#' @export
quality_completeDataPlot <- function(.db, summary_fun, completed) {

  .db %>%
    ggplot(aes(x = .data$center, y = as.integer(.data$complete))) +
    stat_summary(
      fun = summary_fun,
      na.rm = TRUE,
      geom = "bar",
      position = "dodge",
      fill = "darkgreen"
    ) +
    stat_summary(aes(x = "TIPNet"),
                 fun = summary_fun,
                 na.rm = TRUE,
                 geom = "bar",
                 position = "dodge",
                 fill = "darkred",
                 alpha = 30
    ) +
    facet_wrap(~.data$age_class) +
    theme(
      axis.text.x = element_text(angle = 66, hjust = 1, vjust = 1),
      legend.position = "none"
    ) +
    xlab("") +
    ylab("") +
    ggtitle(glue::glue("{completed} cases.")) +
    coord_flip()
}
