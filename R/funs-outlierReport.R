#' Functions for outlier section
#'
#' @param .db (data frame) main data to use
#' @param .center (chr) name(s) of the center(s) to consider
#'
#' @importFrom ggplot2 geom_point labs scale_shape_manual
#' @name funs-outlierReport
NULL


#' @describeIn funs-outlierReport plot
#' @export
outlier_dataPlot <- function(.db, .center) {
  .db %>%
    dplyr::add_row(
      center = "TIPNet",
      prop_outliers = mean(.db$prop_outliers)
    ) %>%
    dplyr::mutate("shape" = factor(.data$center == "TIPNet")) %>%
    ggplot(aes(
      x = forcats::fct_reorder(
        .data$center, .data$prop_outliers,
        .desc = TRUE
      ),
      y = .data$prop_outliers,
      colour = .data$center,
      shape = .data$shape,
      size = .data$shape
    )) +
    geom_point() +
    scale_shape_manual(values = c(19, 8)) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
    ) +
    ylab("Proportions") +
    xlab("") +
    coord_flip() +
    ggtitle("Distributions of outliers by center")
}


#' @describeIn funs-outlierReport data to use
#' @export
outlier_dataToUse <- function(.db) {
  if (length(.center) == 0 || !.center %in% .db[["center"]]) {
    .db[["data"]][[1L]][FALSE, ]
  } else {
    dplyr::filter(.db, .data$center %in% .center)[["data"]][[1]] %>%
      dplyr::rename(
        `patient's code` = .data$codpat,
        `instance id` = .data$redcap_repeat_instance
      )
  }
}
