#' functions for missing-data section
#'
#' @param .db (data frame) main data
#' @param .center (chr) center(s) to consider
#' @name funs-missReport
NULL

#' @describeIn funs-missReport data to use
#' @export
miss_dataToUse <- function(
    .db,
    sheets = c(
      "anagrafica", "accettazione", "degenza", "dimissione",
      "infezione", "ingresso", "punteggio_di_aristotle",
      "pelod_scheda_facoltativa", "pim", "procedure_di_ventilazione"
    )
) {

  col_to_retain <- purrr::map(sheets, ~names(get(.x))) |>
    purrr::reduce(union) |>
    c("complete")

  .db %>%
    dplyr::select(all_of(col_to_retain)) %>%
    dplyr::group_by(.data$center) %>%
    dplyr::summarise_all(~mean(is.na(.))) %>%
    tidyr::pivot_longer(
      -.data$center,
      names_to = "field",
      values_to = "prop"
    )
}


#' @describeIn funs-missReport plot
#' @export
miss_dataPlot <- function(.db) {
  .db %>%
    ggplot(aes(x = .data$center, y = .data$prop, colour = .data$center)) +
    geom_boxplot(outlier.shape = NA) +
    geom_boxplot(outlier.shape = NA,
                 data = tibble(TIPmiss = .db$prop),
                 aes(x = "TIPNet", y = .data$TIPmiss), colour = "black"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
    ) +
    ylab("Proportions") +
    xlab("") +
    coord_flip(ylim = c(min(.db$prop) / 1.5, min(1, max(.db$prop) * 1.5))) +
    ggtitle("Distributions of missigness by center")
}


#' @describeIn funs-missReport table
#' @export
miss_dataTbl <- function(.db, .center) {
  db <- if (length(.center) == 0 || !any(.center %in% .db[["center"]])) {
    .db
  } else {
    dplyr::filter(.db, .data$center %in% .center)
  }

  db %>%
    dplyr::transmute(
      center = .data$center,
      field = as.factor(.data$field),
      `missing (%)` = 100 * round(.data$prop, 4)
    ) %>%
    dplyr::filter(.data[["missing (%)"]] != 0)
}
