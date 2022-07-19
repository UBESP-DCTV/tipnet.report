#' Barplot stratified by center
#'
#' @return a ggplot
#' @export
centervar_plot <- function(.db, what, reported_name) {
  .db |>
    dplyr::mutate(
      dplyr::across(.data[[what]], forcats::fct_explicit_na)
    ) |>
    ggplot(aes(x = .data$center, fill = .data[[what]])) +
    geom_bar(position = "dodge") +
    coord_flip() +
    labs(
      x = "Center",
      y = "Counts",
      fill = stringr::str_to_sentence(reported_name)
    )
}

#' Table stratified by center
#'
#' @return a data frame with levels' counts for the selected variable,
#'   each center.
#' @export
centervar_tbl <- function(.db, what) {
  .db |>
    dplyr::mutate(
      dplyr::across(.data[[what]], forcats::fct_explicit_na)
    ) |>
    dplyr::group_by(.data$center, .data[[what]]) |>
    dplyr::summarise(Somma = dplyr::n())
}
