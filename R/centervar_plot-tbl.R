#' Barplot stratified by center
#'
#' @return a ggplot
#' @export
centervar_plot <- function(.db, what, reported_name) {

  geom_centervar <- if (what == "redcap_repeat_instance") {
    function(p) {
      p +
      geom_boxplot(aes(y = .data[[what]]))
    }
  } else {
    function(p) {
      p +
        geom_bar(
          aes(fill = .data[[what]]),
          position = "dodge"
        )
    }
  }

  .db  |>
    transform_centervar(what = what) |>
    ggplot(aes(x = .data$center)) |>
    geom_centervar() +
    coord_flip() +
    labs(
      x = "Center",
      y = "Counts",
      fill = stringr::str_to_sentence(reported_name)
    ) +
    theme(legend.position = "top")
}

#' Table stratified by center
#'
#' @return a data frame with levels' counts for the selected variable,
#'   each center.
#' @export
centervar_tbl <- function(.db, what) {

  if (what == "redcap_repeat_instance") {
    .db |>
      dplyr::group_by(.data$center, .add = TRUE) |>
      dplyr::summarise(
        N = n(),
        Mediana = median(.data[[what]]),
        IQR = IQR(.data[[what]])
      )
  } else {
    .db |>
      transform_centervar(what = what) |>
      dplyr::group_by(.data$center, .data[[what]], .add = TRUE) |>
      dplyr::summarise(Somma = dplyr::n())
  }

}


transform_centervar <- function(x, what) {
  if (what == "redcap_repeat_instance") {
    x |>
      dplyr::filter(.data[[what]] != 1)
  } else {
    x |>
      dplyr::mutate(
        dplyr::across(.data[[what]], forcats::fct_explicit_na)
      )
  }
}
