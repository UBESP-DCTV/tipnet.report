#' Barplot stratified by center
#'
#' @return a ggplot
#' @export
centervar_plot <- function(.db, what, reported_name) {

  geom_centervar <- if (any(what == c("redcap_repeat_instance","durata_degenza"))) {
    function(p) {
      p +
      geom_boxplot(aes(y = .data[[what]]))
    }
  } else if (any(what == "pim")) {
    function(p) {
      p +
        geom_boxplot(
          aes(
            y = .data[["pim_val"]],
            colour = .data[["pim_type"]]
          )
        ) +
        coord_flip()
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

  .db |>
    transform_centervar(what = what) |>
    ggplot(aes(x = .data$center)) |>
    geom_centervar() +
    coord_flip() +
    labs(
      x = "Center",
      y = "Counts",
      fill = stringr::str_to_sentence(reported_name),
      colour = stringr::str_to_sentence(reported_name)
    ) +
    theme(legend.position = "top")
}




#' Table stratified by center
#'
#' @return a data frame with levels' counts for the selected variable,
#'   each center.
#' @export
centervar_tbl <- function(.db, what) {

  if (any(what == c( "redcap_repeat_instance","durata_degenza"))) {
    checkmate::assert_string(what)

    .db |>
      dplyr::group_by(.data$center, .add = TRUE) |>
      dplyr::summarise(
        N = n(),
        Mediana = median(.data[[what]], na.rm = TRUE),
        IQR = IQR(.data[[what]], na.rm = TRUE)
      )
  } else if (any(what == "pim")) {
    checkmate::assert_string(what)

    .db |>
      transform_centervar(what = what) |>
      dplyr::group_by(.data$center, .data$pim_type, .add = TRUE) |>
      dplyr::summarise(
        Mediana = median(.data[["pim_val"]], na.rm = TRUE),
        IQR = IQR(.data[["pim_val"]], na.rm = TRUE)
      )|>
      dplyr::relocate(dplyr::all_of(c("center", "pim_type"))) |>
      dplyr::arrange(.data[["center"]], .data[["pim_type"]])
  } else {
    .db |>
      transform_centervar(what = what) |>
      dplyr::group_by(
        across(c(.data$center, dplyr::all_of(what))),
        .add = TRUE
      ) |>
      dplyr::summarise(Somma = dplyr::n())
  }

}





transform_centervar <- function(x, what) {
  if (any(what == "pim")) {
    checkmate::assert_string(what)

    x |>
      dplyr::select(
        "center", "age_class", "gender", "tipologia", "ingresso_dt",
        dplyr::matches("pim")
      ) |>
      pivot_longer(
        dplyr::all_of(c("pim2", "pim3")),
        names_to = "pim_type",
        values_to = "pim_val"
      ) |>
      ggplot2::remove_missing(na.rm = TRUE, vars = "tip_val")

  } else if (
    any(what == c("redcap_repeat_instance","durata_degenza"))
  ) {
    checkmate::assert_string(what)
    x |>
      dplyr::filter(.data[[what]] != 1)
  } else {
    x |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(what), forcats::fct_explicit_na)
      )
  }
}
