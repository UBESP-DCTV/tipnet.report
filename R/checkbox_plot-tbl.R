#' Checkbos plot
#'
#' A plot checkboxed characteristics reported as proportionally filled
#' bars.
#'
#' @return a ggplot
#' @export
checkbox_plot <- function(.db, dict) {
  checkbox_db(.db, dict) |>
    ggplot(aes(x = center, fill = name)) +
    geom_bar(position = "fill") +
    coord_flip() +
    labs(x = "Centro", y = "Prop", fill = "Categoria")
}


#' Checkbox table
#'
#' A long table with sum and proportions stratifiable by gender and age
#' class
#'
#' @return a dataframe with sum and prop for each checkbox
#' @export
checkbox_tbl <- function(.db, dict) {
  checkbox_db(.db, dict) |>
    dplyr::group_by(center, value, .add = TRUE) |>
    dplyr::tally() |>
    dplyr::group_by(center, value) |>
    dplyr::mutate(prop = round(n / sum(n, na.rm = TRUE), 3)) |>
    dplyr::ungroup() |>
    dplyr::relocate(dplyr::all_of(c("center", "value"))) |>
    dplyr::arrange(.data[["center"]], .data[["value"]])
}


checkbox_db <- function(.db, dict) {
  .db |>
    dplyr::select(
      .data$center,
      dplyr::all_of(names(dict)),
      dplyr::any_of(c("age_class", "gender"))
    ) |>
    dplyr::rename_with(~dict[.x], .cols = dplyr::all_of(names(dict))) |>
    tidyr::pivot_longer(
      -dplyr::all_of(c("center", "age_class", "gender")),
      values_drop_na = TRUE
    )
}
