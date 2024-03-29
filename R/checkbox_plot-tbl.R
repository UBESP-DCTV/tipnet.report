#' Checkbos plot
#'
#' A plot checkboxed characteristics reported as proportionally filled
#' bars.
#'
#' @return a ggplot
#' @export
checkbox_plot <- function(.db, dict) {
  checkbox_db(.db, dict) |>
    ggplot(aes(x =   forcats::fct_rev(center),  fill = name)) +
    geom_bar(position =position_fill(reverse = TRUE)) +
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


reorder_name_level <- function(.db) {

  if (!"names" %in% (.db)) {
    return(.db)
  }

  .db[["names"]] |>
    forcats::fct_relevel(
      get_ordered_name(.db)
    )

}



get_ordered_name <- function(.db) {
  stopifnot("names" %in% (.db[["names"]]))
  stopifnot(
    `name must be a factor` = is.factor(
      .db[["names"]]
    )
  )

  if (all(
    c('02-basso-flusso', '02-alto-flusso', 'ventilazione','broncoscopia', 'tracheotomia',
      'accesso vascolare','PIC','drenaggio addominale','drenaggio toracico',
      'drenaggio pericardico','cateterismo cardiaco', 'trattamento dialitico',
      'PEG','intervento chirurgico', 'RCP', 'ECMO', 'VAD', 'Rashkind','altro') %in% .db[["names"]]
  )) {
    c('02-basso-flusso', '02-alto-flusso', 'ventilazione','broncoscopia', 'tracheotomia',
      'accesso vascolare','PIC','drenaggio addominale','drenaggio toracico',
      'drenaggio pericardico','cateterismo cardiaco', 'trattamento dialitico',
      'PEG','intervento chirurgico', 'RCP', 'ECMO', 'VAD', 'Rashkind','altro')

    stop("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")


  } else {
    levels(.db[["names"]])
  }
}


