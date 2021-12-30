sheets_to_var <- function(data, name = "sheet", exept = NULL) {

  names_pattern <- paste0(
    "(", paste(attr(data, "sheet_names"), collapse = "|"), ")",
    "_(.*)"
  )

  unused <- setdiff(exept, names(data))
  cols_to_exclude <- setdiff(exept, unused)

  tidyr::pivot_longer(
    data = data,
    cols = -tidyselect::all_of(cols_to_exclude),
    names_to = c(name, ".value"),
    names_pattern = names_pattern
  )
}
