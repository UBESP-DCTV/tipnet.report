#' Add sheets name to REDCap exports
#'
#' Data exported from REDCap are extracted column-binding all the
#' sheets variable in order from left to right. When all the
#' variable of a sheet are considered, the column named
#' `<sheet_name>_completed` is added to track the records for which
#' all the sheet is filled.
#'
#' This function add the name of every sheets as a prefix of the name
#' of each of its corresponding variable (if not already present).
#'
#' @param data (data.frame) a "data" data frame inside the object
#'   exported from REDCap using the [read_redcap] function.
#' @param exept (chr) Character vector of variable's names in `data` to
#'   exclude from being considered into a sheet (i.e. REDCap
#'   infromations not included into a specific form, e.g., ID)
#'
#' @return the data [tibble][tibble::tibble-package] with modified
#'   names.
#' @export
#'
#' @examples
#'
#' sample_df <- data.frame(
#'   a = sample(1),
#'   b = sample(1),
#'   c_complete = sample(1),
#'   d = sample(1),
#'   e_foo = sample(1),
#'   e_complete = sample(1)
#' )
#'
#'
#' add_sheets_prefix(sample_df)
#' add_sheets_prefix(sample_df, exept = "a")
add_sheets_prefix <- function(data, exept = NULL) {

  data_names <- names(data)

  is_sheet_marker <- stringr::str_detect(data_names, "_complete$")
  if (!any(is_sheet_marker)) ui_stop("
    There are not any variables terminanting with {ui_value('_complete')},
    which is supposed to be the sheets marker.
  ")

  sheets_names <- data_names[is_sheet_marker] %>%
    stringr::str_remove("_complete$|_da_compilare.*$")

  if (!is.null(exept)) {
    assertive::assert_is_subset(exept, data_names)
  } else {
    exept <- character()
  }


  current_sheet <- 1L
  new_var_names <- vector("character", length(data_names))
  for (i in seq_along(data_names)) {
    sheet_name <- sheets_names[[current_sheet]]
    var_name   <- data_names[[i]]

    if (is_sheet_marker[[i]]) {
      new_var_names[[i]] <- var_name
      current_sheet[]    <- current_sheet + 1L
      next
    }

    if (var_name %in% exept) {
      new_var_names[[i]] <- var_name
      next
    }

    new_var_names[[i]] <- paste0(sheet_name, "_", var_name)
  }

  new_var_names <- new_var_names %>%
    stringr::str_replace("_da_compilare.+(_complete)", "\\1")

  names(data) <- new_var_names
  attr(data, "sheet_names") <- sheets_names
  data
}
