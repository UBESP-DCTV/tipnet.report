#' Get a sheet table
#'
#' @param x (tbl_df) output from [db_update_from_server]
#' @param sheet (chr) name of sheet to extract
#' @param field (chr, default = NULL) if not NULL, the name of the field
#'   containing the sheet required. If NULL, it is supposed that there is only
#'   one field containing the required sheet, and that is used. If more than
#'   one field exist with the required sheet, and field is NULL, an error
#'   is thrown suggesting the possible fields to consider.
#'
#' @return the required sheet [tibble][tibble::tibble-package]
#' @export
#'
#' @examples
#' \dontrun{
#'   tipnet <- read_rds(db_update_from_server())
#'   tipnet %>% get_sheet("anagrafica")
#'   tipnet %>% get_sheet("anagrafica", field = "anagrafica")
#'
#' }
get_sheet <- function(x, sheet, field = NULL) {

  where_sheet <- sheet == x[["sheet"]]
  n_sheets <- sum(where_sheet)

  if (n_sheets == 0) ui_stop("Sheet {ui_value(sheet)} does not exists in data.")


  possible_fields <- x[["fields"]][where_sheet]

  if (!is.null(field) && (!field %in% possible_fields)) {
    ui_stop("
      Sheet {ui_value(sheet)} is not present in the field {ui_value(field)}.
      Try to select one field of {ui_value(possible_fields)}.
    ")
  }

  if (n_sheets > 1 && is.null(field)) {
    ui_stop("
      Sheet {ui_value(sheet)} is present in the {ui_field(possible_fields)}.
      Provide the {ui_field('field')} argument to {ui_code('get_table()')}.
    ")
  }

  sheet_row <- if (is.null(field)) {
    where_sheet
  } else {
    where_sheet & (field == x[["fields"]])
  }

  x[["tables"]][[which(sheet_row)]]
}
