factorize_sheet <- function(sheet, table, meta) {

  fctrs_list     <- extract_meta_factors(meta, sheet)
  is_fctr_in_use <- names(fctrs_list) %in% names(table)
  fctrs_list     <- fctrs_list[is_fctr_in_use]

  if (length(fctrs_list) == 0) return(table)

  factorize(table, fctrs_list)
}
