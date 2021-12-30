nosi_columns_to_lgl <- function(db) {
  dplyr::mutate_if(db, is_nosi_var, ~{
      stringr::str_detect(., pattern = "^[sS]")
    })
}

are_nosi_records <- function(x) {
  are_nosi <- stringr::str_detect(x,
    "^[sSnN][iI\\u00ec\\u00cc\\u00ed\\u00cdoO]$"
  )
}

is_nosi_var <- function(x, na.rm = TRUE) {
  is.character(x) && all(are_nosi_records(x), na.rm = na.rm)
}
