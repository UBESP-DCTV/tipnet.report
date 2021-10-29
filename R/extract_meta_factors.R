extract_meta_factors <- function(meta, sheet) {
  fct_levels <- meta[["fct_level"]]

  meta_to_consider <- (meta[["sheet"]] == sheet) &
    !(meta[["field_type"]] %in% c("text", "calc")) &
    purrr::map_lgl(fct_levels, ~length(.x) > 0)

  fct_levels[meta_to_consider]
}
