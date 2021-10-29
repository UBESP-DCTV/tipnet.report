convert_to_integer_numerical_integer_columns <- function(db) {
    dplyr::mutate_if(db, is_integer_col, convert_to_integer)
}


convert_to_integer <- function(x) {
    nm <- names(x)
    assertive::coerce_to(x, "numeric") %>%
        # round is needed to correctly parse below approximation, e.g.,
        # without `round()`, `as.integer()` will trunc, e.g.,
        # 1/(1 - 0.98) become 49
        round() %>%
        as.integer() %>%
        purrr::set_names(nm)
}


are_all_whole_number <- function(x, tol = 200 * .Machine$double.eps) {
  all(assertive::is_whole_number(x, tol = tol), na.rm = TRUE)
}

is_integer_col <- function(x) {
    is.numeric(x) && are_all_whole_number(x)
}
