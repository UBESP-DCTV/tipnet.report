#' Extract form coded string to corresponding levels
#'
#' @param x (chr) a string (i.e. character vector of length one)
#'   representing factors' levels in the form of
#'   `level1, label1 | level2, label2 | ... | levelN, labelN`
#'
#' @return (chr) a character string of length `N` reporting the label
#'   only.
#' @export
#'
#' @examples
#' coded <- "1, maschio | 2, femmina | 3, ambiguo"
#' levels <- str_to_level(coded)
str_to_level <- function(x) {
  assertive::assert_is_a_non_empty_string(x)

  if (is.na(x)) return(character())

  stringr::str_split(x, " \\| ")[[1]] %>%
    stringr::str_remove(pattern = "^.*?, ") %>%
    unique()
}
