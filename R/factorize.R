#' Factorize selection of variables
#'
#' Given a data frame and a list of named factors' levels, [factorize]
#' mutate in factor the variables of the data frame corresponding to
#' the names of the list provided, considering the corresponding
#' levels/
#'
#' @param x (data frame)
#' @param lev (named list) of levels. Names must be all included in
#'   variables' names of `x`
#'
#' @return a new version of x, of the same class, with the required
#'   variables mutated to factors
#' @export
#'
#' @examples
#' library(tibble)
#'
#' df <- tibble(
#'   foo = c("a", "a", "b"),
#'   bar = 1:3,
#'   baz = c("one", "two", "three"),
#'   qux = c("random", "sample", "text")
#' )
#'
#' fct_levels <- list(
#'   foo = c("a", "b", "c", "d"),
#'   baz = c("one", "two", "three")
#' )
#'
#' factorize(df, fct_levels)
factorize <- function(x, lev) {
  for (var in names(lev)) {
    x[[var]] <- factor(x[[var]], lev[[var]])
  }
  x
}
