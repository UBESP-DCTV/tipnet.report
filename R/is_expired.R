#' Check if a file is expired
#'
#' @param filepath (chr) file path
#' @param max_days (int) number of days until expiration
#'
#' @return (lgl) `TRUE` if the file at `filepath` is expired respect
#'   `max_days`. `FALSE` otherwise
#' @export
#'
is_expired <- function(filepath, max_days) {
  stopifnot(fs::file_exists(filepath))

  day_modified <-
    fs::file_info(filepath, follow = TRUE)[["modification_time"]] %>%
    as.Date()

  (lubridate::today() - day_modified) > lubridate::days(max_days)
}
