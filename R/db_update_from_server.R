#' Pull REDCap data from the server
#'
#' @param path_data (chr) path for data folder (default is `data/`
#'   under the current project or the [here][here::here] path, if it not
#'   exists it will be created)
#'
#' @return (lgl) `TRUE` if success, `FALSE` otherwise
#' @export
#'
#' @examples
#' \dontrun{
#'   db_update_from_server()
#' }
db_update_from_server <- function(path_data = NULL, use_city = TRUE) {

  path_data <- path_data %||% data_path()

  data_file <- paste0(lubridate::today(), "-", "tipnet.rds")
  data_path <- file.path(path_data, data_file)

  meta_file <- paste0(lubridate::today(), "-", "tipnet_meta.rds")
  meta_path <- file.path(path_data, meta_file)

  tipnet_raw <- read_redcap(tipnet_redcap_url())

  all_ok <- all(c(
    tipnet_raw$data$success,
    tipnet_raw$meta_data$success
  ))

  if (!all_ok) {
    ui_warn("Connection error to REDCap (in server-start chunk).")
    return(invisible(FALSE))
  }

  tipnet_meta <- tidy_extract(tipnet_raw, "meta", use_city = use_city)

  tipnet <- tipnet_raw %>%
    tidy_extract("data", use_city = use_city) %>%
    nest_tables() %>%
    dplyr::mutate(
      tables = purrr::map2(
        .x = .data$sheet,
        .y = .data$tables,
        ~ factorize_sheet(.x, .y, meta = tipnet_meta)
      )
    )

  readr::write_rds(tipnet, data_path, compress = "xz")
  readr::write_rds(tipnet,
    file.path(path_data, "tipnet.rds"),
    compress = "xz"
  )

  readr::write_rds(tipnet_meta, meta_path)
  readr::write_rds(tipnet_meta, file.path(path_data, "tipnet_meta.rds"))

  invisible(TRUE)

}
