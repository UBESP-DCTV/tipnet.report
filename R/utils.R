tipnet_token <- function() {
  val <- Sys.getenv("REDCAP_TIPNET_PAT")
  if (identical(val, "")) {
    stop("`REDCAP_TIPNET_PAT` env var has not been set")
  }
  val
}


`%||%` <- function(x, y) if (is.null(x)) y else x


skip_if_no_auth <- function() {
  if (identical(Sys.getenv("REDCAP_TIPNET_PAT"), "")) {
    testthat::skip("No authentication available")
  }
}

#' Path to data folder
#' @param path path from which to start looking for the data
#' @export
data_path <- function(path = here::here()) {
  current_folder <- basename(normalizePath(path))
  path_to_data <- switch(current_folder,
    "TIPNet_test"   = fs::path(path, "..", "..", "tipnet-data"),
    "TIPNet"        = fs::path(path, "..", "..", "tipnet-data"),
    "tipnet.report"  = fs::path(path, "..", "tipnet-data"),
    "tipnet.report" = fs::path(path, "..", "tipnet-data"),
    "report"        = data_path(fs::path(path, "..", "..", "tipnet-data")),
    "static"        = data_path(fs::path(path, "..", "..", "tipnet-data")),
    current_folder
  )

  if (path_to_data == current_folder) stop(current_folder)

  fs::dir_create(path_to_data)
}
