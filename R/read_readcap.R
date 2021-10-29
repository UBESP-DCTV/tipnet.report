#' Import the data from REDCap
#'
#' Import the data from REDCap repository into two
#' [tibble][tibble::tibble-package], one for the content and one
#' for the main metadata.
#'
#' @param url (chr) the url for the REDCap database
#'   (e.g.: "https://redcap.dctv.unipd.it/api/").
#' @param token (chr) Personal Access Token to the REDCap project
#'
#' @note For `read_redcap()` to function properly, the user must have
#'   Export permissions for the 'Full Data Set'.
#'
#'
#' @return (list) a list with 2 [tibble][tibble::tibble-package]. The
#'   first ("data") contains the data of the study, whereas the other
#'   contains three columns with the names of the variables in `data`,
#'   the names of the form associated to each field and the labels
#'   associated to each field
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   url   <- "https://bbmc.ouhsc.edu/redcap/api/"
#'   token <- "9A81268476645C4E5F03428B8AC3AA7B"
#'   rc <- read_redcap(token = token, url = url)
#' }
#'
read_redcap <- function(
  url, # = tipnet_redcap_url(),
  token = tipnet_token()
  ) {

  assertive::assert_is_a_string(token)
  assertive::assert_is_a_string(url)

  # list of two: `data` and `meta_data`
  # ~ 9 min/call (1.5-1.8 s/batch)
  data <- REDCapR::redcap_read(
    batch_size = 300L,
    redcap_uri = url,
    token = token,
    raw_or_label = "label",
    raw_or_label_headers = "raw",
    export_checkbox_label = TRUE,
    export_data_access_groups = TRUE,
    col_types = NULL,
    guess_type = TRUE,
    verbose = FALSE
  )

  meta_data <- REDCapR::redcap_metadata_read(
    redcap_uri = url,
    token = token,
    verbose = FALSE
  )

  list(data = data, meta_data = meta_data)

}
