#' Extract data from REDCap import in tidy way
#'
#' After that information are retrieved by [read_redcap], this
#' function extract data or metadata from it, cleaning variables
#' names, lowering every textual column, and renaming the event
#' field like "fields"
#'
#' @param data the output of [read_redcap]
#' @param type (chr) "data" to get the main data of the project, or
#'   "meta" to get the meta-data.
#'
#' @return [tibble][tibble::tibble-package]
#' @export
#'
#' @examples
#' \dontrun{
#'   tipnet_raw <- read_redcap(tipnet_redcap_url())
#'
#'   tidy_extract(tipnet_raw, "data")
#'   tidy_extract(tipnet_raw, "meta")
#' }
#'
tidy_extract <- function(
    data,
    type = c("data", "meta"),
    use_city = TRUE
) {
  type <- match.arg(type)

  if (type == "meta") {
    type <- "meta_data"
  }

  res <- data[[type]][["data"]] %>%
    tibble::as_tibble() %>%
    janitor::clean_names() %>%
    dplyr::mutate_if(is.character, tolower)

  if (type == "data") {
    res <- res %>%
      dplyr::filter(
        stringr::str_detect(.data[["codpat"]], "^0000-", negate = TRUE)
      ) %>%
      dplyr::rename(
        center = .data$redcap_data_access_group,
        fields = .data$redcap_event_name
      ) %>%
      dplyr::mutate(
        center = factorize_centers(.data$center, use_city = use_city)
      ) %>%
      # dplyr::filter(center %in% c("<MISSING>", "", "Prova")) %>%
      dplyr::mutate(center = forcats::fct_drop(.data$center))
  }

  if (type == "meta_data") {
    res <- res %>%
      dplyr::rename(
        sheet = .data$form_name,
        fct_level = .data$select_choices_or_calculations
      ) %>%
      dplyr::mutate(
        fct_level = purrr::map2(.data$fct_level, .data$field_type, ~{
          dplyr::case_when(
            !is.na(.x) & !(.y %in% c("calc", "text")) ~ str_to_level(.x),
            TRUE ~ .x
          )
        }) %>%
          purrr::set_names(.data$field_name),

        sheet = stringr::str_replace(
          .data$sheet,
          "_da_compilare.*$",
          ""
        )
      )
  }

  janitor::remove_empty(res, c("rows"))
}
