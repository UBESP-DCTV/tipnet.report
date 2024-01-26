#' picu admissions module
#'
#' @param .db (data frame) data
#' @param what (chr, default "gender") variable to describe. One of
#'   "gender", or "etnicity"
#'
#' @name funs-admissionReport
NULL


#'
#' @describeIn funs-admissionReport plot
#' @export
#' @examples
#'
#' admission_Plot(full_records) # default what = "mal_cronica"
#' admission_Plot(full_records, "mal_cronica") # same as before
#' admission_Plot(full_records, "etnia")
admission_Plot <- function(
    .db,
    type = c("centervar", "checkbox"),
    what = c("mal_cronica","popc","insuff_organo7", "pim"),
    dict = NULL
) {
  type <- match.arg(type)
  what <- match.arg(what)

  if (type == "centervar") {
    reported_name <- switch(what,
                            "mal_cronica" = "malattia cronica",
                            "popc" = "POPC",
                            "insuff_organo7" = "Insufficienze d'organo",
                            "pim" = "PIM"
    )


    centervar_plot(.db, what, reported_name)

  } else if (type == "checkbox") {
    checkbox_plot(.db, dict)
  }
}



#' @describeIn funs-admissionReport data
#' @export
#' @examples
#'
#' admission_dataToUse(full_records, "Completed only")
#' admission_dataToUse(full_records, "overall")
admission_dataToUse <- function(
    .db,
    .which = c("Completed only", "Overall")
) {

  .which <- match.arg(.which)

  if (.which == "Completed only") {
    return(
      filter(.db, .data$complete)
    )
  }
  .db
}


admission_dataTbl <- function(
    .db,
    type = c("centervar", "checkbox"),
    what = c("mal_cronica","popc","insuff_organo7", "pim"),
    dict = NULL,
    by_gender = FALSE,
    by_ageclass = FALSE,
    by_type = FALSE,
    by_year = FALSE
) {
  type <- match.arg(type)
  what <- match.arg(what)

  if (by_gender) {
    .db <- .db |>
      group_by(.data[["gender"]])
  }

  if (by_ageclass) {
    .db <- .db |>
      group_by(.data[["age_class"]], .add = TRUE)
  }

  if (by_type) {
    .db <- .db |>
      group_by(.data[["tipologia"]], .add = TRUE)
  }

  if (by_year) {
    .db <- .db |>
      dplyr::mutate(
        year = as.integer(lubridate::year(.data[["ingresso_dt"]]))
      ) |>
      group_by(.data[["year"]], .add = TRUE)
  }


  if (type == "centervar") {
    centervar_tbl(.db, what)
  } else if (type == "checkbox") {
    checkbox_tbl(.db, dict)
  }

}
