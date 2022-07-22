#' Origin module
#'
#' @param .db (data frame) data
#' @param what (chr, default "gender") variable to describe. One of
#'   "gender", or "etnicity"
#'
#' @name funs-originReport
NULL


#'
#' @describeIn funs-originReport plot
#' @export
#' @examples
#'
#' origin_Plot(full_records) # default what = "gender"
#' origin_Plot(full_records, "gender") # same as before
#' origin_Plot(full_records, "etnia")
origin_Plot <- function(
    .db,
    what = c("ricovero_progr", "redcap_repeat_instance"),
    by_gender = FALSE,
    by_ageclass = FALSE
) {
  what <- match.arg(what)

  reported_name <- what |>
    switch(
      "ricovero_progr" = "Ricovero programmato",
      "redcap_repeat_instance" = "Riammissione"
    )

  p <- .db |>
    centervar_plot(what, reported_name)

  if (by_gender) {
    p <- p +
      aes(colour = gender)
  }

  if (by_ageclass) {
    p <- p +
     facet_wrap(~ age_class, nrow = 3)
  }

  p
}


origin_dataTbl <- function(
    .db,
    what = c("ricovero_progr", "redcap_repeat_instance"),
    by_gender = FALSE,
    by_ageclass = FALSE
) {
  what <- match.arg(what)

  if (by_gender) {
    .db <- .db |>
      group_by(.data[["gender"]])
  }

  if (by_ageclass) {
    .db <- .db |>
      group_by(.data[["age_class"]], .add = TRUE)
  }

  .db |>
    centervar_tbl(what)
}


#' @describeIn funs-originReport data
#' @export
#' @examples
#'
#' origin_dataToUse(full_records, "Completed only")
#' origin_dataToUse(full_records, "overall")
origin_dataToUse <- function(
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
