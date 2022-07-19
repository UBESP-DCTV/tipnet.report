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
    what = c("ricovero_progr")
) {
  what <- match.arg(what)

  reported_name <- what |>
    switch(
      "ricovero_progr" = "Ricovero programmato"
    )

  .db |>
    centervar_plot(what, reported_name)
}


origin_dataTbl <- function(
    .db,
    what = c("ricovero_progr")
) {
  what <- match.arg(what)
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
