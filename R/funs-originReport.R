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
    type = c("centervar", "checkbox"),
    what = c("ricovero_progr", "redcap_repeat_instance","provenienza",
             "altro_osp","tipologia2","tipo_chir","priorita",
             "motivo_post_oper","motivo_ricovero2","motivo_ric_trauma2"),
    dict = NULL


) {
  type <- match.arg(type)
  what <- match.arg(what)

  if (type == "centervar") {

    reported_name <- what |>
      switch(
        "ricovero_progr" = "Ricovero programmato",
        "redcap_repeat_instance" = "Riammissione",
        "provenienza" = "Provenienza",
        "altro_osp" = "Altro ospedale",
        "tipologia2" = "tipo",
        "tipo_chir" = "Tipo chirurgia",
        "priorita" = "Priorità",
        "motivo_post_oper" = "post chirurgia",
        "motivo_ricovero2"  = "ric. medico",
        "motivo_ric_trauma2" = "post-trauma"

      )

    centervar_plot(.db, what, reported_name)





  } else if (type == "checkbox") {
    checkbox_plot(.db, dict)
  }
}

origin_dataTbl <- function(
    .db,
    type = c("centervar", "checkbox"),
    what = c("ricovero_progr", "redcap_repeat_instance","provenienza",
             "altro_osp","tipologia2","tipo_chir","priorita",
             "motivo_post_oper","motivo_ricovero2","motivo_ric_trauma2"),
    dict = NULL,
    by_gender = FALSE,
    by_ageclass = FALSE
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

  if (type == "centervar") {
    centervar_tbl(.db, what)
  } else if (type == "checkbox") {
    checkbox_tbl(.db, dict)
  }

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
