#' picu focus module
#'
#' @param .db (data frame) data
#' @param what (chr, default "gender") variable to describe. One of
#'   "gender", or "etnicity"
#'
#' @name funs-focusReport
NULL


#'
#' @describeIn funs-focusReport plot
#' @export
#' @examples
#'
#' focus_Plot(full_records) # default what = "mal_cronica"
#' focus_Plot(full_records, "mal_cronica") # same as before
#' focus_Plot(full_records, "etnia")
focus_Plot <- function(
    .db,
    type = c("centervar", "checkbox"),
    what = c("insufficienza","procedure_rico","vent_iniz2","niv_it","inf_ingresso_tip",
             "sede_inf2","diagnosi_inf","tipo_inf","sepsi_2"),
    dict = NULL
) {
  type <- match.arg(type)
  what <- match.arg(what)
  if (type == "centervar") {


  reported_name <- switch(what,
                          "insufficienza" = "Insufficienza",
                          "procedure_rico" = "Procedura ricovero",
                          "vent_iniz2" = "Inizio ventilazione",
                          "niv_it" ="Tecnica utilizzata",
                          "inf_ingresso_tip" = "infezione ingresso",
                          "sede_inf2" = "Sede infenzione",
                          "diagnosi_inf" = "Diagnosi infezione",
                          "tipo_inf" = "Tipo infezione",
                          "sepsi_2" = "Infezione evoluta in sepsi"

  )

  centervar_plot(.db, what, reported_name)
  } else if (type == "checkbox") {
    checkbox_plot(.db, dict)
  }
}



#' @describeIn funs-focusReport data
#' @export
#' @examples
#'
#' focus_dataToUse(full_records, "Completed only")
#' focus_dataToUse(full_records, "overall")
focus_dataToUse <- function(
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


focus_dataTbl <- function(
    .db,
    type = c("centervar", "checkbox"),
    what = c("insufficienza","procedure_rico","vent_iniz2","niv_it","inf_ingresso_tip",
             "sede_inf2","diagnosi_inf","tipo_inf","sepsi_2"),
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
