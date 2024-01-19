#' los module
#'
#' @param .db (data frame) data
#' @param what (chr, default "gender") variable to describe. One of
#'   "gender", or "etnicity"
#'
#' @name funs-losReport
NULL


#'
#' @describeIn funs-losReport plot
#' @export
#' @examples
#'
#' los_Plot(full_records) # default what = "mal_cronica"
#' los_Plot(full_records, "mal_cronica") # same as before
#' los_Plot(full_records, "etnia")
los_Plot <- function(
    .db,
    what = c("diagnosi","diagnosi_2","popc_dimissione", "popc_delta","esito_tip","mod_decesso","deceduto",
             "prelievo_organi","destinazione","durata_degenza","smr")
) {
  what <- match.arg(what)

  reported_name <- switch(what,
                          "diagnosi" = "Diagnosi princ.",
                          "diagnosi_2" = "Diagnosi 2",
                          "popc_dimissione" = "POPC",
                          "popc_delta" = "Delta POPC",
                          "esito_tip" ="esito",
                          "mod_decesso" = "Decesso",
                          "deceduto" = "Acc. decesso",
                          "prelievo_organi" = "Organi",
                          "destinazione" = "destinazione",
                          "durata_degenza" = "Durata",
                          "smr" ="SMR"


  )

  centervar_plot(.db, what, reported_name)
}



#' @describeIn funs-lospimReport plot
#' @export
pimlos_plot <- function(.db) {
  .db |>
    dplyr::select("center", "durata_degenza", dplyr::matches("pim")) |>
    tidyr::pivot_longer(
      dplyr::all_of(c("pim3")),
      names_to = "pim_type",
      values_to = "pim_val"
    ) |>
    ggplot2::remove_missing(
      na.rm = TRUE,
      vars = c("pim_val", "durata_degenza")
    ) |>
    group_by(.data$center, .data$pim_type) |>
    summarise(
      pimmed = mean(.data$pim_val),
      durata_media = mean(.data$durata_degenza)
    ) |>
    dplyr::ungroup() |>
    ggplot(
      aes(.data$durata_media, .data$pimmed, colour = .data$pim_type)
    ) +
    geom_text(aes(label = .data$center)) +
    labs(
      x = "Durata media",
      y = "PIM medio (per centro)",
      colour = "PIM",
      label = "PIM"
    )
}





#' @describeIn funs-losReport data
#' @export
#' @examples
#'
#' los_dataToUse(full_records, "Completed only")
#' los_dataToUse(full_records, "overall")
los_dataToUse <- function(
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


los_dataTbl <- function(
    .db,
    what = c("diagnosi","diagnosi_2","popc_dimissione","popc_delta","esito_tip","mod_decesso","deceduto",
             "prelievo_organi","destinazione","durata_degenza","smr"),
    by_gender = FALSE,
    by_ageclass = FALSE,
    by_type = FALSE,
    by_year = FALSE

) {
  what <- match.arg(what)

  if (by_gender) {
    .db <- .db |>
      group_by(.data[["gender"]], .add = TRUE)
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
  .db |>
    centervar_tbl(what)
}
