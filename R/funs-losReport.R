#' los module
#'
#' @param .db (data frame) data
#' @param what (chr, default "gender") variable to describe. One of
#'   "gender", or "etnicity"
#'
#' @name funs-losReport
NULL


#' @describeIn funs-losReport plot
#' @export
#' @examples
#'
#' \dontrun{
#'   los_Plot(full_records) # default what = "mal_cronica"
#'   los_Plot(full_records, "mal_cronica") # same as before
#'   los_Plot(full_records, "etnia")
#' }
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



#' @describeIn funs-losReport pim-time plot
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

#' @describeIn funs-losReport smr-time plot
#' @export
smrlos_plot <- function(.db) {
  .db |>
    dplyr::select("center", "durata_degenza", "esito_tip","pim3") |>
    dplyr::group_by(.data$center, .add = TRUE) |>
    dplyr::summarise(
      #smr_pim2 = sum(.data$esito_tip=="morto", na.rm = TRUE) /
      # (sum(.data$pim2, na.rm = TRUE)/100),
      smr = sum(.data$esito_tip=="morto", na.rm = TRUE) /
        (sum(.data$pim3, na.rm = TRUE)/100),
      durata_media = mean(.data$durata_degenza))|>
    ggplot2::remove_missing(
      na.rm = TRUE,
      vars = c("smr", "durata_media")
    ) |>
    dplyr::ungroup() |>
    ggplot(
      aes(.data$durata_media, .data$smr)
    ) +
    geom_text(aes(label = .data$center)) +
    labs(
      x = "Durata media",
      y = "SMR (per centro)",
      colour = "SMR",
      label = "SMR"
    )
}

#' @describeIn funs-losReport smr-vol plot
#' @export
smrlosvol_plot <- function(.db) {
  .db |>
    dplyr::select("center", "esito_tip","pim3") |>
    dplyr::group_by(.data$center, .add = TRUE) |>
    dplyr::summarise(
      smr = sum(.data$esito_tip=="morto", na.rm = TRUE) /
        (sum(.data$pim3, na.rm = TRUE)/100),
      volume = n())|>
    ggplot2::remove_missing(
      na.rm = TRUE,
      vars = c("smr", "volume")
    ) |>
    dplyr::ungroup() |>
    ggplot(
      aes(.data$volume, .data$smr)
    ) +
    geom_text(aes(label = .data$center)) +
    labs(
      x = "Volume ricoveri",
      y = "SMR (per centro)",
      colour = "SMR",
      label = "SMR"
    )
}


#' @describeIn funs-losReport data
#' @export
#' @examples
#'
#' \dontrun{
#'   los_dataToUse(full_records, "Completed only")
#'   los_dataToUse(full_records, "overall")
#' }
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
