#' Origin module
#'
#' @param .db (data frame) data
#' @name funs-OriginReport
NULL

#' @describeIn funs-OriginReport plot
#' @export
#' @examples
#'
#'#' @describeIn funs-OriginReport data
#' @export
#' @examples
#'
#' descriptives_dataToUse(full_records, "Completed only")
#' descriptives_dataToUse(full_records, "overall")
descriptives_dataToUse <- function(
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







#' descriptives_originPlot(full_records)
descriptives_originPlot <- function(.db) {

  .db %>% filter(is_prematuro=="no") %>%
    ggplot(aes(x = .data$center, fill = .data$provenienza)) +
    geom_bar(position = "dodge") +
    coord_flip()
}
#' descriptives_tipologiaPlot(full_records)

descriptives_tipologiaPlot <- function(.db) {

  .db %>% filter(eta>=0) %>%
    ggplot(aes(x = .data$center, fill = .data$tipologia2)) +
    geom_bar(position = "dodge") +
    coord_flip()
}
#' descriptives_altroospPlot(full_records)

descriptives_altroospPlot <- function(.db) {

  .db %>% filter(provenienza=="altro ospedale") %>%
    ggplot(aes(x = .data$center, fill = .data$altro_osp)) +
    geom_bar(position = "dodge") +
    coord_flip()
}
#' descriptives_tipochirPlot(full_records)

descriptives_tipochirPlot <- function(.db) {

  .db %>% filter(tipologia2=="Chirurgico") %>%
    ggplot(aes(x = .data$center, fill = .data$tipo_chir)) +
    geom_bar(position = "dodge") +
    coord_flip()
}
