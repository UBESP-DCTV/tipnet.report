utils::globalVariables("where")



#' Global / extensive data setup
#'
#' Here put data that should be cached at the server startup, and
#' computed only there once.
#'
#' @export
generate_main_data <- function() {

  tipnet <- fetch_tipnet()
  sheets <- extract_sheets(tipnet)
  full_records <- join_all_sheets(sheets)

  c(sheets, list(full_records = full_records))

}








fetch_tipnet <- function() {
  readr::read_rds(file.path(data_path(), "tipnet.rds"))
}








extract_sheets <- function(tipnet) {
  c(
    anagrafica = list(
      get_sheet(tipnet, "anagrafica", field = "anagrafica")
    ),

    purrr::set_names(c(
      "accettazione", "ingresso", "degenza", "infezione", "dimissione",
      "punteggio_di_aristotle", "pelod_scheda_facoltativa", "pim",
      "procedure_di_ventilazione"
    )) %>%
      purrr::map(get_sheet, x = tipnet)
  )
}








join_all_sheets <- function(sheets) {

  sheets[["accettazione"]] %>%
    dplyr::full_join(sheets[["anagrafica"]],
                     by = c("codpat", "center"),
                     suffix = c(".accettazione", ".anagrafica")
    ) %>%

    dplyr::full_join(sheets[["ingresso"]],
                     by = c("codpat", "center", "redcap_repeat_instance")
    ) %>%
    dplyr::full_join(sheets[["pim"]],
                     by = c("codpat", "center", "redcap_repeat_instance"),
                     suffix = c(".ingresso", ".pim")
    ) %>%

    dplyr::full_join(sheets[["pelod_scheda_facoltativa"]],
                     by = c("codpat", "center", "redcap_repeat_instance")
    ) %>%
    dplyr::full_join(sheets[["punteggio_di_aristotle"]],
                     by = c("codpat", "center", "redcap_repeat_instance"),
                     suffix = c(".pelod", ".aristotle")
    ) %>%

    dplyr::full_join(sheets[["degenza"]],
                     by = c("codpat", "center", "redcap_repeat_instance")
    ) %>%
    dplyr::full_join(sheets[["procedure_di_ventilazione"]],
                     by = c("codpat", "center", "redcap_repeat_instance"),
                     suffix = c(".degenza", ".ventilazione")
    ) %>%

    dplyr::full_join(sheets[["infezione"]],
                     by = c("codpat", "center", "redcap_repeat_instance")
    ) %>%
    dplyr::full_join(sheets[["dimissione"]],
                     by = c("codpat", "center", "redcap_repeat_instance"),
                     suffix = c(".infezione", ".dimissione")
    ) %>%

    dplyr::mutate_at(
      dplyr::vars(dplyr::starts_with("complete")), ~{
        . == "complete"
      }
    ) %>%
    dplyr::filter(!is.na(center)) |>
    dplyr::mutate(
      codpat = as.factor(.data$codpat),
      eta = as.integer(.data[["eta"]]),
      tipologia2=factor(
        dplyr::case_when(
          .data[["tipologia"]]=="medico"~ "Medico",
          .data[["tipologia"]]=="chirurgico"~ "Chirurgico",
          .data[["tipologia"]]=="trauma, ustione, intossicazione, annegamento, avvelenamento, folgorazione, inalazione di fumo, ipotermia, soffocamento"~ "Trauma",
          TRUE ~ "[missing tipologia]"
        )
      ),
      tipologia2 =  forcats::fct_rev(tipologia2),
      delta_popc = as.numeric(.data[["popc_dimissione"]]) - as.numeric(.data[["popc"]]) ,
      popc_delta = as.factor(
        dplyr::case_when(
          .data[["delta_popc"]] < 0~ "Peggioramento",
          .data[["delta_popc"]] == 0 ~ "Stabile",
          .data[["delta_popc"]] > 0~ "Miglioramento",
          TRUE ~ "[missing delta_popc]"
        )),
          niv1_gg = (niv1del_dt %--% niv1al_dt)/days(1),
          niv2_gg = (niv2del_dt %--% niv2al_dt)/days(1),
          niv3_gg = (niv3del_dt %--% niv3al_dt)/days(1),
          niv4_gg = (niv4del_dt %--% niv4al_dt)/days(1),
          it1_gg = (it1del_dt %--% it1al_dt)/days(1),
          it2_gg = (it2del_dt %--% it2al_dt)/days(1),
          it3_gg = (it3del_dt %--% it3al_dt)/days(1),
          niv1_gg = if_else(is.na(.data$niv1_gg), 0, .data$niv1_gg),
          niv2_gg = if_else(is.na(.data$niv2_gg), 0, .data$niv2_gg),
          niv3_gg = if_else(is.na(.data$niv3_gg), 0, .data$niv3_gg),
          niv4_gg = if_else(is.na(.data$niv4_gg), 0, .data$niv4_gg),
          it1_gg = if_else(is.na(.data$it1_gg), 0, .data$it1_gg),
          it2_gg = if_else(is.na(.data$it2_gg), 0, .data$it2_gg),
          it3_gg = if_else(is.na(.data$it3_gg), 0, .data$it3_gg),
          niv_tot = niv1_gg + niv2_gg + niv3_gg + niv4_gg,
          it_tot = it1_gg + it2_gg + it3_gg,
         niv_it_tot = niv_tot + it_tot,
          niv_it_tot = ifelse(niv_it_tot < 0,0,niv_it_tot),
         niv_it_tot = ifelse(niv_it_tot >=500,0,niv_it_tot),
          popc_delta =  forcats::fct_rev(popc_delta),
          popc_dimissione = as.factor(.data$popc_dimissione),
          popc = factor(
        dplyr::case_when(
          .data[["popc"]] == 1 ~ "1 - Buona performance globale",
          .data[["popc"]] == 2 ~ "2 - Lieve disabilità globale",
          .data[["popc"]] == 3 ~ "3 - Moderata disabilità globale",
          .data[["popc"]] == 4 ~ "4 - Grave disabilità globale",
          .data[["popc"]] == 5 ~ "5 - Coma",
          .data[["popc"]] == 6 ~ "6 - Morte cerebrale",
           TRUE ~ "[missing popc]")),
     popc =  forcats::fct_relevel(popc, "[missing popc]", after = 6),
     popc_dimissione = as.factor(
       dplyr::case_when(
         .data[["popc_dimissione"]]== 1 ~ "1 - Buona performance globale",
         .data[["popc_dimissione"]] == 2 ~ "2 - Lieve disabilità globale",
         .data[["popc_dimissione"]] == 3 ~ "3 - Moderata disabilità globale",
         .data[["popc_dimissione"]] == 4 ~ "4 - Grave disabilità globale",
         .data[["popc_dimissione"]] == 5 ~ "5 - Coma",
         .data[["popc_dimissione"]] == 6 ~ "6 - Morte cerebrale",
         TRUE ~ "[missing popc dimissione]")),
     popc_dimissione =  forcats::fct_relevel(popc, "[missing popc]", after = 6),
     mod_decesso = as.factor(
       dplyr::case_when(
         .data[["mod_decesso"]]== "morte nonostante rianimazione cardiopolmonare" ~ "morte nonostante RCP",
         .data[["mod_decesso"]]== "sospensione dei trattamenti di supporto vitale" ~ "sospensione dei TSV",
         .data[["mod_decesso"]] == "astensione dall'iniziare trattamenti di supporto vitale" ~ "astensione dall'iniziare TSV",
         .data[["mod_decesso"]] == "decisione di non rianimare" ~ "decisione di non rianimare",
         .data[["mod_decesso"]] == "morte cerebrale" ~ "morte cerebrale",
         TRUE ~ "[missing mod_decesso]")),
     mod_decesso =  forcats::fct_rev(mod_decesso),

      motivo_ricovero2  =  forcats::fct_recode(motivo_ricovero,"alterato sensorio" = "alterato sensorio / crisi convulsive","disordini metabolici" = "disordini metabolici / disidratazione","insufficienza cardiocircolatoria" = "insufficienza cardiocircolatoria (no shock settico)",
                 "diagnosi sepsi correlata" = "diagnosi sepsi correlata (di natura diversa da respiro cuore snc)","shock distributivo" = "shock distributivo (settico)","arresto" = "arresto cardiocircolatorio","programmato" = "programmato per procedure invasive"),
   sede_inf2  =  forcats::fct_recode(sede_inf,"polmone" = "polmone (infezione di comunità)","polmone - vap" = "polmone - vap (inf. nosocomiale associata al ventilatore)","snc (ventr.)" = "snc (da derivazione ventricolare)",
                                            "snc (non ventr.)" = "snc (non da derivazione ventricolare)","vie aree inf." = "vie aree inferiori (non polmonite)","vie aeree sup." = "vie aeree superiori"),
   vent_iniz2  =  forcats::fct_recode(vent_iniz,"prima dell'ingresso" = "prima dell'ingresso in rianimazione","all'ingresso" = "all'ingresso (entro la prima ora) in rianimazione","durante la degenza" = "durante la degenza in rianimazione"),

   motivo_ric_trauma2  =  forcats::fct_recode(motivo_ric_trauma,"insufficienza cardiovascolare" = "insufficienza cardiovascolare (shock emorragico)"),
   diagnosi_inf  =  forcats::fct_recode(diagnosi_inf,"accertata" = "accertata (check con esame colturale positivo e microrganismo compilato)"),
   tipo_inf  =  forcats::fct_recode(tipo_inf,"nosocomiale" = "nosocomiale (dopo 48h da inizio ospedalizzazione)"),
      age_class = age_to_class(.data[["eta"]], .data[["eta_giorni"]]),
      complete =
        .data[["complete.anagrafica"]] &
        .data[["complete.accettazione"]] &
        .data[["complete.pim"]] &
        .data[["complete.dimissione"]]
    )



}








extract_outliers <- function(full_records) {

  full_records[["codpat"]] <- as.character(full_records[["codpat"]])
  full_records[["center"]] <- as.character(full_records[["center"]])

  aux_out <- full_records %>%
    dplyr::select(
      .data$center, .data$codpat, .data$redcap_repeat_instance,
      where(is.numeric)
    ) %>%
    dplyr::mutate_at(
      dplyr::vars(
        -.data$center, -.data$codpat, -.data$redcap_repeat_instance
      ),
      ~ abs(. - median(., na.rm = TRUE)) >
        1.5 * diff(quantile(., probs = c(0.25, 0.75), na.rm = TRUE))
    ) %>%
    janitor::remove_empty("cols") %>%
    dplyr::select(
      .data$center, .data$codpat, .data$redcap_repeat_instance,
      where(~any(. == TRUE, na.rm = TRUE))
    ) %>%
    tidyr::pivot_longer(
      -c(.data$center, .data$codpat, .data$redcap_repeat_instance),
      names_to = "variable"
    ) %>%
    dplyr::group_by(.data$center) %>%
    dplyr::mutate(
      n = dplyr::n()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$value) %>%
    dplyr::select(-.data$value)



  data_lst <- unclass(full_records)


  aux_out %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      value = extract_value(data_lst,
        .data[["codpat"]],
        .data[["redcap_repeat_instance"]],
        .data[["variable"]]
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      dplyr::across(c("center", "codpat", "variable"), as.factor)
    ) %>%
    dplyr::nest_by(.data$center) %>%
    dplyr::mutate(
      n_outliers = nrow(.data$data),
      prop_outliers = .data$n_outliers / .data$data[["n"]][[1L]],
      data = list(dplyr::select(.data$data, -.data$n))
    ) %>%
    dplyr::ungroup()
}








extract_value <- function(data_lst, codpat, instance, variable) {

  cod <- data_lst[["codpat"]] == codpat

  if (sum(cod) == 1L) {
    data_lst[[variable]][cod]
  } else {
    rep <- data_lst[["redcap_repeat_instance"]] == instance
    data_lst[[variable]][cod & rep]
  }
}
