#' ---
#' title: "TIP-Net"
#' subtitle: "Report dati `r params$year` (mesi: `r params$first_month` - `r params$last_month`)"
#' author: "Unità di Biostatistica, Epidemiologia, e Sanità Pubblica<br>Dipartimento di Scienze Cardio-Toraco-Vascolari e Sanità Pubblica<br>University of Padova"
#' date: "Data di creazione del report: `r Sys.Date()` (ver. 1.0.0)"
#' output:
#'   bookdown::html_document2:
#'     toc: true
#'     toc_float: true
#'     keep_md: true
#'     fig_width: 10
#'     fig_height: 5
#'     fig_caption: true
#'
#' params:
#'   year: 2024
#'   first_month: 7
#'   last_month: 12
#' ---
#'
#'
#' ```{r setup, include=FALSE}
#' knitr::opts_chunk$set(
#'
#'     echo = FALSE,
#'     comment = "",
#'     collapse = TRUE,
#'     warning = FALSE,
#'     message = FALSE,
#'
#'     out.width  = "100%",  # part of output covered
#'     dpi = 192              # set output resolution (impact weight)
#' )
#'
#' options(width = 150)
#' set.seed(1)
#' ```
#'

#+ echo=FALSE
htmltools::img(
  src = knitr::image_uri("img/ubep_logo.jpg"),
  alt = "logo",
  style = "position:absolute; top:0; right:0; padding:10px",
  width = "15%"
)




if (interactive()) {
  params <- list(
    year = 2024,
    first_month = 7,
    last_month = 12
  )
}

report_type <- if (params$first_month == 1) {
  if (params$last_month == 6) "I semester" else "annual"
} else {
  if (params$first_month == 7 && params$last_month == 12) {
    "II semester"
  } else {
    ui_stop("month paramenters wrongly set")
  }
}


#+ pkg, include = FALSE

suppressPackageStartupMessages({
  # load project functions
  if (
    fs::file_exists(here::here("tipnet.report.Rproj")) &&
      fs::file_exists(here::here("DESCRIPTION"))
  ) {
    devtools::load_all(quiet = FALSE)
  } else {
    # remotes::install_github("UBESP-DCTV/tipnet.report")
    library(tipnet.report)
  }

  # data management
  library(tidyverse)
  library(lubridate)
  library(janitor)

  # utilities
  library(depigner)
  library(glue)
  library(here)

  # rendering
  library(pander)
  panderOptions("round", 2)
  panderOptions("knitr.auto.asis", FALSE)
  panderOptions("table.split.table", Inf)
  panderOptions(
    "table.alignment.default",
    function(df) c("left", rep("center", length(df) - 1L))
  )

  # data analyses
  library(rms)
  options(datadist = "dd")

  # metadata
  library(metathis)
})


#+ echo=FALSE
meta() %>%
  meta_general(
    description = paste0("TIP-Net ", report_type, " report (", params$year, ")"),
    generator = "RMarkdown"
  ) %>%
  meta_social(
    title = paste0("TIP-Net ", report_type, " report (", params$year, ")"),
    og_type = "website",
    og_author = "UBEP",
    twitter_card_type = "summary",
    twitter_creator = "@CorradoLanera"
  )


#+ data-load
data_dir <- "../tipnet-data"
# db_update_from_server(data_dir)


tip_data <- read_rds(here(data_dir, "tipnet.rds"))

#'
#' # Preambolo
#'
#' Il seguente report riporta misure di sintesi dei dati della rete
#' TIP-Net per l'anno `r params$year`. Nel capitolo \@ref(general) sono
#' riportate le sintesi per l'intera rete TIP-Net, mentre nel capitolo
#' \@ref(centers) sono riportate le sintesi divise, in ciascuna
#' sezione, per centro.
#'
#' Per ogni sezione (sia generale che per i centri) sono disponibili tre
#' tab (accessibili facendo _click_ sul loro nome):
#'
#'  - **Accettazione**: distribuzioni e caratteristiche per `genere` ed
#'    `etnia`
#'  - **Descrittive**: per ciascuna caratteristica di interesse si
#'    riportano la numerosità (N = dati non mancanti), la distribuzione
#'    (I, II, e III quartile) con media e deviazione standard per le
#'    variabili continue, mentre frequenza e numerosità assoluta per le
#'    variabili discrete. Tutte le metriche sono riportate sia
#'    stratificate per sesso (_maschio_ o _femmina_), che globali
#'    (_combined_)
#'  - **SMR**: _Standardized Mortality Rate_ riportate sia relativamente
#'    allo score `PIM2` che `PIM3`.
#'

#'
#' # Report generale {#general}
#' ## TIP-Net {.tabset .tabset-fade .tabset-pills}
#'
anagrafica <- tip_data[[3]][[1]] %>%
  select(codpat, gender, etnia, center)

label(anagrafica, self = FALSE) <- c(
  "Codice paziente", "Sesso", "Etnia", "Centro"
)




accettazione <- tip_data[[3]][[3]] %>%
  filter(
    !is.na(ingresso_dt),
    year(ingresso_dt) == params$year,
    month(ingresso_dt) >= params$first_month,
    month(ingresso_dt) <= params$last_month
  ) %>%
  select(
    codpat, eta_giorni, priorita, ricovero_progr, tipologia,
    motivo_ricovero, redcap_repeat_instance, starts_with("mal_cronica"),
    center
  ) %>%
  mutate(
    motivo_ricovero = ordered(motivo_ricovero,
      levels = sort(unique(motivo_ricovero))
    ),
    age_class = case_when(
      is.na(eta_giorni) ~ "[dato mancante]",
      eta_giorni < 0 ~ "[giorni negativi]",
      eta_giorni <= 30 ~ "neonati",
      eta_giorni <= 365.25 ~ "lattanti",
      eta_giorni <= 365.25 * 5 ~ "prescolare",
      eta_giorni <= 365.25 * 12 ~ "scolare",
      eta_giorni <= 365.25 * 18 ~ "adolescente",
      eta_giorni > 365.25 * 18 ~ "adulto",
      TRUE ~ as.character(eta_giorni)
    ) %>%
      ordered(c(
        "neonati", "lattanti", "prescolare", "scolare",
        "adolescente", "adulto", "[giorni negativi]",
        "[dato mancante]"
      )),
    across(mal_cronica0_1:mal_cronica0_11, ~ !is.na(.x))
  ) %>%
  rowwise() %>%
  mutate(
    n_comorb = sum(c_across(mal_cronica0_1:mal_cronica0_11))
  )

label(accettazione, self = FALSE) <- c(
  "Codice paziente", "Età (giorni)", "Priorità", "Ricovero programmato",
  "Tipologia di ricovero", "Motivo del ricovero", "Redcap rep-id",
  "Presenza di comorbidità", "Comorbidità: cardiologica",
  "Comorbidità: metabolica", "Comorbidità: neurologica",
  "Comorbidità: neuromuscolare", "Comorbidità: onco-ematologica",
  "Comorbidità: renale", "Comorbidità: respiratoria",
  "Comorbidità: sindromica", "Comorbidità: altro",
  "Comorbidità: malformato/esiti di malformazione",
  "Comorbidità: ex-prematuro", "Comorbidità: gastroenterologica",
  "Comorbidità: trapiantologica", "Centro", "Classe di età",
  "Numero di comorbidità"
)

accettazione <- accettazione %>%
  dplyr::relocate(age_class, .after = eta_giorni) %>%
  dplyr::relocate(mal_cronica0_9, .after = mal_cronica0_11) %>%
  dplyr::relocate(n_comorb, .after = mal_cronica0_9)


pim <- tip_data[[3]][[5]] %>%
  select(codpat, pim2, pim3, redcap_repeat_instance)


label(pim, self = FALSE) <- c(
  "Codice paziente", "PIM 2", "PIM 3", "Redcap rep-id"
)




ventilazione <- tip_data[[3]][[9]] %>%
  select(codpat, niv_it, redcap_repeat_instance)

label(ventilazione, self = FALSE) <- c(
  "Codice paziente", "Tecnica di ventilazione", "Redcap rep-id"
)




infezione <- tip_data[[3]][[10]] %>%
  select(codpat, tipo_inf, redcap_repeat_instance)

label(infezione, self = FALSE) <- c(
  "Codice paziente", "Tipologia di infezione", "Redcap rep-id"
)




dimissione <- tip_data[[3]][[13]] %>%
  select(
    codpat, durata_degenza, mod_decesso, esito_tip,
    diagnosi, redcap_repeat_instance
  ) %>%
  mutate(
    mod_decesso = ordered(mod_decesso,
      levels = sort(unique(mod_decesso))
    ),
    diagnosi = diagnosi %>%
      ordered(levels = c(
        sort(setdiff(unique(diagnosi), "altro")), "altro"
      ))
  )

label(dimissione, self = FALSE) <- c(
  "Codice paziente", "Durata della degenza", "Modalità di decesso",
  "Esito TIP", "Diagnosi alla dimissione", "Redcap rep-id"
)




data_to_describe <- left_join(accettazione, pim) %>%
  left_join(ventilazione) %>%
  left_join(infezione) %>%
  left_join(dimissione) %>%
  left_join(anagrafica) %>%
  select(-etnia)

if (centers_table[["center_city"]][[1]] != "") {
  data_to_describe <- data_to_describe %>%
    mutate(
      center = forcats::fct_relabel(center, ~ str_c(
        dplyr::filter(centers_table, center == .x)[["center_city"]],
        .x,
        sep = " - "
      ))
    )
}

eval_summaries <- function(tip_data, current_center = NULL) {
  tip_data <- tip_data %>%
    dplyr::ungroup()

  if (!is_null(current_center)) {
    tip_data <- tip_data %>%
      dplyr::filter(center == current_center)
  }

  if (!nrow(tip_data)) {
    return(NULL)
  }

  tip_data <- dplyr::select(tip_data, -center)

  Accettazione <- anagrafica %>%
    dplyr::inner_join(tip_data) %>%
    dplyr::select(gender, etnia)

  names(Accettazione) <- label(Accettazione)
  label(Accettazione, self = FALSE) <- c("", "")

  desc_base <- Hmisc::describe(Accettazione)

  desc_tip <- summary(
    formula = gender ~ .,
    data = tip_data |>
      dplyr::select(-codpat, -redcap_repeat_instance) %>%
      remove_empty(which = c("rows", "cols")),
    method = "reverse",
    overall = TRUE,
    continue = 3
  ) %>%
    depigner::tidy_summary(digits = 3, exclude1 = FALSE, long = TRUE, prmsd = TRUE)


  smr <- tip_data %>%
    dplyr::summarise(
      SMR_pim2 = sum(.data$esito_tip == "morto", na.rm = TRUE) /
        (sum(.data$pim2, na.rm = TRUE) / 100),
      SMR_pim3 = sum(.data$esito_tip == "morto", na.rm = TRUE) /
        (sum(.data$pim3, na.rm = TRUE) / 100)
    )

  list(desc_base = desc_base, desc_tip = desc_tip, smr = smr)
}

safe_eval <- purrr::safely(eval_summaries)

overall <- safe_eval(data_to_describe)

#'
#' ### Accettazione
#'
html(overall[["result"]][[1]], header = c("a", "b"))

#'
#' ### Descrittive
#'
#+ results='asis'
pander(overall[["result"]][[2]])

#'
#' ### SMR
#'
#+ results='asis'
cat(
  " \nSMR PIM2 (overall): ", round(overall[["result"]][[3]][[1]], 2), "\n",
  " \nSMR PIM3 (overall): ", round(overall[["result"]][[3]][[2]], 2), "\n\n"
)


named_ids <- centers_table$id %>%
  set_names(as.character(centers_table$center))

centers <- levels(data_to_describe$center) %>%
  set_names(paste0(
    "DAG: ", named_ids[str_remove(., "^.*? - ")]
  ))

center_summaries <- centers %>%
  purrr::set_names(
    glue::glue("DAG encoded: {stringr::str_remove(., ' \\\\(\\\\)')}")
  ) %>%
  purrr::map(safe_eval, tip_data = data_to_describe)




# aa <- transpose(center_summaries)

#'
#' # Report centri {#centers}
#'

#+ results='asis'
iwalk(center_summaries, ~ {
  cat(" \n## ", .y, " {.tabset .tabset-fade .tabset-pills} \n")

  if (is.null(.x[["result"]])) {
    cat(" \n > no data for this center \n\n")
  } else {
    cat(" \n### Accettazione \n")
    cat(" \n", html(.x[["result"]][[1]]), " \n")

    cat(" \n### Descrittive \n")
    cat(
      " \n",
      pander(.x[["result"]][[2]]),
      " \n"
    )

    cat(" \n### SMR \n")
    cat(
      " \nSMR PIM2: ", round(.x[["result"]][[3]][[1]], 2), "\n",
      " \nSMR PIM3: ", round(.x[["result"]][[3]][[2]], 2), "\n\n"
    )
  }
})
