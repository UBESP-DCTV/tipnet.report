#' ---
#' title: "TIP-Net"
#' subtitle: "Età mancanti `r params$year`"
#' author: "Unità di Biostatistica, Epidemiologia, e Sanità Pubblica<br>Dipartimento di Scienze Cardio-Toraco-Vascolari e Sanità Pubblica<br>University of Padova"
#' date: "Data di creazione del report: `r Sys.Date()` (ver. 0.1.1)"
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
#'   year: 2019
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
  alt = 'logo',
  style = 'position:absolute; top:0; right:0; padding:10px',
  width = '15%')




if (interactive()) {
  params <- list(year = 2019)
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
  panderOptions("table.alignment.default",
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
    description = paste0("TIP-Net età mancanti (", params$year, ")"),
    generator = "RMarkdown"
  ) %>%
  meta_social(
    title = "TIP-Net età mancanti",
    og_type = "website",
    og_author = "UBEP",
    twitter_card_type = "summary",
    twitter_creator = "@CorradoLanera"
  )


#+ data-load
data_dir <- "../tipnet-data"
# db_update_from_server(data_dir)


tip_data <- read_rds(here(data_dir, "2020-11-10-tipnet.rds"))


anagrafica <- tip_data[[3]][[1]] %>%
  select(codpat, gender, etnia, center)

label(anagrafica, self = FALSE) <- c(
  "Codice paziente", "Sesso", "Etnia", "Centro")





accettazione <- tip_data[[3]][[3]] %>%
  filter(year(ingresso_dt) == params$year) %>%
  select(codpat, eta_giorni, center, ingresso_dt)

label(accettazione, self = FALSE) <- c(
  "Codice paziente", "Età (giorni)", "Centro", "Data ingresso")

data_to_describe <- left_join(accettazione, anagrafica) %>%
  select(-etnia) %>%
  mutate(
    center = forcats::fct_relabel(center, ~str_c(
      dplyr::filter(centers_table, center == .x) %>%
        dplyr::pull(center_city),
      .x,
      sep = " - "
    ))
  )

#'
#' # Età mancanti per centro
#'
data_to_describe %>%
    select(center, eta_giorni) %>%
    mutate(eta_giorni = is.na(eta_giorni)) %>%
    table() %>%
    as_tibble() %>%
    pivot_wider(names_from = eta_giorni,
                values_from = n,
                names_prefix = "Eta mancanti: ") %>%
    arrange(desc(`Eta mancanti: TRUE`)) %>%
    DT::datatable(filter = "top")



#'
#' # ID con età mancanti per centro
#'
#' Questi ID sarebbe da "aprirli" uno per uno in REDcap (almeno una
#' volta), per permettere a REDCap stesso di eseguire gli script di
#' calcolo interni su di essi e calcolare le età in automatico.
#'

wrong_id <- tip_data[[3]][[3]] %>%
filter(
  year(ingresso_dt) == params$year,
  is.na(eta_giorni)) %>%
select(center, codpat) %>%
  mutate(
    center = forcats::fct_relabel(center, ~str_c(
      dplyr::filter(centers_table, center == .x) %>%
        dplyr::pull(center_city),
      .x,
      sep = " - "
    ))
  )

DT::datatable(wrong_id, filter = "top")

if (interactive()) {
  walk(wrong_id$center,
       ~write_csv(wrong_id %>% filter(center == .x),
                 file = here::here("analyses/wrong_ids",
                                   paste0(.x, ".csv")))
  )
}
