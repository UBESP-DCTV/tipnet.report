library(tibble)

centers_table <- tibble::tribble(
  ~id, ~center, ~center_city,
  "1", "1", "", # "Ospedale Gaslini",                                  "Genova",
  "2", "2", "", # "Ospedale SS Biagio e Arrigo",                       "Alessandria",
  "3", "3", "", # "Ospedale Infantile Regina Margherita",              "Torino",
  "4", "4", "", # "Ospedale Maggiore della Carità",                    "Novara",
  "5", "5", "", # "Ospedale Buzzi",                                    "Milano",
  "6", "6", "", # "IRCCS Policlinico De Marchi",                       "Milano",
  "7", "7", "", # "Ospedale Civile Maggiore",                          "Verona",
  "8", "8", "", # "Azienda Ospedaliera Universitaria",                 "Padova",
  "9", "9", "", # "IRCCS Burlo Garofalo",                              "Trieste",
  "10", "10", "", # "Ospedale Sant'Orsola Malpighi",                     "Bologna",
  "11", "11", "", # "Ospedale Meyer",                                    "Firenze",
  "12", "12", "", # "Ospedale Bambino Gesù DEA",                         "Roma",
  "13", "13", "", # "Terapia Intensiva Pediatrica",                      "Vicenza",
  "14", "14", "", # "Ospedale Santo Bono",                               "Napoli",
  "15", "15", "", # "Ospedale G Di Cristina",                            "Palermo",
  "16", "16", "", # "",                                                  "<MISSING>",
  "19", "19", "", # "Ospedale Regina Margherita - TIP cardiochirurgica", "Torino",
  "20", "20", "", # "Ospedale Salesi",                                   "Ancona",
  "21", "21", "", # "Ospedale Bambino Gesù TIP",                         "Roma",
  "22", "22", "", # "IRCCS Gemelli",                                     "Roma",
  "23", "23", "", # "Patologia Neonatale Buzzi",                         "Milano",
  "24", "24", "", # "Ospedale Bambino Gesù Patologia Neonatale",         "Roma",
  "25", "25", "", # "Ospedale Bambino Gesù",                             "Palidoro",
  "26", "26", "", # "Ospedale San Donato TIP cardiochirurgica",          "Milano",
  "27", "27", "", # "Spedali Civili",                                    "Brescia",
  "28", "28", "", # "Ospedale Garibaldi Nomisma",                        "Catania",
  "29", "29", "", # "Policlinico TIN TIP",                               "Messina",
  "30", "30", "", # "Ospedale Monaldi TIP cardiochirurgica",             "Napoli",
  "31", "31", "", # "Ospedale Riuniti",                                  "Bergamo",
  "32", "32", "", # "Ospedale Bambino Gesù TIP cardiochirurgica",        "Roma",
  "33", "33", "", # "Ospedale Pediatrico Giovanni XXIII",                "Bari",
  "34", "34", "", # "ASST dei 7 laghi",                                  "Varese",
  "35", "35", "", #  "Azienda Ospedaliero-universitaria",                "Parma"
  "Prova", "Prova", "" # "Prova",                                         "Prova"
) %>%
  dplyr::mutate(
    dplyr::across(dplyr::all_of(c("center", "center_city")), as.factor)
  )

old_ids <- readr::read_csv(
  here::here("data-raw/old_ids.csv"),
  col_names = c("sequential_id", "real_id", "center", "center_city"),
  skip = 1,
  col_types = "iicc"
)

need_privacy <- TRUE
if (need_privacy) {
  old_ids <- old_ids |>
    dplyr::mutate(
      center = real_id
    )
}

comorb <- c(
  "mal_cronica0_1" = "Cardiologica",
  "mal_cronica0_2" = "Metabolica",
  "mal_cronica0_3" = "Neurologica",
  "mal_cronica0_4" = "Neuromuscolare",
  "mal_cronica0_5" = "Onco-ematologica",
  "mal_cronica0_6" = "Renale",
  "mal_cronica0_7" = "Respiratoria",
  "mal_cronica0_8" = "Sindromica",
  "mal_cronica0_9" = "Altro",
  "mal_cronica0_10" = "Malformato",
  "mal_cronica0_11" = "Ex-prematuro",
  "mal_cronica0_12" = "Gastroenterologica",
  "mal_cronica0_13" = "Trapiantologica"
)



trauma <- c(
  "tipo_trauma_1" = "anafilassi",
  "tipo_trauma_2" = "annegamento",
  "tipo_trauma_3" = "avvelenamento",
  "tipo_trauma_4" = "folgorazione",
  "tipo_trauma_5" = "ingestione",
  "tipo_trauma_6" = "intossicazione-CO",
  "tipo_trauma_7" = "ipotermia",
  "tipo_trauma_8" = "soffocamento",
  "tipo_trauma_9" = "addominale",
  "tipo_trauma_10" = "contusione-fegato",
  "tipo_trauma_11" = "contusione-milza",
  "tipo_trauma_12" = "altra-contusione",
  "tipo_trauma_13" = "cranico",
  "tipo_trauma_14" = "facciale",
  "tipo_trauma_15" = "scheletrico",
  "tipo_trauma_16" = "spinale",
  "tipo_trauma_17" = "toracico",
  "tipo_trauma_18" = "ustioni",
  "tipo_trauma_19" = "altro"
)

insuff_organo <- c(
  "insuff_organo_1" = "respiratoria",
  "insuff_organo_2" = "cardiovascolare",
  "insuff_organo_3" = "neurologica",
  "insuff_organo_4" = "renale",
  "insuff_organo_5" = "emocoagulativa",
  "insuff_organo_6" = "epatica"
)

insuff_organo_ricovero <- c(
  "insuff_organo_2_1" = "respiratoria",
  "insuff_organo_2_2" = "cardiovascolare",
  "insuff_organo_2_3" = "neurologica-acuta",
  "insuff_organo_2_4" = "renale",
  "insuff_organo_2_5" = "emocoagulativa",
  "insuff_organo_2_6" = "epatica"
)

procedure_ricovero <- c(
  "procedure_ric_1" = "02-alto_flusso",
  "procedure_ric_4" = "02-basso_flusso",
  "procedure_ric_2" = "ventilazione",
  "procedure_ric_3" = "accesso-vascolare",
  "procedure_ric_5" = "drenaggio-pericardico",
  "procedure_ric_7" = "drenaggio-toracico",
  "procedure_ric_8" = "drenaggio-addominale",
  "procedure_ric_6" = "peg",
  "procedure_ric_10" = "broncoscopia",
  "procedure_ric_11" = "tracheotomia",
  "procedure_ric_12" = "cateterismo-cardiaco",
  "procedure_ric_13" = "intervento-chirurgico",
  "procedure_ric_14" = "tratt-dialitico",
  "procedure_ric_17" = "altro",
  "procedure_ric_18" = "VAD",
  "procedure_ric_19" = "Rashkind",
  "procedure_ric_20" = "RCP",
  "procedure_ric_15" = "ECMO",
  "procedure_ric_16" = "PIC"
)
nutrizione <- c(
  "nutrizione_1" = "enterale",
  "nutrizione_2" = "NPT",
  "nutrizione_3" = "os",
  "nutrizione_4" = "idratazione"
)

ventilazione <- c(
  "vam0_1" = "restrittivo-primario",
  "vam0_2" = "restrittico-secondario",
  "vam0_3" = "ostruttivo",
  "vam0_4" = "coma",
  "vam0_5" = "sepsi",
  "vam0_6" = "post-operatoria"
)

usethis::use_data(centers_table, old_ids, comorb, trauma, insuff_organo,
  insuff_organo_ricovero,
  procedure_ricovero,
  nutrizione, ventilazione,
  internal = TRUE,
  overwrite = TRUE
)
