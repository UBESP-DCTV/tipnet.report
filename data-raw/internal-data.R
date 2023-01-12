library(tibble)

centers_table <- tibble::tribble(
  ~id, ~center,                                             ~center_city,
   "1","1","",# "Ospedale Gaslini",                                  "Genova",
   "2","2","",# "Ospedale SS Biagio e Arrigo",                       "Alessandria",
   "3","3","",# "Ospedale Infantile Regina Margherita",              "Torino",
   "4","4","",# "Ospedale Maggiore della Carità",                    "Novara",
   "5","5","",# "Ospedale Buzzi",                                    "Milano",
   "6","6","",# "IRCCS Policlinico De Marchi",                       "Milano",
   "7","7","",# "Ospedale Civile Maggiore",                          "Verona",
   "8","8","",# "Azienda Ospedaliera Universitaria",                 "Padova",
   "9","9","",# "IRCCS Burlo Garofalo",                              "Trieste",
  "10","10","",# "Ospedale Sant'Orsola Malpighi",                     "Bologna",
  "11","11","",# "Ospedale Meyer",                                    "Firenze",
  "12","12","",# "Ospedale Bambino Gesù DEA",                         "Roma",
  "13","13","",# "Terapia Intensiva Pediatrica",                      "Vicenza",
  "14","14","",# "Ospedale Santo Bono",                               "Napoli",
  "15","15","",# "Ospedale G Di Cristina",                            "Palermo",
  "16","16","",# "",                                                  "<MISSING>",
  "19","19","",# "Ospedale Regina Margherita - TIP cardiochirurgica", "Torino",
  "20","20","",# "Ospedale Salesi",                                   "Ancona",
  "21","21","",# "Ospedale Bambino Gesù TIP",                         "Roma",
  "22","22","",# "IRCCS Gemelli",                                     "Roma",
  "23","23","",# "Patologia Neonatale Buzzi",                         "Milano",
  "24","24","",# "Ospedale Bambino Gesù Patologia Neonatale",         "Roma",
  "25","25","",# "Ospedale Bambino Gesù",                             "Palidoro",
  "26","26","",# "Ospedale San Donato TIP cardiochirurgica",          "Milano",
  "27","27","",# "Spedali Civili",                                    "Brescia",
  "28","28","",# "Ospedale Garibaldi Nomisma",                        "Catania",
  "29","29","",# "Policlinico TIN TIP",                               "Messina",
  "30","30","",# "Ospedale Monaldi TIP cardiochirurgica",             "Napoli",
  "31","31","",# "Ospedale Riuniti",                                  "Bergamo",
  "32","32","",# "Ospedale Bambino Gesù TIP cardiochirurgica",        "Roma",
  "33","33","",# "Ospedale Pediatrico Giovanni XXIII",                "Bari",
  "Prova","Prova",""# "Prova",                                         "Prova"
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
  'mal_cronica0_1' = 'Cardiologica',
  'mal_cronica0_2' = 'Metabolica',
  'mal_cronica0_3' = 'Neurologica',
  'mal_cronica0_4' = 'Neuromuscolare',
  'mal_cronica0_5' = 'Onco-ematologica',
  'mal_cronica0_6' = 'Renale',
  'mal_cronica0_7' = 'Respiratoria',
  'mal_cronica0_8' = 'Sindromica',
  'mal_cronica0_9' = 'Altro',
  'mal_cronica0_10' = 'Malformato',
  'mal_cronica0_11' = 'Ex-prematuro',
  'mal_cronica0_12' = 'Gastroenterologica',
  'mal_cronica0_13' = 'Trapiantologica'
)

usethis::use_data(centers_table, old_ids, comorb,
  internal = TRUE,
  overwrite = TRUE
)
