library(tibble)

centers_table <- tibble::tribble(
  ~id, ~center,                                             ~center_city,
   "1", "Ospedale Gaslini",                                  "Genova",
   "2", "Ospedale SS Biagio e Arrigo",                       "Alessandria",
   "3", "Ospedale Infantile Regina Margherita",              "Torino",
   "4", "Ospedale Maggiore della Carità",                    "Novara",
   "5", "Ospedale Buzzi",                                    "Milano",
   "6", "IRCCS Policlinico De Marchi",                       "Milano",
   "7", "Ospedale Civile Maggiore",                          "Verona",
   "8", "Azienda Ospedaliera Universitaria",                 "Padova",
   "9", "IRCCS Burlo Garofalo",                              "Trieste",
  "10", "Ospedale Sant'Orsola Malpighi",                     "Bologna",
  "11", "Ospedale Meyer",                                    "Firenze",
  "12", "Ospedale Bambino Gesù DEA",                         "Roma",
  "13", "Terapia Intensiva Pediatrica",                      "Vicenza",
  "14", "Ospedale Santo Bono",                               "Napoli",
  "15", "Ospedale G Di Cristina",                            "Palermo",
  "16", "",                                                  "<MISSING>",
  "19", "Ospedale Regina Margherita - TIP cardiochirurgica", "Torino",
  "20", "Ospedale Salesi",                                   "Ancona",
  "21", "Ospedale Bambino Gesù TIP",                         "Roma",
  "22", "IRCCS Gemelli",                                     "Roma",
  "23", "Patologia Neonatale Buzzi",                         "Milano",
  "24", "Ospedale Bambino Gesù Patologia Neonatale",         "Roma",
  "25", "Ospedale Bambino Gesù",                             "Palidoro",
  "26", "Ospedale San Donato TIP cardiochirurgica",          "Milano",
  "27", "Spedali Civili",                                    "Brescia",
  "28", "Ospedale Garibaldi Nomisma",                        "Catania",
  "29", "Policlinico TIN TIP",                               "Messina",
  "30", "Ospedale Monaldi TIP cardiochirurgica",             "Napoli",
  "31", "Ospedale Riuniti",                                  "Bergamo",
  "32", "Ospedale Bambino Gesù TIP cardiochirurgica",        "Roma",
  "33", "Ospedale Pediatrico Giovanni XXIII",                "Bari",
  "Prova", "Prova",                                         "Prova"
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

usethis::use_data(centers_table, old_ids,
  internal = TRUE,
  overwrite = TRUE
)
