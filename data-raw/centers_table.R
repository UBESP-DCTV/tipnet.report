library(tibble)

centers_table <- tibble::tribble(
  ~id, ~center,                                             ~center_city,
   "1L", "Ospedale Gaslini",                                  "Genova",
   "2L", "Ospedale SS Biagio e Arrigo",                       "Alessandria",
   "3L", "Ospedale Infantile Regina Margherita",              "Torino",
   "4L", "Ospedale Maggiore della Carità",                    "Novara",
   "5L", "Ospedale Buzzi",                                    "Milano",
   "6L", "IRCCS Policlinico De Marchi",                       "Milano",
   "7L", "Ospedale Civile Maggiore",                          "Verona",
   "8L", "Azienda Ospedaliera Universitaria",                 "Padova",
   "9L", "IRCCS Burlo Garofalo",                              "Trieste",
  "10L", "Ospedale Sant'Orsola Malpighi",                     "Bologna",
  "11L", "Ospedale Meyer",                                    "Firenze",
  "12L", "Ospedale Bambino Gesù DEA",                         "Roma",
  "13L", "Terapia Intensiva Pediatrica",                      "Vicenza",
  "14L", "Ospedale Santo Bono",                               "Napoli",
  "15L", "Ospedale G Di Cristina",                            "Palermo",
  "16L", "",                                                  "<MISSING>",
  "19L", "Ospedale Regina Margherita - TIP cardiochirurgica", "Torino",
  "20L", "Ospedale Salesi",                                   "Ancona",
  "21L", "Ospedale Bambino Gesù TIP",                         "Roma",
  "22L", "IRCCS Gemelli",                                     "Roma",
  "23L", "Patologia Neonatale Buzzi",                         "Milano",
  "24L", "Ospedale Bambino Gesù Patologia Neonatale",         "Roma",
  "25L", "Ospedale Bambino Gesù",                             "Palidoro",
  "26L", "Ospedale San Donato TIP cardiochirurgica",          "Milano",
  "27L", "Spedali Civili",                                    "Brescia",
  "28L", "Ospedale Garibaldi Nomisma",                        "Catania",
  "29L", "Policlinico TIN TIP",                               "Messina",
  "30L", "Ospedale Monaldi TIP cardiochirurgica",             "Napoli",
  "31L", "Ospedale Riuniti",                                  "Bergamo",
  "32L", "Ospedale Bambino Gesù TIP cardiochirurgica",        "Roma",
  "33L", "Ospedale Pediatrico Giovanni XXIII",                "Bari",
  "Prova", "Prova",                                         "Prova"
) %>%
  dplyr::mutate_if(is.character, as.factor)

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
