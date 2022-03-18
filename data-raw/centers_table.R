centers_table <- tibble::tribble(
  ~id, ~center,                                             ~center_city,
   1L, "Ospedale Gaslini (Genova)",                         "Genova",
   2L, "Ospedale SS Biagio e Arrigo (Alessandria)",         "Alessandria",
   3L, "Ospedale Infantile Regina Margherita (Torino)",     "Torino",
   4L, "Ospedale Maggiore della Carità (Novara)",           "Novara",
   5L, "Ospedale Buzzi (Milano)",                           "Milano",
   6L, "IRCCS Policlinico De Marchi (Milano)",              "Milano",
   7L, "Ospedale Civile Maggiore (Verona)",                 "Verona",
   8L, "Azienda Ospedaliera Universitaria (Padova)",        "Padova",
   9L, "IRCCS Burlo Garofalo (Trieste)",                    "Trieste",
  10L, "Ospedale Sant'Orsola Malpighi (Bologna)",           "Bologna",
  11L, "Ospedale Meyer (Firenze)",                          "Firenze",
  12L, "Ospedale Bambino Gesù DEA (Roma)",                  "Roma",
  13L, "Terapia Intensiva Pediatrica (Vicenza)",            "Vicenza",
  14L, "Ospedale Santo Bono (Napoli)",                      "Napoli",
  15L, "Ospedale G Di Cristina (Palermo)",                  "Palermo",
  16L, "Taormina",                                          "Taormina",
  19L, "Ospedale Regina Margherita - TIP cardiochirurgica (Torino)", "Torino",
  20L, "Ospedale Salesi (Ancona)",                          "Ancona",
  21L, "Ospedale Bambino Gesù TIP (Roma)",                  "Roma",
  22L, "IRCCS Gemelli (Roma)",                              "Roma",
  23L, "Patologia Neonatale Buzzi(Milano)",                 "Milano",
  24L, "Ospedale Bambino Gesù Patologia Neonatale (Roma)",  "Roma",
  25L, "Ospedale Bambino Gesù (Palidoro)",                  "Palidoro",
  26L, "Ospedale San Donato TIP cardiochirurgica (Milano)", "Milano",
  27L, "Spedali Civili (Brescia)",                          "Brescia",
  28L, "Ospedale Garibaldi Nomisma (Catania)",              "Catania",
  29L, "Policlinico TIN TIP (Messina)",                     "Messina",
  30L, "Ospedale Monaldi TIP cardiochirurgica (Napoli)",    "Napoli",
  31L, "Ospedale Riuniti (Bergamo)",                        "Bergamo",
  32L, "Ospedale Bambino Gesù TIP cardiochirurgica (Roma)", "Roma",
  33L, "Ospedale Pediatrico Giovanni XXIII (Bari)",         "Bari"
) %>%
  dplyr::mutate_if(is.character, as.factor)

old_ids <- readr::read_csv(
  here::here("../data-raw/old_ids.csv"),
  col_types = "i"
)

usethis::use_data(centers_table, old_ids,
  internal = TRUE,
  overwrite = TRUE
)
