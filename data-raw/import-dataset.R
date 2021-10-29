## code to prepare `tipnet_db` dataset goes here
library(assertive) # it should precede tidyverse
library(tidyverse)
library(lubridate)
library(janitor)
library(here)  # it should follow the lubridate package because `here()`
library(glue)
# library(tipnet.report)

## Raw data will stay off-line, commonly onle level above the the
## R-package folder. This assure the package remains lightweight
## and data private even in a public developed project.
raw_path <- here::here("..", "data-raw", "db.zip")


raw_data_list <- unzip(raw_path, list = TRUE) %>%
    filter(str_detect(Name, "csv/.+")) %>%
    `[[`("Name") %>%
    set_names(
        str_remove_all(., "^csv/|\\.csv*") %>%
            str_to_lower()
    ) %>%
    map(~{
        unz(raw_path, .x) %>%
            # read_csv do not correctly parse text fields starting or
            # anding with double quotes (if they are not coupled, e.g.,
            # "foo bar 1"")
            read_delim(
                delim = ",",
                # This is THE problem, e.g. in the file
                # "tip_accettazione.csv" the row 6646 of the column
                # TIPO_SPEC1 finish with two double quotes (becaus is
                # a measure in inch like "foo bar 1"" and the parser
                # in read_csv() go creazy (see
                # https://github.com/tidyverse/readr/issues/932)
                escape_double = FALSE,
                # In read_csv() this is standard and smart. I prefer
                # to maintain this behaveour
                trim_ws = TRUE,
                # guess 1000 are not sufficient for some columns which
                # are almost empty, and the first numeric or character
                # calue are after the 1000th row (parsed as logical
                # otherwise)
                guess_max = 1e5
            )
    })

import_problems <- raw_data_list %>%
    map(attr, "problems") %>%
    keep(~!is.null(.))

# Now there is still only one problem (one column extra in two lines
# of the file no_pre_dimissioe_tip.csv). By a manual ispection I do
# not see any issue, anyway, it don't seams to be a real problem
import_problems


# DB part of the script ------------------------------------------------
# From this part of the script I will perform an essential cleaning
# of the dataframes
tipnet_db <- map(raw_data_list, ~{
    clean_names(.x) %>%
        lowering_character_columns() %>%
        nosi_columns_to_lgl() %>%
        convert_to_integer_numerical_integer_columns()
})

## furter processing of the data........................................
# stop("
#     Furter preprocessing needed?
#     If so did it and then save the file as indicated"
# )


## Data should not stay on-line on GitHub for both privacy and
## space. This assure we can track the import and the general
## manipulation of the raw dataset while maintaining them off-line.

## save single object for quick and lightweight usage, and the whole
## db for cohomprensive computations.
iwalk(tipnet_db, ~write_rds(.x, here("..", "data", glue("{.y}.rds"))))
save(tipnet_db, file = here("..", "data", "tipnet_db.rda"))

## Please, document any specification. report any changes on `R/data.R`
