#' picu admissions module
#'
#' @param .db (data frame) data
#'   "gender", or "etnicity"
#'
#' @name funs-admissionReport1
NULL


#'
#' @describeIn funs-admissionReport1 plot
#' @export
#' @examples
#'
#' admission_Plot1(full_records, "mal_cronica") # same as before
#' admission_Plot1(full_records, "etnia")

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
admission_Plot1 <- function(.db, dict) {

  .db |>
    select(.data$center, dplyr::all_of(names(dict))) |>
    rename_with(~dict[.x], .cols = dplyr::all_of(names(dict))) |>
    pivot_longer(-"center", values_drop_na = TRUE) |>
    ggplot(aes(y = center, fill = name)) +
    geom_bar(position = "fill")

  # stop("update dictionaries as interenal data defined and stored in data-raw/centers_table.R")
}

# admission_Plot1(full_records, comorb)


#' @describeIn funs-admissionReport data
#' @export
#' @examples
#'
#' admission_dataToUse(full_records, "Completed only")
#' admission_dataToUse(full_records, "overall")
admission_dataToUse <- function(
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

admission_dataTbl1 <- function(
    .db,
    by_gender = FALSE,
    by_ageclass = FALSE

) {
  if (by_ageclass & !by_gender) {


.db  |>
      select(.data$center,.data$gender,.data$age_class,c(.data$mal_cronica0_1:.data$mal_cronica0_13)) |>
      dplyr::ungroup() |>
      tidyr::pivot_longer(
        -c(.data$center,.data$gender,.data$age_class),
        names_to = "variable"
      ) %>%
      mutate(value1 = (ifelse(.data$value == " ",0,1))) |>
      mutate(value1 = (ifelse(is.na(.data$value1),0,1))) |>
      select(-.data$value) %>%
      group_by(.data$center,.data$variable,.data$age_class, .add = TRUE) %>%
      summarise(Sum = sum(.data$value1))


  }
 else if (by_gender & !by_ageclass) {


    .db  |>
      select(.data$center,.data$gender,c(.data$mal_cronica0_1:.data$mal_cronica0_13)) |>
      dplyr::ungroup() |>
      tidyr::pivot_longer(
        -c(.data$center,.data$gender),
        names_to = "variable"
      ) %>%
      mutate(value1 = (ifelse(.data$value == " ",0,1))) |>
      mutate(value1 = (ifelse(is.na(.data$value1),0,1))) |>
      select(-.data$value) %>%
      group_by(.data$center,.data$variable,.data$gender, .add = TRUE) %>%
      summarise(Sum = sum(.data$value1))

 }
else if (by_gender && by_ageclass) {


 .db <-   .db  |>
      select(.data$center,.data$gender,.data$age_class,c(.data$mal_cronica0_1:.data$mal_cronica0_13)) |>
      dplyr::ungroup() |>
      tidyr::pivot_longer(
        -c(.data$center,.data$gender,.data$age_class),
        names_to = "variable"
      ) %>%
      mutate(value1 = (ifelse(.data$value == " ",0,1))) |>
      mutate(value1 = (ifelse(is.na(.data$value1),0,1))) |>
      select(-.data$value) %>%
      group_by(.data$center,.data$variable,.data$gender,.data$age_class, .add = TRUE) %>%
      summarise(Sum = sum(.data$value1))

  }
else {
    .db  |>
      select(.data$center,c(.data$mal_cronica0_1:.data$mal_cronica0_13)) |>
      dplyr::ungroup() |>
      tidyr::pivot_longer(
        -c(.data$center),
        names_to = "variable"
      ) %>%
      mutate(value1 = (ifelse(.data$value == " ",0,1))) |>
      mutate(value1 = (ifelse(is.na(.data$value1),0,1))) |>
      select(-.data$value) %>%
      group_by(.data$center,.data$variable) %>%
      summarise(Sum = sum(.data$value1))

  }


}




