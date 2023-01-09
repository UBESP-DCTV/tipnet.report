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
admission_Plot1 <- function(.db) {

  .db  |>
    select(.data$center,c(.data$mal_cronica0_1:.data$mal_cronica0_13)) |>
    rename('Cardiologica'='mal_cronica0_1', 'Metabolica'='mal_cronica0_2',
           'Neurologica'=	'mal_cronica0_3',
           'Neuromuscolare' =	'mal_cronica0_4',
           'Onco-ematologica' = 'mal_cronica0_5',
           'Renale' =	'mal_cronica0_6',
           'Respiratoria'	= 'mal_cronica0_7',
           'Sindromica' =	'mal_cronica0_8',
           'Altro' =	'mal_cronica0_9',
           'Malformato' =	'mal_cronica0_10',
           'Ex-prematuro'=	'mal_cronica0_11',
           'Gastroenterologica' =	'mal_cronica0_12',
           'Trapiantologica'=	'mal_cronica0_13'
    ) |>
   # dplyr::group_by(.data$center) %>%
    #dplyr::mutate(
     # n = dplyr::n()) %>%
    #dplyr::ungroup() |>
    tidyr::pivot_longer(
      -c(.data$center),
      names_to = "variable"
    ) %>%
    mutate(value1 = (ifelse(.data$value == " ",0,1))) |>
    mutate(value1 = (ifelse(is.na(.data$value1),0,1))) |>
    select(-.data$value) %>%
    group_by(.data$center,.data$variable) %>%
    summarise(prop = sum(.data$value1)) %>%
    ggplot(aes(x = .data$prop, y = .data$center, group = .data$variable, fill = .data$variable))+
    geom_bar(stat = "identity")
    #group_by(.data$center,.data$variable,.data$n) %>%
    #summarise(Categorie = (.data$prop)/.data$n*100) %>%
    #select(-.data$n) %>%
   # ggplot(aes(x = " ", y = .data$prop, fill = .data$variable)) +
    #geom_bar(stat="identity", width=1, color="white") +
    #coord_polar("y", start=0) +
    #facet_wrap(~.data$center,nrow = 4)+
    #theme_void()
    #theme(axis.text.x=element_blank())
  #labs(
  #  fill = stringr::str_to_sentence(reported_name)
  #)


}



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




