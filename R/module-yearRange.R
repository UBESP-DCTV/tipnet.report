yearRangeUI <- function(id, last_data_date) {
  ns <- NS(id)

  fluidPage(
    fluidRow(
      sliderInput(
        ns("years"),
        "Report's years range (new data collection started on 2010):",
        min = lubridate::ymd("1992-01-01"),
        max = last_data_date,
        value = c(lubridate::ymd("2010-01-01"), last_data_date),
        step = lubridate::years(1L),
        timeFormat = "%Y"
      )
    ),
    fluidRow(textOutput(ns("record_selected")))
  )

}









yearRange <- function(id) {


  callModule(id = id, function(input, output, session) {

    full_filtered <- reactive({
      tip_years <- lubridate::interval(
        input[["years"]][[1L]],
        input[["years"]][[2L]]
      )
      selected_years <- full_records[["ingresso_dt"]] %within% tip_years

      full_records |>
        dplyr::filter(selected_years)
    })

    outliers_filtered <-reactive({
      extract_outliers(full_filtered())
    })

    output$record_selected <- renderText({
      glue::glue(
        "Record selected: {nrow(full_filtered())} (between {input[['years']]})."
      )
    })

    return(list(
      full_filtered = full_filtered,
      outliers_filtered = outliers_filtered
    ))
  })


}
