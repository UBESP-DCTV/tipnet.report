#' Title
#'
#' @param id a
#' @param label a
#'
#' @importFrom shiny NS fluidRow textInput textOutput
#' @return a
#' @export
ymdInputUI <- function(id, label) {
  ns <- NS(id)
  label <- paste0(label, " (yyyy-mm-dd)")

  fluidRow(
    textInput(ns("date"), label),
    textOutput(ns("error"))
  )
}

#' Title a
#'
#' @param id  a
#'
#' @importFrom shiny reactive req renderText callModule
#'
#' @return a
#' @export
#'
ymdInput <- function(id) {
  callModule(id = id, function(input, output, session) {
    date <- reactive({
      req(input$date)
      lubridate::ymd(input$date, quiet = TRUE)
    })

    output$error <- renderText({
      if (is.na(date())) {
        "Please enter valid date in yyyy-mm-dd form"
      }
    })

    date
  })
}
