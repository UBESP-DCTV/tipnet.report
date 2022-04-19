#' Origin module
#'
#' General description
#'
#' @param id name for the specific instance of the module.
#' @param data database to use
#'
#' @importFrom shiny NS callModule reactive req
#' @importFrom shiny fluidPage fluidRow selectInput textOutput plotOutput
#' @importFrom shiny renderText renderPlot column
#' @importFrom plotly renderPlotly plotlyOutput ggplotly
#' @importFrom ggplot2 ggplot aes stat_summary facet_wrap coord_flip
#' @importFrom ggplot2 theme ggtitle xlab ylab element_text
#'
#' @name module-originReport
NULL

#' @describeIn module-originReport user interface
#' @export
OriginReportUI_origin <- function(id) {
  ns <- NS(id)

  fluidPage(
    fluidRow(
      column(5, selectInput(ns("completed"),
                            label   = "Records to use",
                            choices = c("Completed only", "Overall"),
                            selected = "Completed only"
      ))
    ),
    fluidRow(plotOutput(ns("dist"), height = "800px"))
  )
}

#' @describeIn module-descriptivesReport server function
#' @export
OriginReport_origin <- function(id, data) {

  callModule(id = id, function(input, output, session) {

    completed <- reactive({
      req(input$completed)
    })

    data_to_use <- reactive({
      descriptives_dataToUse(data(), completed())
    })



    output$dist <- renderPlot({
      descriptives_originPlot(data_to_use())
    })

  })
}





#' @describeIn module-descriptivesReport static report function
#' @export
OriginReportStatic_origin <- function(data, completed) {

  data_to_use <- quality_dataToUse(data(), completed())
  descriptives_originPlot(data_to_use()) +
    labs(subtitle = glue::glue("Data used: {completed}."))

}

#' @describeIn module-originReport user interface
#' @export
OriginReportUI_tipologia <- function(id) {
  ns <- NS(id)

  fluidPage(
    fluidRow(
      column(5, selectInput(ns("completed"),
                            label   = "Records to use",
                            choices = c("Completed only", "Overall"),
                            selected = "Completed only"
      ))
    ),
    fluidRow(plotOutput(ns("dist"), height = "800px"))
  )
}

#' @describeIn module-descriptivesReport server function
#' @export
OriginReport_tipologia <- function(id, data) {

  callModule(id = id, function(input, output, session) {

    completed <- reactive({
      req(input$completed)
    })

    data_to_use <- reactive({
      descriptives_dataToUse(data(), completed())
    })



    output$dist <- renderPlot({
      descriptives_tipologiaPlot(data_to_use())
    })

  })
}





#' @describeIn module-descriptivesReport static report function
#' @export
OriginReportStatic_tipologia <- function(data, completed) {

  data_to_use <- quality_dataToUse(data(), completed())
  descriptives_tipologiaPlot(data_to_use()) +
    labs(subtitle = glue::glue("Data used: {completed}."))

}
#' @describeIn module-originReport user interface
#' @export

OriginReportUI_altroosp <- function(id) {
  ns <- NS(id)

  fluidPage(
    fluidRow(
      column(5, selectInput(ns("completed"),
                            label   = "Records to use",
                            choices = c("Completed only", "Overall"),
                            selected = "Completed only"
      ))
    ),
    fluidRow(plotOutput(ns("dist"), height = "800px"))
  )
}

#' @describeIn module-descriptivesReport server function
#' @export
OriginReport_altroosp <- function(id, data) {

  callModule(id = id, function(input, output, session) {

    completed <- reactive({
      req(input$completed)
    })

    data_to_use <- reactive({
      descriptives_dataToUse(data(), completed())
    })



    output$dist <- renderPlot({
      descriptives_altroospPlot(data_to_use())
    })

  })
}





#' @describeIn module-descriptivesReport static report function
#' @export
OriginReportStatic_altroosp <- function(data, completed) {

  data_to_use <- quality_dataToUse(data(), completed())
  descriptives_altroospPlot(data_to_use()) +
    labs(subtitle = glue::glue("Data used: {completed}."))

}

#' @describeIn module-originReport user interface
#' @export

OriginReportUI_tipochir <- function(id) {
  ns <- NS(id)

  fluidPage(
    fluidRow(
      column(5, selectInput(ns("completed"),
                            label   = "Records to use",
                            choices = c("Completed only", "Overall"),
                            selected = "Completed only"
      ))
    ),
    fluidRow(plotOutput(ns("dist"), height = "800px"))
  )
}

#' @describeIn module-descriptivesReport server function
#' @export
OriginReport_tipochir <- function(id, data) {

  callModule(id = id, function(input, output, session) {

    completed <- reactive({
      req(input$completed)
    })

    data_to_use <- reactive({
      descriptives_dataToUse(data(), completed())
    })



    output$dist <- renderPlot({
      descriptives_tipochirPlot(data_to_use())
    })

  })
}





#' @describeIn module-descriptivesReport static report function
#' @export
OriginReportStatic_tipochir <- function(data, completed) {

  data_to_use <- quality_dataToUse(data(), completed())
  descriptives_tipochirPlot(data_to_use()) +
    labs(subtitle = glue::glue("Data used: {completed}."))

}
