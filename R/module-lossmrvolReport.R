#' lossmrvol module
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
#' @name module-lossmrvolReport
NULL


#' @describeIn module-lossmrvolReport user interface
#' @export
lossmrvolReportUI <- function(id) {
  ns <- NS(id)

  fluidPage(
    fluidRow(
      column(5, selectInput(ns("completed"),
                            label   = "Records to use",
                            choices = c("Completed only", "Overall"),
                            selected = "Completed only"
      ))
    ),
    fluidRow(column(12, plotlyOutput(ns("smrlosvol"), height = "800px"))

  ))

}


#' @describeIn module-lossmrvolReport server function
#' @export
lossmrvolReport <- function(id, data) {

  callModule(id = id, function(input, output, session) {

    completed <- reactive({
      req(input$completed)
    })

    data_to_use <- reactive({
      los_dataToUse(data(), completed())
    })


    output$smrlosvol <- renderPlotly({
      data_to_use() |>
        smrlosvol_plot() |>
        plotly::ggplotly(dynamicTicks = TRUE)
    })

  })
}
