#' lospim module
#'
#' General description
#'
#' @param id name for the specific instance of the module.
#' @param data database to use
#' @param completed (chr) "Completed" or "Not-completed"
#' @param type (chr) "Total" or "Proportion"
#'
#' @importFrom shiny NS callModule reactive req
#' @importFrom shiny fluidPage fluidRow selectInput textOutput plotOutput
#' @importFrom shiny renderText renderPlot column
#' @importFrom plotly renderPlotly plotlyOutput ggplotly
#' @importFrom ggplot2 ggplot aes stat_summary facet_wrap coord_flip
#' @importFrom ggplot2 theme ggtitle xlab ylab element_text
#'
#' @name module-lospimReport
NULL


#' @describeIn module-lospimReport user interface
#' @export
lospimReportUI <- function(id) {
  ns <- NS(id)

  fluidPage(

    fluidRow(plotlyOutput(ns("pimlos"), height = "800px")),

    )

}


#' @describeIn module-lospimReport server function
#' @export
lospimReport <- function(id, data, what) {

  callModule(id = id, function(input, output, session) {

    completed <- reactive({
      req(input$completed)
    })

    data_to_use <- reactive({
      los_dataToUse(data(), completed())
    })


    output$pimlos <- renderPlotly({
      data_to_use() |>
        pimlos_plot() |>
        plotly::ggplotly()
    })



  })
}


#' @describeIn module-lospimReport static report function
#' @export
lospimReportStatic <- function(data, completed, what) {
  data_to_use <- quality_dataToUse(data, completed)
  lospim_Plot(data_to_use, what = what) +
    labs(subtitle = glue::glue("Data used: {completed}."))
}
