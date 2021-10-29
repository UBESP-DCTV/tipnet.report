#' Sample module
#'
#' General description
#'
#' @param id numeric vectors.
#'
#' @importFrom shiny NS callModule reactive req
#' @importFrom shiny fluidRow selectInput textOutput plotOutput
#' @importFrom shiny renderText renderPlot
#' @importFrom plotly renderPlotly plotlyOutput ggplotly
#' @importFrom ggplot2 ggplot aes geom_boxplot facet_wrap coord_flip
#' @importFrom ggplot2 theme ggtitle xlab ylab element_text
#'
#' @name sampleReport
NULL


#' @describeIn sampleReport user interface
#'
#' User Interface description
#'
#' @export
sampleReportUI <- function(id) {
  ns <- NS(id)

}

#' @describeIn sampleReport server function
#'
#' Server description
#'
#' @export
sampleReport <- function(id) {
  callModule(id = id, function(input, output, session) {

  })
}
