#' module for missing-data section
#'
#' General description
#'
#' @param id numeric vectors.
#' @param data data frame
#' @param center (chr) name(s) of the center(s) to consider
#'
#' @importFrom shiny NS callModule reactive req
#' @importFrom shiny fluidRow selectInput textOutput plotOutput
#' @importFrom shiny renderText renderPlot
#' @importFrom plotly renderPlotly plotlyOutput ggplotly
#' @importFrom ggplot2 ggplot aes geom_boxplot facet_wrap coord_flip
#' @importFrom ggplot2 theme ggtitle xlab ylab element_text
#' @importFrom ggplot2 theme_minimal
#'
#' @name module-missReport
NULL


#' @describeIn module-missReport user interface
#' @export
missReportUI <- function(id, data) {
  ns <- NS(id)

  fluidPage(
    fluidRow(
      column(12, plotOutput(ns("out_plot")))
    ),
    fluidRow(
      column(12, selectInput(ns("center"),
         label   = "Choose a center to display its missing data",
         choices = unique(data[["center"]]),
         multiple = TRUE
      ))
    ),
    fluidRow(
      column(12, DT::DTOutput(ns("out_table")))
    )
  )
}


#' @describeIn module-missReport server function
#' @export
missReport <- function(id, data) {
  callModule(id = id, function(input, output, session) {

    data_to_use <- reactive({
      miss_dataToUse(data)
    })

    output$out_plot <- renderPlot(
      miss_dataPlot(data_to_use())
    )

    center <- reactive({
      req(input$center)
    })

    output$out_table <- DT::renderDT(
      miss_dataTbl(data_to_use(), center()),
      filter = list(position = "top", clear = TRUE)
    )
  })
}


#' @describeIn module-missReport static report function
#' @export
missReportStatic <- function(data, center) {
  data_to_use <- miss_dataToUse(data)

  print(miss_dataPlot(data_to_use))

  DT::datatable(miss_dataTbl(data_to_use, center),
    filter = list(position = "top", clear = TRUE)
  )
}
