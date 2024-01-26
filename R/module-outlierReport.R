#' Outlier module
#'
#' General description
#'
#' @param id numeric vectors.
#' @param data data frame
#' @param center (chr) name(s) of the center(s) to consider
#' @importFrom shiny NS callModule reactive req
#' @importFrom shiny fluidRow selectInput textOutput plotOutput
#' @importFrom shiny renderText renderPlot
#' @importFrom plotly renderPlotly plotlyOutput ggplotly
#' @importFrom ggplot2 ggplot aes geom_boxplot facet_wrap coord_flip
#' @importFrom ggplot2 theme ggtitle xlab ylab element_text
#'
#' @name module-outlierReport
NULL


#' @describeIn module-outlierReport user interface
#' @export
outlierReportUI <- function(id, data) {
  ns <- NS(id)

  fluidPage(
    fluidRow(
      column(12, plotOutput(ns("out_plot")))
    ),
    fluidRow(
      column(12, selectInput(ns("center"),
         label   = "Choose a center to display its data outliers",
         choices = unique(data[["center"]]),
         multiple = TRUE
      ))
    ),
    fluidRow(
      column(12, DT::DTOutput(ns("out_table")))
    )
  )
}

#' @describeIn module-outlierReport server function
#' @export
outlierReport <- function(id, data) {
  callModule(id = id, function(input, output, session) {

    center <- reactive({
      req(input$center)
    })

    db_to_use <- reactive({
      outlier_dataToUse(data(), center())
    })

    output$out_plot <- renderPlot(
      outlier_dataPlot(data())
    )

    output$out_table <- DT::renderDT(db_to_use(),
      filter = list(position = "top", clear = TRUE),
      server = FALSE
    )
  })
}

#' @describeIn module-outlierReport static report function
#' @export
outlierReportStatic <- function(data, center) {
  data_to_use <- outlier_dataToUse(data, center)

  print(outlier_dataPlot(data))

  DT::datatable(data_to_use,
    filter = list(position = "top", clear = TRUE)
  )
}
