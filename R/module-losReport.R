#' los module
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
#' @name module-losReport
NULL


#' @describeIn module-losReport user interface
#' @export
losReportUI <- function(id) {
  ns <- NS(id)

  fluidPage(
    fluidRow(
      column(5, selectInput(ns("completed"),
                            label   = "Records to use",
                            choices = c("Completed only", "Overall"),
                            selected = "Completed only"
      ))
    ),
    fluidRow(plotlyOutput(ns("dist"), height = "800px")),
    fluidRow(
      column(5, checkboxInput(ns("byAgeclass"),
                              label   = "Per età"
      )),
      column(5, checkboxInput(ns("byGender"),
                              label   = "Per genere"
      ))
    ),
    fluidRow(
      column(12, DT::DTOutput(ns("tbl")))
    )
  )
}


#' @describeIn module-losReport server function
#' @export
losReport <- function(id, data, what) {

  callModule(id = id, function(input, output, session) {

    completed <- reactive({
      req(input$completed)
    })

    data_to_use <- reactive({
      los_dataToUse(data(), completed())
    })

    output$dist <- renderPlotly({
      data_to_use() |>
        los_Plot(
          what = what

        )|>
        plotly::ggplotly(dynamicTicks = TRUE)
    })

    output$tbl <- DT::renderDT(
      data_to_use() |>
        los_dataTbl(
          what = what,
          by_ageclass = input[["byAgeclass"]],
          by_gender = input[["byGender"]]
        ),

      filter = list(position = "top", clear = TRUE),
      server = FALSE
    )

  })
}


#' @describeIn module-losReport static report function
#' @export
losReportStatic <- function(data, completed, what) {
  data_to_use <- quality_dataToUse(data, completed)
  los_Plot(data_to_use, what = what) +
    labs(subtitle = glue::glue("Data used: {completed}."))
}