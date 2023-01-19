#' Descriptives module
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
#' @name module-descriptivesReport
NULL

#' @describeIn module-descriptivesReport user interface
#' @export
descriptivesReportUI <- function(id, what = NULL) {
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

    fluidRow(column(12,
      checkboxGroupInput(ns("strat"),
                  label = "Additional strata for the table.",
                  choices = c("gender", "etnia", "age"),
                  selected = stringr::str_remove(what, "_class$")
      )
    )),
    fluidRow(column(12, DT::DTOutput(ns("tbl"))))
  )
}

#' @describeIn module-descriptivesReport server function
#' @export
descriptivesReport <- function(id, data, what) {

  callModule(id = id, function(input, output, session) {

    completed <- reactive({
      req(input$completed)
    })

    strat <- reactive({
      req(input$strat)
    })

    data_to_use <- reactive({
      descriptives_dataToUse(data(), completed())
    })

    output$dist <- renderPlotly({
      descriptives_Plot(data_to_use(), what = what) |>
                plotly::ggplotly(dynamicTicks = TRUE) |>
        plotly::layout(boxmode = "group")

    })

    output$tbl <- DT::renderDT(
      data_to_use() |>
        descriptives_dataTbl(strat = strat()),

      filter = list(position = "top", clear = TRUE),
      server = FALSE
    )

  })
}





#' @describeIn module-descriptivesReport static report function
#' @export
descriptivesReportStatic <- function(data, completed, what) {

  data_to_use <- quality_dataToUse(data, completed)
  descriptives_Plot(data_to_use, what = what) +
    labs(subtitle = glue::glue("Data used: {completed}."))

}
