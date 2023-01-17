#' Admission module
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
#' @name module-admissionReport
NULL


#' @describeIn module-admissionReport user interface
#' @export
admissionReportUI <- function(id) {
  ns <- NS(id)

  fluidPage(
    fluidRow(
      column(5, selectInput(ns("completed"),
                            label   = "Records to use",
                            choices = c("Completed only", "Overall"),
                            selected = "Completed only"
      ))
    ),
    fluidRow(plotlyOutput(ns("dist"), height = "1000px")),
    fluidRow(
      column(5, checkboxInput(ns("byAgeclass"),
                              label   = "Per età"
      )),
      column(5, checkboxInput(ns("byGender"),
                              label   = "Per genere"
      )),
      column(5, checkboxInput(ns("byYear"),
                              label   = "Per anno"
      )),
      column(5, checkboxInput(ns("byType"),
                              label   = "Per tipologia di ricovero"
      ))
    ),
    fluidRow(
      column(12, DT::DTOutput(ns("tbl")))
    )
  )
}


#' @describeIn module-admissionReport server function
#' @export
admissionReport <- function(id, data, type, what = NULL, dict = NULL) {

  callModule(id = id, function(input, output, session) {

    completed <- reactive({
      req(input$completed)
    })

    data_to_use <- reactive({
      admission_dataToUse(data(), completed())
    })

    output$dist <- renderPlotly({
      data_to_use() |>
        admission_Plot(type = type, what = what, dict = dict) |>
        plotly::ggplotly() |>
        layout(boxmode = "group")
    })

    output$tbl <- DT::renderDT(
      data_to_use() |>
        admission_dataTbl(
          type = type,
          what = what,
          dict = dict,
          by_ageclass = input[["byAgeclass"]],
          by_gender = input[["byGender"]],
          by_type = input[["byType"]],
          by_year = input[["byYear"]]
        ),

      filter = list(position = "top", clear = TRUE),
      server = FALSE
    )

  })
}


#' @describeIn module-admissionReport static report function
#' @export
admissionReportStatic <- function(data, completed, what) {
  data_to_use <- quality_dataToUse(data, completed)
  admission_Plot(data_to_use, what = what) +
    labs(subtitle = glue::glue("Data used: {completed}."))
}
