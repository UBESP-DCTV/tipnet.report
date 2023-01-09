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
#' @name module-admissionReport1
NULL


#' @describeIn module-admissionReport1 user interface
#' @export
admissionReportUI1 <- function(id) {
  ns <- NS(id)

  fluidPage(
    fluidRow(
      column(5, selectInput(ns("completed"),
                            label   = "Records to use",
                            choices = c("Completed only", "Overall"),
                            selected = "Completed only"
      ))
    ),
    fluidRow(plotOutput(ns("dist"), height = "800px",width="1000px")),
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


#' @describeIn module-admissionReport server function
#' @export
admissionReport1 <- function(id, data) {

  callModule(id = id, function(input, output, session) {

    completed <- reactive({
      req(input$completed)
    })

    data_to_use <- reactive({
      admission_dataToUse(data(), completed())
    })

    output$dist <- renderPlot({
        admission_Plot1((data_to_use())


        )
      #plotly::ggplotly(dynamicTicks = TRUE)

    })

    output$tbl <- DT::renderDT(
        admission_dataTbl1(
        data_to_use(),
          by_ageclass = input[["byAgeclass"]],
          by_gender = input[["byGender"]]
        ),

      filter = list(position = "top", clear = TRUE),
      server = FALSE
    )

  })
}


#' @describeIn module-admissionReport static report function
#' @export
admissionReportStatic1 <- function(data, completed) {
  data_to_use <- quality_dataToUse(data, completed)
  admission_Plot1(data_to_use) +
    labs(subtitle = glue::glue("Data used: {completed}."))
}
