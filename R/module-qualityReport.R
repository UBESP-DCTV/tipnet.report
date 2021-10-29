#' Quality module
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
#' @name module-qualityReport
NULL

#' @describeIn module-qualityReport user interface
#' @export
qualityReportUI <- function(id) {
  ns <- NS(id)

  fluidPage(
    fluidRow(textOutput(ns("stat_global"))),
    fluidRow(textOutput(ns("out_of_age"))),


    fluidRow(
      column(5, selectInput(ns("completed"),
        label   = "Records to show",
        choices = c("Completed", "Not completed"),
        selected = "Completed"
      )),
      column(5, selectInput(ns("type"),
        label   = "Summary type",
        choices = c("Proportion", "Total"),
        selected = "Proportion"
      ))
    ),


    fluidRow(plotOutput(ns("plot_global"), height = "800px"))
  )
}

#' @describeIn module-qualityReport server function
#' @export
qualityReport <- function(id, data) {

  are_out_age <- quality_areOutAge(data)

  callModule(id = id, function(input, output, session) {

    data_to_use <- reactive({
      quality_dataToUse(data, completed())
    })

    fun_y <- reactive({
      req(input$type)
      quality_summaryFun(input$type)
    })

    completed <- reactive({
      req(input$completed)
    })


    output$stat_global <- renderText(
      quality_statGlobal(data_to_use(), completed())
    )
    output$out_of_age <- renderText(quality_outOfAge(are_out_age))

    output$plot_global <- renderPlot({
      quality_completeDataPlot(data_to_use(), fun_y(), completed())
    })

  })
}





#' @describeIn module-qualityReport static report function
#' @export
qualityReportStatic <- function(data, completed, type) {

  data_to_use <- quality_dataToUse(data, completed)
  fun_y <- quality_summaryFun(type)
  are_out_age <- quality_areOutAge(data)

  quality_statGlobal(data_to_use, completed)
  quality_outOfAge(are_out_age)
  quality_completeDataPlot(data_to_use, fun_y, completed) +
    labs(subtitle = glue::glue("Summary type: {type} (each group)."))
}
