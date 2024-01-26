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

  all_sheets <- c(
    "anagrafica", "accettazione", "degenza", "dimissione",
    "infezione", "ingresso", "punteggio_di_aristotle",
    "pelod_scheda_facoltativa", "pim", "procedure_di_ventilazione"
  ) %>%
    purrr::set_names(str_to_sentence(str_replace_all(., "_", " ")))

  fluidPage(
    fluidRow(
      column(12, textOutput(ns("txt")))
    ),
    fluidRow(
      column(12, checkboxGroupInput(
        ns("active_sheets"),
        label = "Select REDCap sheets to consider",
        choices = all_sheets,
        selected = all_sheets,
        inline = TRUE
      ))
    ),
    fluidRow(
      column(12, plotOutput(ns("miss_plot")))
    ),
    fluidRow(
      column(12, selectInput(ns("center"),
         label   = "Choose a center to display its missing data",
         choices = unique(data[["center"]]),
         multiple = TRUE
      ))
    ),
    fluidRow(
      column(12, DT::DTOutput(ns("miss_table")))
    )
  )
}


#' @describeIn module-missReport server function
#' @export
missReport <- function(id, data) {
  callModule(id = id, function(input, output, session) {

    n_missing <- reactive({
      map_int(data(), ~sum(is.na(.)))
    })
    are_w_missing <- reactive({
      n_missing() > 0
    })

    data_to_use <- reactive({
      miss_dataToUse(data(), input$active_sheets)
    })

    output$miss_plot <- renderPlot(
      miss_dataPlot(data_to_use())
    )

    center <- reactive({
      req(input$center)
    })

    output$miss_table <- DT::renderDT(
      miss_dataTbl(data_to_use(), center()),
      filter = list(position = "top", clear = TRUE),
      server=FALSE
    )

    output$txt <- renderText(glue::glue(
        "Overall, there are {sum(are_w_missing())} variables with some missing data (out of {length(data())}) for a total number of {sum(n_missing())} missing entries in the whole dataset ({round(100 * mean(is.na(data())), 2)}% of missingness)."
    ))
  })
}


#' @describeIn module-missReport static report function
#' @export
missReportStatic <- function(data, center) {
  n_missing <- map_int(data, ~sum(is.na(.)))
  are_w_missing <- n_missing > 0
  data_to_use <- miss_dataToUse(data)

  glue::glue(
    "Overall, there are {sum(are_w_missing)} variables with some missing data (out of {length(data)}) for a total number of {sum(n_missing)} missing entries in the whole dataset ({round(100 * mean(is.na(data)), 2)}% of missingness)."
  )

  print(miss_dataPlot(data_to_use))

  DT::datatable(miss_dataTbl(data_to_use, center),
    filter = list(position = "top", clear = TRUE)
  )
}
