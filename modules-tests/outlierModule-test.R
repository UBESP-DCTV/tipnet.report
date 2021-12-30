library(shiny)

devtools::load_all(".")
purrr::iwalk(
  tipnet.report:::generate_main_data(),
  ~ assign(.y, .x, pos = 1)
)


# Define UI for application that draws a histogram
ui <- fluidPage(
  outlierReportUI("err_outlier", data = outliers)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  outlierReport("err_outlier", data = outliers)
}

# Run the application
shinyApp(ui = ui, server = server)
