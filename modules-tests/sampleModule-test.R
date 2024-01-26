library(shiny)

devtools::load_all(".")
purrr::iwalk(
  tipnet.report:::generate_main_data(),
  ~assign(.y, .x, pos = 1)
)


# Define UI for application that draws a histogram
ui <- fluidPage(
  sampleReportUI("err_missing")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  sampleReport("err_missing")
}

# Run the application
shinyApp(ui = ui, server = server)
