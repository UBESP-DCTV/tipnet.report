library(shiny)

devtools::load_all(".")
purrr::iwalk(
  generate_main_data(),
  ~ assign(.y, .x, pos = 1)
)


# Define UI for application that draws a histogram
ui <- fluidPage(
  descriptivesReportUI("foo")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  descriptivesReport("foo", data = full_records)
}

# Run the application
shinyApp(ui = ui, server = server)
