library(shiny)

devtools::load_all(".")
purrr::iwalk(
  tipnet.report:::generate_main_data(),
  ~assign(.y, .x, pos = 1)
)


# Define UI for application that draws a histogram
ui <- fluidPage(
  originReportUI("foo")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  originReport("foo", data = data_ranged[["full_filtered"]],
               what = "ricovero_prog")
}

# Run the application
shinyApp(ui = ui, server = server)
