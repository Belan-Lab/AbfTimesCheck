#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(tidyverse)
library(readABF)
library(shiny)

options(shiny.maxRequestSize = 10*1024^3)

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("AbfTimesCheck"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
          fileInput(
            "abffiles", 
            label = "Upload .abf files",
            multiple = TRUE,
            accept = ".abf"
            )
        ),

        # MainPanel
        mainPanel(
          tableOutput("files")
        )
    )
)

# Define server logic
server <- function(input, output) {
  output$files <- renderTable({
    req(input$abffiles)
    ext <- tools::file_ext(input$abffiles$datapath)
    validate(need(all(ext == "abf"), "Only .abf files are supported. Please upload an .abf files"))
    
    input$abffiles |>
      rowwise() |> # need this because abf_obj will be a list column
      mutate(abf = map(datapath, readABF),
             daytime = abf$header$uFileStartTimeMS / 1000,
             stopwatch = abf$header$uStopwatchTime
             ) |> 
      ungroup() |> # ending rowwise()
      select(name, daytime, stopwatch) |>
      arrange(stopwatch) |>
      mutate (
        timediff = daytime - first(daytime)
      )
    
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
