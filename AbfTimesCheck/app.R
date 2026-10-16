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
library(bslib)
library(hms)

options(shiny.maxRequestSize = 10*1024^3)

# Define UI for application
ui <- fluidPage(
  
  titlePanel("Abf Times Checker"),
  
  p("V 0.1"),
  
  fileInput(
    "abfFiles",
    label = "Upload .abf files",
    multiple = TRUE,
    accept = ".abf"
  ),
  
  input_switch("hms", "Show times in hour:min:sec"),
  
  tableOutput("tableTimes"),
  
  downloadButton("downloadCSV", "Download table as .csv file")
  
  
)

# Define server logic
server <- function(input, output) {
  output$tableTimes <- renderTable({
    req(input$abfFiles)
    ext <- tools::file_ext(input$abfFiles$datapath)
    validate(need(all(ext == "abf"), "Only .abf files are supported. Please upload an .abf files"))
    
    input$abfFiles |>
      rowwise() |> # need this because abf_obj will be a list column
      mutate(abf = map(datapath, readABF),
             daytime = abf$header$uFileStartTimeMS / 1000,
             stopwatch = abf$header$uStopwatchTime,
             duration = abf$header$lActualAcqLength * abf$header$fADCSampleInterval / 1000000
             ) |> 
      ungroup() |> # ending rowwise()
      select(name, daytime, stopwatch, duration) |>
      arrange(daytime) |>
      mutate (
        timediff = daytime - first(daytime),
        timegap = lead(daytime) - daytime - duration
      ) |> 
      (\(x){if(input$hms == TRUE) mutate(x, across(!name, ~as.character(hms(.x)))) else x})()
    # hms(50)
    }, digits = 3)
  
  data <- mtcars
  
  output$downloadCSV <- downloadHandler(
    filename = function() {
      paste("ABF-times-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data, file)
    }
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

#add features : 
# - gap between files
# - export to csv
# - copy column values



