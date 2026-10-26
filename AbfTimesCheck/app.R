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
  
  p("Version 0.4"),
  
  fluidRow(
    column(
      5,
      fileInput(
        "abfFiles",
        label = "Upload .abf files",
        multiple = TRUE,
        accept = ".abf"
      )
    ),
    column(
      7,
      radioButtons( 
        inputId = "sort_by", 
        label = "Sort files by", 
        choices = list( 
          "Clampfit stopwatch time" = "stopwatch", 
          "System time" = "daytime"
        )
      ),
      input_switch("show_hms", "Show times in hour:min:sec"),
    )
  ),
  
  tableOutput("table"),
  
  downloadButton("downloadCSV", "Download table as .csv file")
  
  
)

# Define server logic
server <- function(input, output) {
  
  times_table <- reactive({
    req(input$abfFiles)
    ext <- tools::file_ext(input$abfFiles$datapath)
    validate(need(
      all(ext == "abf"),
      "Only .abf files are supported. Please upload an .abf files"
    ))
    
    input$abfFiles |>
      rowwise() |> # need this because abf_obj will be a list column
      mutate(abf = map(datapath, readABF),
             sweeps = length(abf$data),
             duration = abf$header$lActualAcqLength * abf$header$fADCSampleInterval / 1000000,
             daytime = abf$header$uFileStartTimeMS / 1000,
             stopwatch = abf$header$uStopwatchTime
      ) |> 
      ungroup() |> # ending rowwise()
      select(name, sweeps, duration, daytime, stopwatch) |>
      arrange(.data[[input$sort_by]]) |>
      mutate(
        timediff = .data[[input$sort_by]] - first(.data[[input$sort_by]]),
        timegap = lead(.data[[input$sort_by]]) - .data[[input$sort_by]] - duration
      ) |> 
      (\(x){if (input$show_hms == TRUE) mutate(x, across(!name, ~as.character(hms(.x)))) else x})()
    
  })
  
  output$table <- renderTable(times_table(), digits = 3)
  
  output$downloadCSV <- downloadHandler(
    filename = function() {
      paste("ABF-times-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(times_table(), file)
    }
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

#add features : 
# - gap between files
# - export to csv
# - copy column values



