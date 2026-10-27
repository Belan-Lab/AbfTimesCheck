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
library(rclipboard)
library(hms)

options(shiny.maxRequestSize = 10*1024^3)

# Define UI for application
ui <- fluidPage(
  
  titlePanel("Abf Times Checker"),
  
  p("Version 0.4"),
  
  fluidRow(
    column(
      4,
      fileInput(
        "abfFiles",
        label = "Upload .abf files",
        multiple = TRUE,
        accept = ".abf"
      ),
      p("All time values are shown in seconds.")
    ),
    column(
      8,
      radioButtons( 
        inputId = "sort_by", 
        label = "Order files by", 
        choices = list(
          "System time" = "daytime",
          "Clampfit stopwatch time" = "stopwatch"
        )
      ),
      textOutput("hms_text"),
      input_switch("show_hms", "Show times in hour:min:sec")
    )
  ),
  
  tableOutput("table"),
  
  downloadButton("downloadCSV", "Download table as .csv file"),
  
  rclipboardSetup(),
  
  uiOutput("copy_delta")
  
)


# Define server logic
server <- function(input, output) {
  
  hms_text <- renderText({
    if (input$show_hms) {
      "All time values are in hour:min:sec format"
    } else {
      "All time values are displayed in seconds"
    }
  })
  
  input_table <- reactive({
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
      select(name, sweeps, duration, daytime, stopwatch)
  })
  
  difftimes_table <- reactive({
    req(input_table)
    input_table()|>
      arrange(.data[[input$sort_by]]) |>
      mutate(
        timediff = .data[[input$sort_by]] - first(.data[[input$sort_by]]),
        timegap = .data[[input$sort_by]] - lag(.data[[input$sort_by]]) - lag(duration)
      ) 
  })
  
  final_table <- reactive({
    req(difftimes_table)
    {if (input$show_hms == TRUE) {
      difftimes_table()|>
        mutate(across(!c(name, sweeps), ~as.character(hms(.x))))
    } else difftimes_table()} |>
      rename(
        `File name` = name,
        `Sweeps count` = sweeps,
        `Total duration` = duration,
        `System time at the file start` = daytime,
        `Clampfit time at the file start` = stopwatch,
        `Δs (If first file starts on zero, this starts on:)` = timediff,
        `Gap after the previous file ends` = timegap
        )
  })
  
  output$table <- renderTable(final_table(), digits = 3)
  
  output$downloadCSV <- downloadHandler(
    filename = function() {
      paste("ABF-times-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(final_table(), file)
    }
  )
  
  clipboardtext <- reactive({
    difftimes_table() |> pull(timediff) |> paste(sep = ", ")
  })
  
  output$copy_delta <- renderUI({
    rclipButton(
      inputId = "clipbutton",
      label = "Copy Δs",
      clipText = {difftimes_table() |> pull(timediff) |> paste(collapse = ", ")},
      icon = icon("clipboard"),
      tooltip = "Δs values (in s) separated by comma",
      placement = "top",
      options = list(delay = list(show = 800, hide = 100), trigger = "hover")
    )
  })

  
}

# Run the application 
shinyApp(ui = ui, server = server)

