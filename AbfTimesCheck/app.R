#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

options(shiny.maxRequestSize = 1*1024^3)

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
          textOutput("names")
        )
    )
)

# Define server logic
server <- function(input, output) {
  output$names <- renderText({
    paste("Uploaded files: ", input$abffiles)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
