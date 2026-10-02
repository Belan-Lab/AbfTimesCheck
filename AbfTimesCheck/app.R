#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

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
    validate(need(all(ext == "abf"), "Only .abf files are supported. Please upload an .abf file"))
    input$abffiles
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
