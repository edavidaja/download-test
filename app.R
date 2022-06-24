library(shiny)

# Define UI for data download app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Downloading Data"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Choose dataset ----
      numericInput("reps", "reps:", value = 500, min = 50),

      # Button
      downloadButton("downloadData", "Download")

    ),
    mainPanel()
  )
)

# Define server logic to display and download selected file ----
server <- function(input, output) {

  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    data.table::rbindlist(replicate(input$reps, mtcars, simplify = FALSE))
  })


  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("mtcars", input$reps, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )

}

# Create Shiny app ----
shinyApp(ui, server)
