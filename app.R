library(shiny)

# Define UI for data download app ----
ui <- fluidPage(
  # App title ----
  titlePanel("up and down"),

  # Columnar layout with upload and download operations ----
  fluidRow(
    # Left column: Upload functionality
    column(6,
      div(
        style = "height: 350px;", # Fixed height
        wellPanel(
          style = "height: 100%;", # Full height of parent
          h3("Upload"),
          # Input: File upload ----
          fileInput("fileUpload", "Upload Any File:"),
          
          # Output: Information about the uploaded file
          h4("Uploaded File Information"),
          tableOutput("fileInfo")
        )
      )
    ),
    
    # Right column: Download functionality
    column(6,
      div(
        style = "height: 350px;", # Same fixed height
        wellPanel(
          style = "height: 100%;", # Full height of parent
          h3("Download"),
          # Input: Choose target file size ----
          numericInput(
            "targetSize",
            "Target file size:",
            value = 1,
            min = 0.1,
            step = 0.1
          ),
          
          # Select size unit
          selectInput(
            "sizeUnit",
            "Size unit:",
            choices = c("KB" = "KB", "MB" = "MB", "GB" = "GB"),
            selected = "MB"
          ),
          
          # Button
          br(),
          downloadButton("downloadData", "Download"),
          br(), br(), # Added extra space to make layout more balanced
          div(style = "visibility: hidden;", "Placeholder") # Invisible placeholder for balance
        )
      )
    )
  )
)

# Define server logic to display and download selected file ----
server <- function(input, output) {
  # Reactive for computing required reps based on target size
  computedReps <- reactive({
    # Based on README.md: 500 reps ~ 604KB
    # This means 1 rep is about 1.208KB

    # Convert input to KB for calculation
    targetKB <- input$targetSize
    if (input$sizeUnit == "MB") {
      targetKB <- input$targetSize * 1024
    } else if (input$sizeUnit == "GB") {
      targetKB <- input$targetSize * 1024 * 1024
    }

    # Calculate reps (1 rep ≈ 1.208KB)
    repsNeeded <- round(targetKB / 1.208)

    # Ensure minimum reps is 50
    return(max(50, repsNeeded))
  })

  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    data.table::rbindlist(replicate(computedReps(), mtcars, simplify = FALSE))
  })

  # Display information about the uploaded file
  output$fileInfo <- renderTable({
    req(input$fileUpload)

    # Create a data frame with file information
    data.frame(
      Name = input$fileUpload$name,
      Size = paste0(round(input$fileUpload$size / 1024, 2), " KB"),
      Type = input$fileUpload$type
    )
  })

  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("mtcars-", input$targetSize, input$sizeUnit, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
}

# Create Shiny app ----
shinyApp(ui, server)
