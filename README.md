# TMT_Out_File_UploadR
Shiny App to create four dynamic file input widgets for integration in Rmd workflow for processing Tandem Mass Tag experiments in 2D-Thermal Profiling. For a Non-Parametric Analysis two replicates of each condition are required.

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

#libraries needed
library(shiny)

#we need to increase the maximum upload size to 1GB
options(shiny.maxRequestSize=1000*1024^2)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Thermal ProfilR 2D-TPP Dropbox Shiny App"),
  
  # File input widgets
  sidebarLayout(
    sidebarPanel(
      fileInput("vehicleControl1",
                "Upload Vehicle Control 1",
                multiple = TRUE,
                # accept multiple file types
                accept = c(".csv", ".txt", ".xlsx", ".xls")),
      fileInput("vehicleControl2",
                "Upload Vehicle Control 2",
                multiple = TRUE,
                accept = c(".csv", ".txt", ".xlsx", ".xls")),
      fileInput("treatment1",
                "Upload Treatment 1",
                multiple = TRUE,
                accept = c(".csv", ".txt", ".xlsx", ".xls")),
      fileInput("treatment2",
                "Upload Treatment 2",
                multiple = TRUE,
                accept = c(".csv", ".txt", ".xlsx", ".xls"))
    ),
    
    # Main panel to display outputs
    mainPanel(
      # Output for displaying file paths
      uiOutput("savedFilesOutput")
    )
  )
)

# Define our server logic
server <- function(input, output, session) {
  #set a target directory for files
  #ensure these are forward slashes and not backslashes
  #To use - replace Source_Dir with the name of the directory to pull files from
  targetDirectory <- "C:\\Users\\Source_Dir"
  # Create the directory if it doesn't exist we must create one
  if (!dir.exists(targetDirectory)) {
    dir.create(targetDirectory, recursive = TRUE)
  }
  # Create reactive values to store the paths of the saved files
  savedFilePaths <- reactiveValues(
    vehicleControl1 = NULL,
    vehicleControl2 = NULL,
    treatment1 = NULL,
    treatment2 = NULL
  )
  
  # Modified function to save uploaded file with a prefix in the target directory
  saveUploadedFile <- function(uploadedFile, prefix) {
    if (is.null(uploadedFile)) return(NULL)
    # Add prefix to the filename
    filenameWithPrefix <- paste0(prefix, "_", uploadedFile$name)
    targetFilePath <- file.path(targetDirectory, filenameWithPrefix)
    file.copy(uploadedFile$datapath, targetFilePath)
    return(targetFilePath)
  }
  
  # Function to process a dataframe of uploaded files
  saveUploadedFiles <- function(uploadedFiles, prefix) {
    if (is.null(uploadedFiles)) {
      return(NULL)
    }
    
    lapply(seq_len(nrow(uploadedFiles)), function(i) {
      saveUploadedFile(uploadedFiles[i, ], prefix)
    })
  }
  
  # Adjusted observe block
  observe({
    savedFilePaths$vehicleControl1 <- saveUploadedFiles(input$vehicleControl1, "vc1")
    savedFilePaths$vehicleControl2 <- saveUploadedFiles(input$vehicleControl2, "vc2")
    savedFilePaths$treatment1 <- saveUploadedFiles(input$treatment1, "T1")
    savedFilePaths$treatment2 <- saveUploadedFiles(input$treatment2, "T2")
  })
  # Function to check if a file exists
  checkFileExistence <- function(filePaths) {
    sapply(filePaths, function(filePath) {
      if (!is.null(filePath) && file.exists(filePath)) {
        return(filePath)
      } else {
        return("File not found")
      }
    })
  }
  
  # UI output for displaying saved file paths
  # This allows the user to quickly find the files for future specification of file paths
  output$savedFilesOutput <- renderUI({
    fluidRow(
      column(6, wellPanel("Vehicle Control 1 Files:", verbatimTextOutput("vc1Files"))),
      column(6, wellPanel("Vehicle Control 2 Files:", verbatimTextOutput("vc2Files"))),
      column(6, wellPanel("Treatment 1 Files:", verbatimTextOutput("treatment1Files"))),
      column(6, wellPanel("Treatment 2 Files:", verbatimTextOutput("treatment2Files")))
    )
  })
  
  # Render text outputs for each file category
  output$vc1Files <- renderText({ paste(savedFilePaths$vehicleControl1, collapse = "\n") })
  output$vc2Files <- renderText({ paste(savedFilePaths$vehicleControl2, collapse = "\n") })
  output$treatment1Files <- renderText({ paste(savedFilePaths$treatment1, collapse = "\n") })
  output$treatment2Files <- renderText({ paste(savedFilePaths$treatment2, collapse = "\n") })
}
# Run the application 
shinyApp(ui = ui, server = server)
