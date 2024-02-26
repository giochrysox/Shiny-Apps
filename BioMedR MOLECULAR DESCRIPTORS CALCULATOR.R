library(shiny)
library(shinydashboard)
library(BioMedR)

ui <- dashboardPage(
  dashboardHeader(title = "BioMedR Molecular Descriptor Calculator"),
  dashboardSidebar(
    fileInput("file", "Upload an SDF file"),
    actionButton("calculate", "Calculate Molecular Descriptors")
  ),
  dashboardBody(
    textOutput("status")
  )
)

server <- function(input, output) {
  data <- reactiveValues(
    file_path = ""
  )
  
  observeEvent(input$file, {
    inFile <- input$file
    if (!is.null(inFile)) {
      file_path <- paste0(tools::file_path_sans_ext(inFile$name), "_molecular_descriptors.csv")
      data$file_path <- file_path
    }
  })
  
  observeEvent(input$calculate, {
    req(input$file)
    inFile <- input$file
    file_path <- data$file_path
    
    mol <- readMolFromSDF(inFile$datapath)
    dat <- extrDrugAIO(mol, warn = FALSE)
    write.csv(dat, file_path)
    output$status <- renderText({
      paste("File", file_path, "is ready for download.")
    })
  })
}

shinyApp(ui, server)
