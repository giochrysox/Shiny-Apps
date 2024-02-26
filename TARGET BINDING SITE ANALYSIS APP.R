# Install and load required packages if not already installed
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}

if (!requireNamespace("bio3d", quietly = TRUE)) {
  install.packages("bio3d")
}

library(shiny)
library(bio3d)
library(shinydashboard)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Target Binding Site Analysis App"),
  dashboardSidebar(
    textInput("pdb_id", "Enter PDB ID:", ""),
    actionButton("run_analysis", "Run Analysis")
  ),
  dashboardBody(
    textOutput("status"),
    downloadButton("download_pdb", "Download PDB"),
    downloadButton("download_csv", "Download CSV")
  )
)

# Define server
server <- function(input, output) {
  output$status <- renderText({
    if (input$run_analysis > 0) {
      # Clear the Global environment
      rm(list = ls())
      
      # Retrieve online and read pdb file of the androgen receptor (AR) target
      pdb_id <- toupper(input$pdb_id)
      pdb <- read.pdb(pdb_id)
      
      # Check if PDB file exists
      if (is.null(pdb)) {
        return("Invalid PDB ID. Please enter a valid PDB ID.")
      }
      
      # Retrieve the binding sites
      bs <- binding.site(pdb)
      
      # Save PDB file
      write.pdb(pdb, paste0(pdb_id, '.pdb'))
      
      # Save CSV file with binding sites
      write.csv(bs$resnames, paste0(pdb_id, '-binding-sites.csv'))
      
      return("Analysis completed. Files are ready for download.")
    }
  })
  
  output$download_pdb <- downloadHandler(
    filename = function() {
      paste0(input$pdb_id, '.pdb')
    },
    content = function(file) {
      file.copy(paste0(input$pdb_id, '.pdb'), file)
    }
  )
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0(input$pdb_id, '-binding-sites.csv')
    },
    content = function(file) {
      file.copy(paste0(input$pdb_id, '-binding-sites.csv'), file)
    }
  )
}

# Run the Shiny app
shinyApp(ui, server)
