library(shiny)
library(shinydashboard)
library(ChemmineR)
library(fmcsR)
library(tidyverse)

ui <- dashboardPage(
  dashboardHeader(title = "STRUCTURAL SIMILARITY"),
  dashboardSidebar(
    textInput("pubchem_cids", "Enter PubChem CIDs (comma-separated):"),
    actionButton("run_analysis", "Run Analysis")
  ),
  dashboardBody(
    textOutput("notification")
  )
)

server <- function(input, output) {
  observeEvent(input$run_analysis, {
    isolate({
      # Extract entered CIDs and convert to numeric vector
      entered_cids <- as.numeric(unlist(strsplit(input$pubchem_cids, ",")))
      
      # Retrieve SDF files from PubChem using the entered CIDs
      compounds <- pubchemCidToSDF(entered_cids)
      write.SDF(compounds, file = "phytochemicals.sdf")
      
      # Import the sdf file
      sdf <- read.SDFset("phytochemicals.sdf")
      
      # Initialize a list to store the comparison results
      cmp_list <- vector("list", length = length(sdf))
      
      # Perform structural comparison in a loop
      for (i in seq_along(sdf)) {
        cmp_list[[i]] <- fmcsBatch(sdf[[i]], sdf)
      }
      
      # Write CSV files with names of compounds
      for (i in seq_along(cmp_list)) {
        cmp_file <- paste0("CMP", i, ".csv")
        write.csv(cmp_list[[i]], file = cmp_file)
      }
      
      # Get the list of CSV files in your directory
      csv_files <- list.files(pattern = ".csv")
      
      # Create a function to read and process each CSV file
      add_title_column <- function(file_path) {
        # Read the CSV file
        data <- read.csv(file_path)
        
        # Extract the title from the file path
        title <- tools::file_path_sans_ext(basename(file_path))
        
        # Add a new column with the title
        data <- mutate(data, Title = title)
        
        return(data)
      }
      
      # Notify the user when the process is complete
      output$notification <- renderText({
        "Analysis completed. Results are ready."
      })
    })
  })
}

shinyApp(ui = ui, server = server)
