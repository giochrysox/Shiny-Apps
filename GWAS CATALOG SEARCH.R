# Install and load the required packages
if (!require("shiny")) {
  install.packages("shiny")
}
if (!require("shinydashboard")) {
  install.packages("shinydashboard")
}
if (!require("readr")) {
  install.packages("readr")
}
library(shiny)
library(shinydashboard)
library(readr)

# Function to download the GWAS catalog data asynchronously
download_gwas_data <- function() {
  url <- "https://www.ebi.ac.uk/gwas/api/search/downloads/full"
  tryCatch(
    {
      download.file(url, "gwas_catalog.tsv", mode = "wb", quiet = TRUE)
    },
    error = function(e) {
      print(paste("Error downloading GWAS catalog data:", e$message))
    }
  )
}

# Attempt to download the GWAS catalog data
download_gwas_data()

# Check if the file was downloaded successfully
if (!file.exists("gwas_catalog.tsv")) {
  stop("Failed to download GWAS catalog data. Please try again later.")
}

# Read the GWAS catalog data from the downloaded TSV file
gwas_data <- read_tsv("gwas_catalog.tsv")

# Define the UI for the Shiny app
ui <- dashboardPage(
  dashboardHeader(title = "GWAS Catalog Search"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Search", tabName = "search_tab")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "search_tab",
              fluidPage(
                titlePanel("GWAS Catalog Search"),
                sidebarLayout(
                  sidebarPanel(
                    textInput("query", "Enter Disease Trait or Gene of Interest"),
                    radioButtons("search_type", "Search by:", c("Disease Trait" = "disease", "Gene" = "gene")),
                    actionButton("search_button", "Search"),
                    actionButton("clear_button", "Clear")
                  ),
                  mainPanel(
                    tableOutput("results_table")
                  )
                )
              )
      )
    )
  )
)

# Define the server for the Shiny app
server <- function(input, output, session) {
  results_data <- reactiveVal(NULL)
  
  observeEvent(input$search_button, {
    query <- input$query
    search_type <- input$search_type
    
    if (search_type == "disease") {
      filtered_data <- gwas_data[grep(query, gwas_data$`DISEASE/TRAIT`, ignore.case = TRUE), ]
    } else {
      filtered_data <- gwas_data[grep(query, gwas_data$`MAPPED_GENE`), ]
    }
    
    if (nrow(filtered_data) > 0) {
      results_data(filtered_data)
    } else {
      results_data(NULL)
    }
  })
  
  observeEvent(input$clear_button, {
    results_data(NULL)
    updateTextInput(session, "query", value = "")
  })
  
  output$results_table <- renderTable({
    if (!is.null(results_data())) {
      results_data()
    }
  })
}

# Run the Shiny app
shinyApp(ui, server)
