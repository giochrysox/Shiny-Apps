library(shiny)
library(httr)
library(dplyr)
library(shinydashboard)

# Function to retrieve drug-drug interactions from KEGG API
getDrugInteractions <- function(keggID) {
  # Construct the API URL
  api_url <- paste("https://rest.kegg.jp/ddi", keggID, sep = "/")
  
  # Make the HTTP request
  response <- httr::GET(api_url)
  
  # Check if the request was successful (status code 200)
  if (httr::status_code(response) == 200) {
    # Parse the response content (drug interactions)
    interactions <- httr::content(response, "text")
    return(interactions)
  } else {
    # Print an error message if the request was not successful
    return(paste("Error:", httr::status_code(response)))
  }
}

# Function to format interactions into a data frame
formatInteractions <- function(interactions) {
  # Split interactions and arrange into a data frame with three columns
  interaction_list <- strsplit(interactions, "\n")[[1]]
  interaction_df <- data.frame(do.call(rbind, strsplit(interaction_list, "\t")), stringsAsFactors = FALSE)
  colnames(interaction_df) <- c("Input Drug", "Other Drug", "Type of DDI", "Info")
  interaction_df <- interaction_df[, -4]  # Remove the "Info" column
  
  return(interaction_df)
}

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Drug-Drug Interactions Explorer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Interactions", tabName = "interactions", icon = icon("list"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "interactions",
        fluidPage(
          titlePanel("Drug-Drug Interactions Explorer"),
          sidebarLayout(
            sidebarPanel(
              textInput("keggID", "Enter KEGG ID", value = "D00564"),
              actionButton("submitBtn", "Submit"),
              downloadButton("downloadCSV", "Download CSV")
            ),
            mainPanel(
              tableOutput("interactionsTable")
            )
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  interactions <- reactiveVal(NULL)
  
  observeEvent(input$submitBtn, {
    keggID <- isolate(input$keggID)
    interactions_data <- getDrugInteractions(keggID)
    interactions(formatInteractions(interactions_data))
  })
  
  output$interactionsTable <- renderTable({
    interactions()
  })
  
  output$downloadCSV <- downloadHandler(
    filename = function() {
      paste(isolate(input$keggID), "DDIs.csv", sep = "_")
    },
    content = function(file) {
      write.csv(interactions(), file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
