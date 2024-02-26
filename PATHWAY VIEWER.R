library(shiny)
library(shinydashboard)
library(KEGGREST)
library(httr)
library(jsonlite)
library(rWikiPathways)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Pathway Viewer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("KEGG Pathway", tabName = "kegg"),
      menuItem("Reactome Pathway", tabName = "reactome"),
      menuItem("WikiPathways", tabName = "wikipathways")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "kegg",
              textInput("kegg_id", "Enter KEGG Pathway ID", ""),
              actionButton("view_kegg", "View KEGG Pathway")
      ),
      tabItem(tabName = "reactome",
              textInput("reactome_id", "Enter Reactome Pathway ID", ""),
              actionButton("view_reactome", "View Reactome Pathway")
      ),
      tabItem(tabName = "wikipathways",
              textInput("wiki_query", "Enter WikiPathways Search Query", ""),
              actionButton("search_wiki", "Search WikiPathways")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Function to fetch Reactome pathway diagram by ID
  getReactomePathwayDiagram <- function(pathway_id) {
    base_url <- "https://reactome.org/ContentService/data/pathways/low/diagram/entity/"
    species <- "9606"  # Assuming Homo sapiens (you can modify this)
    
    url <- paste0(base_url, pathway_id, "?species=", species)
    
    response <- GET(url)
    
    if (http_error(response)) {
      cat("Error:", http_status(response)$reason, "\n")
      return(NULL)
    }
    
    pathway_data <- content(response, "parsed")
    
    # Extract the diagram URL from the response
    if (length(pathway_data) > 0 && !is.null(pathway_data[[1]]$stId)) {
      diagram_url <- paste0("https://reactome.org/PathwayBrowser/#/", pathway_data[[1]]$stId)
      return(diagram_url)
    } else {
      cat("Pathway with ID", pathway_id, "not found or does not have a diagram available.\n")
      return(NULL)
    }
  }
  
  observeEvent(input$view_kegg, {
    kegg_id <- input$kegg_id
    if (kegg_id != "") {
      png <- keggGet(paste0("path:", kegg_id), "image")
      writePNG(png, 'Cytokine-Cytokine.png')
      browseURL(paste0("file://", file.path(getwd(), "Cytokine-Cytokine.png")))
    }
  })
  
  observeEvent(input$view_reactome, {
    reactome_id <- input$reactome_id
    if (reactome_id != "") {
      diagram_url <- getReactomePathwayDiagram(reactome_id)
      if (!is.null(diagram_url)) {
        browseURL(diagram_url)
      }
    }
  })
  
  observeEvent(input$search_wiki, {
    wiki_query <- input$wiki_query
    if (wiki_query != "") {
      results <- findPathwaysByText(query = wiki_query)
      if (length(results$url) > 0) {
        for (url in results$url) {
          browseURL(url)
        }
      } else {
        cat("No pathways found for query:", wiki_query, "\n")
      }
    }
  })
}

# Run the application
shinyApp(ui, server)
