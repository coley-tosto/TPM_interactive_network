# app.R
library(shiny)
library(visNetwork)
library(dplyr)
library(tidyr)
library(stringr)
library(igraph)
library(purrr)
library(stringdist)

# UI Definition
ui <- fluidPage(
  titlePanel("Research Collaboration Network"),
  
  sidebarLayout(
    sidebarPanel(
      
      # File upload inputs
      fileInput("zoteroData", "Upload Zotero Data (CSV)",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      fileInput("projectData", "Upload Project Data (CSV)",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      
      # Year filter
      sliderInput("yearRange", "Filter by Year:",
                  min = 2015, max = as.numeric(format(Sys.Date(), "%Y")), value = c(2021, 2024), step = 1),
      
      # Search box for specific people
      textInput("searchPerson", "Search for a researcher:",
                placeholder = "Enter name..."),
      
      # Color coding selection
      selectInput("colorBy", "Color nodes by:",
                  choices = c("Institution" = "organization",
                              "Project" = "TPM_project",
                              "Member Type" = "group"),
                  selected = "organization"),
      
      # Minimum collaboration filter
      sliderInput("minCollabs", "Minimum number of collaborations:",
                  min = 1, max = 10, value = 2, step = 1),
      
      # Show external collaborators toggle
      checkboxInput("showExternal", "Show External Collaborators", TRUE),
      
      width = 3
    ),
    
    mainPanel(
      visNetworkOutput("network", height = "800px"),
      div(
        style = "margin-top: 20px",
        h4("Network Statistics"),
        verbatimTextOutput("networkStats")
      ),
      width = 9
    )
  )
)

# Run the app
#shinyApp(ui = ui, server = server)