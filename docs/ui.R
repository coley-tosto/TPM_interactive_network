# app.R
library(shiny)
library(visNetwork)
library(dplyr)
library(tidyr)
library(stringr)
library(igraph)
library(purrr)
library(stringdist)
library(googlesheets4)

# UI Definition
# UI Definition
ui <- fluidPage(
  
  titlePanel("Research Collaboration Network"),
  
  sidebarLayout(
    sidebarPanel(
      
      # Year filter
      sliderInput("yearRange", "Filter by Year:",
                  min = 2015, 
                  max = as.numeric(format(Sys.Date(), "%Y")), 
                  value = c(2021, 2024), 
                  step = 1,
                  sep = ""),
      
      
      # Color coding selection (How do you want the nodes to be colored by?)
      selectInput("colorBy", "Color nodes by:",
                  choices = c("Institution" = "organization",
                              "Department" = "department",
                              "TPM Memeber" = "person",
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
      
      div(style = "margin-top: 20px",
          tabsetPanel(id = "statsTabs",
                      tabPanel("Network Summary",
                               htmlOutput("networkSummaryStats")),
                      tabPanel("Publication Types",
                               htmlOutput("publicationStats")),
                      tabPanel("Researchers",
                               htmlOutput("researcherStats")))
      ),
      
      width = 9)
  )
)