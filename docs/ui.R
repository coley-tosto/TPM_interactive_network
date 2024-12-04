# app.R
library(shiny)
library(visNetwork)
library(dplyr)
library(tidyr)
library(stringr)
library(igraph)
library(purrr)
library(stringdist)
library(googledrive)
library(googlesheets4)
library(shinyjs)

# UI Definition
ui <- fluidPage(
  
  useShinyjs(),
  
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
      
      # Select for specific people
      selectInput("searchPerson", "Select a researcher:",
                  choices = NULL), ##The choices will be populated in the server
      
      # Color coding selection (How do you want the nodes to be colored by?)
      selectInput("colorBy", "Color nodes by:",
                  choices = c("Institution" = "organization",
                              "Department" = "department",
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
