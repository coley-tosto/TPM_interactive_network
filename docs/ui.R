# app.R
library(shiny)
library(visNetwork)
library(dplyr)
library(tidyr)
library(stringr)
library(igraph)
library(purrr)
## Comment back in if reading in data from google sheets
#library(googlesheets4)

# UI Definition
# UI Definition
ui <- fluidPage(
  
  titlePanel("Research Collaboration Network for Te Pūnaha Matatini"),
  
  div(
    p(style = "font-size: 18px;","This network visualization shows the collaborative connections between researchers within Te Pūnaha Matatini and who they are working with external to the group. The network visualized here was build from co-authorship data pulled from works listed on Scopus. Beneath the network are some statistics about the various collaborations, the different output types, and Te Pūnaha Matatini's reach in policy."),
    p(style = "color: grey; font-size: 16px", "Use the options below to customize your view of the network.")),
  
  sidebarLayout(
    sidebarPanel(
      
      # Year filter
      sliderInput("yearRange",
                  tags$div("Filter by Year:", 
                           tags$p(style = "font-size: 12px; color: gray;", 
                                  "Select the range of years to view collaboration data.")),
                  min = 2015, 
                  max = as.numeric(format(Sys.Date(), "%Y")), 
                  value = c(2021, 2024), 
                  step = 1,
                  sep = ""),
     
      
      
      # Color coding selection (How do you want the nodes to be colored by?)
      selectInput("colorBy", 
                  tags$div("Color nodes by:", 
                           tags$p(style = "font-size: 12px; color: gray;", 
                                  "Select how you would like the nodes to be colored by, including Instituion, Organization, Selecting a specific person, and whether they are a TPM member.")),
                  choices = c("Institution" = "organization",
                              "Department" = "department",
                              "TPM Memeber" = "person",
                              "Member Type" = "group"),
                  selected = "organization"),
      
      # Minimum collaboration filter
      sliderInput("minCollabs", 
                  tags$div("Minimum number of collaborations:", 
                           tags$p(style = "font-size: 12px; color: gray;", 
                                  "Select the minimum number of collaborations a pair must have to be included in the network. This will dictate how many nodes and edges are shown.")),
                  min = 1, max = 10, value = 2, step = 1),
      
      # Show external collaborators toggle
      checkboxInput("showExternal", "Show External Collaborators", FALSE),
      
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
                               htmlOutput("researcherStats")),
                      tabPanel("Appearance in Policy",
                               htmlOutput("overtonStats")))
      ),
      
      width = 9)
  )
)