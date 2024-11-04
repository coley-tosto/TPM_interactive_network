# app.R
library(shiny)
library(visNetwork)
library(dplyr)
library(tidyr)
library(stringr)
library(igraph)
library(purrr)

# UI Definition
ui <- fluidPage(
  titlePanel("Research Collaboration Network"),
  
  sidebarLayout(
    sidebarPanel(
      # Color coding selection
      selectInput("colorBy", "Color nodes by:",
                  choices = c("Institution" = "organization",
                              "Field" = "main_field",
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