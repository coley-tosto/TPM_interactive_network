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
      selectInput("projectFilter", "Filter by Project:",
                  choices = NULL, multiple = TRUE),
      selectInput("fieldFilter", "Filter by Field:",
                  choices = NULL, multiple = TRUE),
      selectInput("institutionFilter", "Filter by Institution:",
                  choices = NULL, multiple = TRUE),
      sliderInput("yearRange", "Year Range:",
                  min = 2000, max = 2024,
                  value = c(2000, 2024)),
      actionButton("resetFilters", "Reset Filters")
    ),
    
    mainPanel(
      visNetworkOutput("network", height = "800px"),
      div(
        style = "margin-top: 20px",
        h4("Network Statistics"),
        verbatimTextOutput("networkStats")
      )
    )
  )
)


# Run the app
#shinyApp(ui = ui, server = server)