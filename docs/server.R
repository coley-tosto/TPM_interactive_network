# to setup googlesheet auth (comment out once done) -----------------------------------
#shiny_token <- googlesheets4::gs4_auth() # authenticate w/ your desired Google identity here
#saveRDS(shiny_token, "shiny_app_token.rds")
# 
 googlesheets4::gs4_auth(
  email = "nicole.tosto@gmail.com",
  path = NULL,
  scopes = "https://www.googleapis.com/auth/spreadsheets",
  cache = gargle::gargle_oauth_cache(),
  use_oob = TRUE,
  token = "shiny_app_token.rds"
 )

# Function to standardize author names from Zotero formatting
standardize_name <- function(name) {
  
  # Handle format "Last, First Middle"
  name <- str_trim(name)
  
  # Separates out the Last name vs the first + Middle
  parts <- str_split(name, ",")[[1]]
  
  # If a first AND last name are provided
  if (length(parts) > 1) {
    
    #Pulls out the last name
    last_name <- str_trim(parts[1])
    
    # Separates the first name from the middle initial (if present)
    first_parts <- str_trim(str_split(parts[2], " ")[[1]])
    
    # Pulls out only the first initial
    first_initial <- substr(first_parts[2], 1, 1)
    
  } else {
    
    # Handle other formats (If NO comma separation)
    parts <- str_split(name, " ")[[1]]
    last_name <- parts[[1]]
    first_initial <- substr(parts[2], 1, 1)
    
  }
  
  #Pastes a "standardized" format to use for matching later
  paste(last_name, first_initial, sep = ", ")
  
}

# Function to get unique color palette for any number of categories
get_color_palette <- function(n) {
  
  colorRampPalette(c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
                     "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"))(n)
  
}

# Server logic
server <- function(input, output, session) {
  
  data_store <- reactiveValues(collaboration_data = NULL,
                               overton_authors = NULL,
                               overton_keywords = NULL)
  
  network_data <- reactiveVal()
    
  # Load and process the data
  observe({
    
    ## Read in the chosen "collaboration" dataset and the TPM metadata from a google drive location
    ## Currently using Scopus as the "collaboration" data
    ##Storing the Scopus data for use later on
    collaboration_data <- as.data.frame(read_sheet("https://docs.google.com/spreadsheets/d/1Fhy4H1UnlGDqO6TaKH_q89_fvYsyINNgp7bDViPUV2I/edit?usp=sharing"))
    data_store$collaboration_data <- collaboration_data
    
    metadata <- as.data.frame(read_sheet("https://docs.google.com/spreadsheets/d/16v69T6f9hc7dA8XKZTcECsftq5chI90um0AZP0KtDIo/edit?usp=sharing"))
    
    ## Filter by year if the Year column exists
    
    if("Year" %in% colnames(collaboration_data)) {
      
      collaboration_data <- collaboration_data %>%
        filter(Year >= input$yearRange[1] & Year <= input$yearRange[2])
      
    }
    
    ## Process the Scopus data to create the collaboration network
    collaborations <- collaboration_data %>%
      
      # Pulling out unique authors based on the separator ";"
      mutate(Authors = strsplit(Authors, ";")) %>%
      unnest(Authors) %>%
      
      # Using the "standardize_name" function to ensure names have same format
      mutate(author_std = sapply(Authors, standardize_name)) %>%
      group_by(author_std) %>%
      
      # Counts the number of collaborations for each author 
      summarise(n_collaborations = n())
    
    
    ## Create edges from co-authorship
    edges <- collaboration_data %>%
      
      # Splitting authors and standardizing names
      mutate(Authors = strsplit(Authors, ";")) %>%
      unnest(Authors) %>%
      mutate(author_std = sapply(Authors, standardize_name)) %>%
      
      # First count number of authors per publication
      group_by(EID) %>%
      mutate(n_authors = n()) %>%
      
      # Only create pairs for publications with 2 or more authors
      filter(n_authors >= 2) %>%
      
      # Create all possible pairs within each publication
      do(data.frame(t(combn(.$author_std, 2)), stringsAsFactors = FALSE)) %>%
      
      # Rename columns to 'from' and 'to' and count collaborations
      rename(from = X1, to = X2) %>%
      group_by(from, to) %>%
      summarise(weight = n(),  #How many times each pair has collaborated
                width = sqrt(n()) * 2, #Scaling the edge width based on # of collaborations
                .groups = 'drop')

    
    ## Create nodes with TPM metadata
    nodes <- collaborations %>%
      
      # Add a column to the dataset which states whether or not the person is 
      # a part of the TPM group
      mutate(is_tpm = author_std %in% metadata$stand_name) %>%
      
      # Combine collaboration data with the TPM metadata
      # IF author is not in TPM, an "NA" is substituted
      left_join(metadata, by = c("author_std" = "stand_name")) %>%
      
      # Create a bunch of columns for the visual components of the network
      mutate(id = author_std,
             label = ifelse(is_tpm, author_std, ""),  # Only show labels for TPM members
             group = ifelse(is_tpm, "TPM Member", "External Collaborator"),
             size = n_collaborations * 3 + 10, # Size of node based on number collaborations
             font.size = 30,  # Increase label font size
             borderWidth = ifelse(is_tpm, 3, 1), # Increased border for TPM members
             organization = ifelse(is.na(organization), "Unknown", organization),
             department = ifelse(is.na(department), "Unknown", department),
             person = ifelse(is_tpm, author_std, "External Collaborator"),
             title = sprintf("<p><b>%s</b><br>Institution: %s<br>Department: %s<br>Collaborations: %d<br>Status: %s</p>",
                             author_std, 
                             organization,
                             department,
                             n_collaborations,
                             ifelse(is_tpm, "TPM Member", "External Collaborator"))
             )
    
    # Store processed network data
    network_data(list(nodes = nodes, edges = edges))
    
    #Read-in the the Overton data generated from a list of "authors"
    #Filter to keep only unique instances (by "Matched DOI")
    overton_authors <- as.data.frame(read_sheet('https://docs.google.com/spreadsheets/d/1L5ed2LgBde8cp7fjUPkM71SewzZMAL5A6b3q1c7NvOg/edit?usp=sharing')) %>%
      distinct(`Matched DOI`, .keep_all = TRUE)
    
    #Read-in the Overton data generated with a keyword search
    #Filter to keep only unique instances (by "Policy Document ID")
    overton_keywords <- as.data.frame(read_sheet('https://docs.google.com/spreadsheets/d/1oUdaN45vFKcvvoJ7BOWerAYaYasjTCmbbUiAgmVEI_k/edit?usp=sharing',
                                                 sheet = "Matched references")) %>% 
      distinct(`Policy Document ID`, .keep_all = TRUE)
    
    #Store Overton data for use later
    data_store$overton_authors <- overton_authors
    data_store$overton_keywords <- overton_keywords
    
    
    })
  

  # Creating the network  
  output$network <- renderVisNetwork({
    
    ## Perform a check to ensure all of the dataset filter/building has worked
    ## If problem, no network will show
    req(network_data())
    
    ## Filter based on minimum collaborations
    filtered_edges <- network_data()$edges %>%
      filter(weight >= input$minCollabs)
    
    
    ## Get nodes that have remaining connections
    connected_nodes <- unique(c(filtered_edges$from, filtered_edges$to))
    
    
    ## Filter nodes
    filtered_nodes <- network_data()$nodes %>%
      filter(id %in% connected_nodes,
             (input$showExternal | group == "TPM Member")) %>% 
      
      # Always show labels for TPM members
      mutate(label = case_when(group == "TPM Member" ~ id,
                               TRUE ~ ""),
             
             # Default visual properties
             borderWidth = ifelse(group == "TPM Member", 3, 1),
             font.size = 30)
    
    ## Get unique values for the selected grouping variable and generate the 
    ## color palette
    unique_values <- unique(filtered_nodes[[input$colorBy]])
    color_palette <- get_color_palette(length(unique_values))
    
    ## Create color mapping and update the node colors based on the selected
    ## variable
    color_mapping <- setNames(color_palette, unique_values)
    filtered_nodes$color <- color_mapping[filtered_nodes[[input$colorBy]]]
    
    ## Build the legend based off the color mapping data
    legend_data <- data.frame(label = names(color_mapping),
                              color = unname(color_mapping),
                              shape = "dot")

    
    ## Create network visualization
    visNetwork(filtered_nodes, filtered_edges, width = "100%", height = "800px") %>%
      visGroups() %>%
      
      # Set to highlight the nearest nodes within 2 degrees of collab.
      visOptions(highlightNearest = list(enabled = TRUE, degree = 2),
                 selectedBy = list(variable = input$colorBy, multiple = TRUE)) %>%
      
      # Customization for HOW the network is built
      visPhysics(solver = "forceAtlas2Based", 
                 forceAtlas2Based = list(gravitationalConstant = -100,
                                         centralGravity = 0.01,
                                         springLength = 150,
                                         springConstant = 0.08),
                 stabilization = list(enabled = TRUE,
                                      iterations = 1000)) %>%
      
      # How you want to be able to interact with the network
      # (i.e, zooming in on a node when clicked, showing summary data when hovering, etc.)
      visInteraction(hover = TRUE,
                     zoomView = TRUE,
                     dragView = TRUE,
                     navigationButtons = TRUE) %>%
      
      visLayout(randomSeed = 123) %>% 
      
      visLegend(addNodes = legend_data, 
                useGroups = FALSE) %>%
      
      visEvents(click = "function(nodes) {if (nodes.nodes.length > 0) {
          this.fit({
            nodes: [nodes.nodes[0]],
            animation: true
          });
        }
      }")
  })
  
 
  # Network summary Tabs
  # Overall Summary Statistics
  output$networkSummaryStats <- renderUI({
    
    ## Perform a check to ensure all of the previous steps have worked
    req(network_data())
    
    ## Account for changes when it is only TMP members shown versus when
    ## both TPM members and external collaborators are shown
    if (input$showExternal == TRUE) {
      
    
    ## Get the filtered network data
    filtered_edges <- network_data()$edges %>%
      filter(weight >= input$minCollabs)
    
    connected_nodes <- unique(c(filtered_edges$from, filtered_edges$to))
    
    filtered_nodes <- network_data()$nodes %>%
      filter(id %in% connected_nodes,
             (input$showExternal | group == "TPM Member"))
    
    ## Create igraph object for the network analysis
    g <- graph_from_data_frame(filtered_edges, vertices = filtered_nodes$id)
    
    ## Calculate the TPM-specific metrics
    # Subsetting filtered nodes and edges for TPM-TPM collaborations
    tpm_nodes <- filtered_nodes %>% 
      filter(group == "TPM Member")
    
    tpm_edges <- filtered_edges %>% 
      filter(from %in% tpm_nodes$id & to %in% tpm_nodes$id)
    
    # Calculate the TPM-specific average degree
    tpm_degrees <- degree(g)[tpm_nodes$id]
    tpm_avg_degree <- mean(tpm_degrees)
    
    ## Paste summary statistics
    HTML(paste("<div style='padding: 15px; font-size: 16px;'>",
               "<h3 style='font-size: 24px;'>Researcher Statistics</h3>",
               "<p style='margin-left: 20px;'><strong>Total Researchers:</strong> ", vcount(g), "</p>",
               "<p style='margin-left: 20px;'><strong>TPM Members:</strong> ", nrow(tpm_nodes), "</p>",
               "<p style='margin-left: 20px;'><strong>External Collaborators:</strong> ", vcount(g) - nrow(tpm_nodes), "</p>",
      
               "<h3 style='font-size: 24px;'>Collaboration Statistics</h3>",
               "<p style='margin-left: 20px;'><strong>Total Collaborations:</strong> ", ecount(g), "</p>",
               "<p style='margin-left: 20px;'><strong>TPM Internal Collaborations:</strong> ", nrow(tpm_edges), "</p>",
               "<p style='margin-left: 20px;'><strong>External Collaborations:</strong> ", ecount(g) - nrow(tpm_edges), "</p>",
      
               "<h3 style='font-size: 24px;'>Network Metrics</h3>",
               "<p style='margin-left: 20px;'><strong>Overall Average Degree:</strong> ", round(mean(degree(g)), 2), "</p>",
               "<p style='margin-left: 20px;'><strong>TPM Average Degree:</strong> ", round(tpm_avg_degree, 2), "</p>",
               "</div>")
         )
    }else{
      
      ## Apply additional filtering to keep ONLY TPM members
      filtered_edges <- network_data()$edges %>%
        filter(weight >= input$minCollabs) %>% 
        filter(from %in% (network_data()$nodes$author_std[network_data()$nodes$is_tpm == TRUE]))
      
      
      connected_nodes <- unique(c(filtered_edges$from, filtered_edges$to))
      
      filtered_nodes <- network_data()$nodes %>%
        filter(id %in% connected_nodes)
      
      ## Create igraph object for the network analysis
      g <- graph_from_data_frame(filtered_edges, vertices = filtered_nodes$id)
      
      ## Calculate the TPM-specific metrics
      # Subsetting filtered nodes and edges for TPM-TPM collaborations
      tpm_nodes <- filtered_nodes %>% 
        filter(group == "TPM Member")
      
      tpm_edges <- filtered_edges %>% 
        filter(from %in% tpm_nodes$id & to %in% tpm_nodes$id)
      
      # Calculate the TPM-specific average degree
      tpm_degrees <- degree(g)[tpm_nodes$id]
      tpm_avg_degree <- mean(tpm_degrees)
      HTML(paste("<div style='padding: 15px; font-size: 16px;'>",
                 "<h3 style='font-size: 24px;'>Researcher Statistics</h3>",
                 "<p style='margin-left: 20px;'><strong>TPM Members:</strong> ", nrow(tpm_nodes), "</p>",

                 "<h3 style='font-size: 24px;'>Collaboration Statistics</h3>",
                 "<p style='margin-left: 20px;'><strong>TPM Internal Collaborations:</strong> ", nrow(tpm_edges), "</p>",

                 "<h3 style='font-size: 24px;'>Network Metrics</h3>",
                 "<p style='margin-left: 20px;'><strong>TPM Average Degree:</strong> ", round(tpm_avg_degree, 2), "</p>",
                 "</div>"))
    }
  })
  
  
  # Summary of Publication Types
  output$publicationStats <- renderUI({
    
    req(network_data())
    
    ## Calculate the publication type statistics
    pub_stats <- data_store$collaboration_data %>%
      
      filter(Year >= input$yearRange[1],
             Year <= input$yearRange[2]) %>%
      
      group_by(Item.Type) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      
      arrange(desc(Count))
    
    ## Create the HTML table
    table_rows <- apply(pub_stats, 1, function(row) {
      
      paste0("<tr><td style='padding: 8px;'>", row["Item.Type"], "</td>",
             "<td style='padding: 8px; text-align: right;'>", row["Count"], "</td></tr>")
      
      })
    
    HTML(paste("<div style='padding: 15px;'>",
               "<h3 style='font-size: 24px;'>Publication Types</h3>",
               "<table class='table' style='width: 50%; margin-top: 15px; font-size: 16px;'>",
               "<thead><tr><th style='padding: 8px;'>Type</th><th style='padding: 8px; text-align: right;'>Count</th></tr></thead>",
               "<tbody>",
               paste(table_rows, collapse = ""),
               "</tbody>",
               "</table>",
               "</div>")
         )
  })
  
  
  # Summary of the most connected TPM Researchers
  output$researcherStats <- renderUI({
    
    req(network_data())
    
    ## Account for changes when it is only TMP members shown versus when
    ## both TPM members and external collaborators are shown
    if (input$showExternal == TRUE) {
      
    ## Get the filtered network data
    filtered_edges <- network_data()$edges %>%
      filter(weight >= input$minCollabs)
    
    connected_nodes <- unique(c(filtered_edges$from, filtered_edges$to))
    
    filtered_nodes <- network_data()$nodes %>%
      filter(id %in% connected_nodes,
             (input$showExternal | group == "TPM Member"))
    
    ## Create igraph object for network analysis
    g <- graph_from_data_frame(filtered_edges, 
                               vertices = filtered_nodes$id)
    
    ## Calculate degrees for TPM members only
    degree_table <- data.frame(Researcher = names(degree(g)),
                               Connections = degree(g)) %>%
      
      # Join with filtered_nodes to get group information
      left_join(filtered_nodes %>% select(id, group),
                by = c("Researcher" = "id")) %>%
      
      # Filter for TPM members only
      filter(group == "TPM Member") %>%
      
      # Select and arrange
      select(Researcher, Connections) %>%
      arrange(desc(Connections)) %>%
      head(10)
    
    ## Create the HTML table
    table_rows <- apply(degree_table, 1, function(row) {
      
      paste0("<tr><td style='padding: 8px;'>", row["Researcher"], "</td>",
             "<td style='padding: 8px; text-align: right;'>", row["Connections"], "</td></tr>")
      
    })
    
    HTML(paste("<div style='padding: 15px;'>",
               "<h3 style='font-size: 24px;'>Most Connected TPM Researchers</h3>",
               "<table class='table' style='width: 50%; margin-top: 15px; font-size: 16px;'>",
               "<thead><tr><th style='padding: 8px;'>Researcher</th><th style='padding: 8px; text-align: right;'>Connections</th></tr></thead>",
               "<tbody>",
               paste(table_rows, collapse = ""),
               "</tbody>",
               "</table>",
               "</div>")
         )
    }else{
      
      ## Get the filtered network data - ONLY TPM members
      filtered_edges <- network_data()$edges %>%
        filter(weight >= input$minCollabs) %>% 
        filter(from %in% (network_data()$nodes$author_std[network_data()$nodes$is_tpm == TRUE]))
      
      
      connected_nodes <- unique(c(filtered_edges$from, filtered_edges$to))
      
      filtered_nodes <- network_data()$nodes %>%
        filter(id %in% connected_nodes)
      
      ## Create igraph object for network analysis
      g <- graph_from_data_frame(filtered_edges, 
                                 vertices = filtered_nodes$id)
      
      ## Calculate degrees for TPM members only
      degree_table <- data.frame(Researcher = names(degree(g)),
                                 Connections = degree(g)) %>%
        
        # Join with filtered_nodes to get group information
        left_join(filtered_nodes %>% select(id, group),
                  by = c("Researcher" = "id")) %>%
        
        # Filter for TPM members only
        filter(group == "TPM Member") %>%
        
        # Select and arrange
        select(Researcher, Connections) %>%
        arrange(desc(Connections)) %>%
        head(10)
      
      ## Create the HTML table
      table_rows <- apply(degree_table, 1, function(row) {
        
        paste0("<tr><td style='padding: 8px;'>", row["Researcher"], "</td>",
               "<td style='padding: 8px; text-align: right;'>", row["Connections"], "</td></tr>")
        
      })
      
      HTML(paste("<div style='padding: 15px;'>",
                 "<h3 style='font-size: 24px;'>Most Connected TPM Researchers</h3>",
                 "<table class='table' style='width: 50%; margin-top: 15px; font-size: 16px;'>",
                 "<thead><tr><th style='padding: 8px;'>Researcher</th><th style='padding: 8px; text-align: right;'>Connections</th></tr></thead>",
                 "<tbody>",
                 paste(table_rows, collapse = ""),
                 "</tbody>",
                 "</table>",
                 "</div>")
      )
      
    }
    
  })
  
  # Summary of Overton Statistics
  output$overtonStats <- renderUI({
    
    req(network_data())
    
    ## Filter the overton stats for the specified years
    overton_authors_stats <- data_store$overton_authors %>%
      
      mutate(publication_year = as.numeric(format(as.Date(Published), "%Y"))) %>% 
      
      filter(publication_year >= input$yearRange[1],
             publication_year <= input$yearRange[2])
    
    overton_keywords_stats <- data_store$overton_keywords %>% 
      
      mutate(publication_year = as.numeric(format(as.Date(Published), "%Y"))) %>% 
      
      filter(publication_year >= input$yearRange[1],
             publication_year <= input$yearRange[2])
    
    ## Create summary of the stats
    HTML(paste("<div style='padding: 15px; font-size: 16px;'>",
               "<h3 style='font-size: 24px;'>Te Pūnaha Matatini's reach in Policy Documents</h3>",
               "<p style='margin-left: 20px;'><strong>Number of times a TPM Member's work is cited:</strong> ", nrow(overton_authors_stats), "</p>",
               "<p style='margin-left: 20px;'><strong>Number of times Te Pūnaha Matatini is mentioned:</strong> ", nrow(overton_keywords_stats), "</p>",
               "</div>")
         )
  })
}
