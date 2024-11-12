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
  
  # Reactive values to store uploaded data and processed network
  data_store <- reactiveValues(zotero_data = NULL,
                               project_data = NULL)
    
  network_data <- reactiveVal()
    
  # Handle file uploads
  observeEvent(input$zoteroData, {
      
    data_store$zotero_data <- read.csv(input$zoteroData$datapath, 
                                       stringsAsFactors = FALSE)
      
    })
    
  observeEvent(input$projectData, {
      
    data_store$project_data <- read.csv(input$projectData$datapath, 
                                        stringsAsFactors = FALSE,
                                        fileEncoding = "latin1")
      
  })
  
  # Process data when both files are uploaded
  observe({
    
    req(data_store$zotero_data, data_store$project_data)
    
    zotero_data <- data_store$zotero_data
    project_data <- data_store$project_data
    
    ## Filter by year if the Year column exists
    
    if("Publication.Year" %in% colnames(zotero_data)) {
      
      zotero_data <- zotero_data %>%
        filter(Publication.Year >= input$yearRange[1] & Publication.Year <= input$yearRange[2])
      
    }
    
    
    ## Process Zotero data to create collaboration network
    collaborations <- zotero_data %>%
      
      # Pulling out unique authors based on the separator ";"
      mutate(authors = strsplit(authors, ";")) %>%
      unnest(authors) %>%
      
      # Using the "standardize_name" function to ensure names have same format
      mutate(author_std = sapply(authors, standardize_name)) %>%
      group_by(author_std) %>%
      
      # Counts the number of collaborations for each author 
      summarise(n_collaborations = n())
    
    
    ## Create edges from co-authorship
    edges <- zotero_data %>%
      
      # Splitting authors and standardizing names
      mutate(authors = strsplit(authors, ";")) %>%
      unnest(authors) %>%
      mutate(author_std = sapply(authors, standardize_name)) %>%
      
      # First count number of authors per publication
      group_by(Key) %>%
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

    
    ## Create nodes with project information
    nodes <- collaborations %>%
      
      # Add a column to the dataset which states whether or not the person is 
      # a part of the TPM group
      mutate(is_tpm = author_std %in% project_data$stand_name) %>%
      
      # Combine collaboration data with the TPM metadata
      # IF author is not in TPM, an "NA" is substituted
      left_join(project_data, by = c("author_std" = "stand_name")) %>%
      
      # Create a bunch of columns for the visual components of the network
      mutate(id = author_std,
             label = ifelse(is_tpm, author_std, ""),  # Only show labels for TPM members
             group = ifelse(is_tpm, "TPM Member", "External Collaborator"),
             size = n_collaborations * 3 + 10, # Size of node based on number collaborations
             font.size = 30,  # Increase label font size
             borderWidth = ifelse(is_tpm, 3, 1), # Increased border for TPM members
             organization = ifelse(is.na(organization), "Unknown", organization),
             TPM_project = ifelse(is.na(TPM_project), "None", TPM_project),
             title = sprintf("<p><b>%s</b><br>Institution: %s<br>Project: %s<br>Collaborations: %d<br>Status: %s</p>",
                             author_std, 
                             organization,
                             TPM_project,
                             n_collaborations,
                             ifelse(is_tpm, "TPM Member", "External Collaborator"))
             )
    
    # Store processed network data
    network_data(list(nodes = nodes, edges = edges))
    
    })
  

  # Creating the network  
  output$network <- renderVisNetwork({
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
    
    ## Apply search filter if provided
    if (!is.null(input$searchPerson) && nchar(trimws(input$searchPerson)) > 0) {
      
      search_pattern <- tolower(trimws(input$searchPerson))
      
      # Find matching nodes
      matching_nodes <- filtered_nodes$id[grepl(search_pattern, 
                                                tolower(filtered_nodes$id), 
                                                fixed = TRUE)]
      
      
      if(length(matching_nodes) > 0) {
        
        ## Find connected nodes (1 degree of separation)
        connected_to_matches <- filtered_edges %>%
          
          filter(from %in% matching_nodes | to %in% matching_nodes) %>%
          gather(key, value, from:to) %>%
          
          pull(value) %>%
          unique()
        
        ## Update node properties based on search and connections
        filtered_nodes <- filtered_nodes %>%
          
          mutate(isMatch = id %in% matching_nodes,
                 isConnected = id %in% connected_to_matches & !isMatch,
                 
                 # Show labels for matching and connected nodes
                 label = case_when(isMatch ~ id,
                                   isConnected ~ id,
                                   TRUE ~ label),
                 
                 # Visual properties for matching nodes
                 borderWidth = case_when(isMatch ~ 8,
                                         isConnected ~ 4,
                                         TRUE ~ borderWidth),
                 
                 font.size = case_when(isMatch ~ 45,
                                       isConnected ~ 35,
                                       TRUE ~ font.size),
                 
                 opacity = case_when(isMatch ~ 1,
                                     isConnected ~ 0.8,
                                     TRUE ~ 0.2),
                 
                 shadow = isMatch)
        
        # Update edge properties
        filtered_edges <- filtered_edges %>%
          
          mutate(isHighlighted = (from %in% matching_nodes | to %in% matching_nodes),
                 width = case_when(isHighlighted ~ width * 2,
                                   TRUE ~ width),
                 opacity = case_when(isHighlighted ~ 1,
                                     TRUE ~ 0.2),
                 color = case_when(isHighlighted ~ "#ff3333",  # Bright red for highlighted connections
                                   TRUE ~ "#848484"  # Gray for other connections
                                   )
          )
        }
    }
    

    ## Get unique values for the selected grouping variable and generate the 
    ## color palette
    unique_values <- unique(filtered_nodes[[input$colorBy]])
    color_palette <- get_color_palette(length(unique_values))
    
    # Create color mapping and update the node colors based on the selected
    # variable
    color_mapping <- setNames(color_palette, unique_values)
    filtered_nodes$color <- color_mapping[filtered_nodes[[input$colorBy]]]
    
    # Modify colors for matching nodes if search is active
    if (!is.null(input$searchPerson) && nchar(trimws(input$searchPerson)) > 0 && length(matching_nodes) > 0) {
      filtered_nodes <- filtered_nodes %>%
        mutate(color = case_when(isMatch ~ "#ff3333",  # Bright red for matches
                                 isConnected ~ color,   # Keep category color for connected nodes
                                 TRUE ~ color))          # Keep category color for others
    }
    
    
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
      
      visLegend(addNodes = data.frame(label = names(color_mapping),
                                      color = color_mapping,
                                      shape = "dot"), 
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
    
    req(network_data())
    
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
  })
  
  
  # Summary of Publication Types
  output$publicationStats <- renderUI({
    
    req(network_data())
    
    ## Calculate the publication type statistics
    pub_stats <- data_store$zotero_data %>%
      
      filter(Publication.Year >= input$yearRange[1],
             Publication.Year <= input$yearRange[2]) %>%
      
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
  })
}