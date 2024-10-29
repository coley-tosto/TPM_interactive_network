# Function to standardize author names
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



# Function to extract main field based on keywords
get_main_field <- function(keywords) {
  
  # Define field categories and their associated keywords
  field_keywords <- list(
    ecology = c("ecosystem", "habitat", "speciation"),
    evolution = c("evolution", "phylogenetic", "genetic", "adaptation", "selection", "conflict"),
    conservation = c("conservation", "preservation", "endangered", "biodiversity"),
    geology = c("geology", "geological", "rock", "mineral", "sediment"),
    psychology = c("psychology", "behavior", "cognitive", "mental")
  )
  
  # Count matches for each field
  field_counts <- sapply(field_keywords, function(kw) {
    sum(str_detect(tolower(keywords), paste(kw, collapse = "|")))
  })
  
  # Return the field with most matches
  names(which.max(field_counts))
  
}

# Server logic
server <- function(input, output, session) {
  # Reactive values to store processed data
  network_data <- reactiveVal()
  
  
  # Load and process data
  observe({
    ## Read data files
    zotero_data <- read.csv("../data/zotero_data.csv", stringsAsFactors = FALSE)
    project_data <- read.csv("../data/project_data.csv", stringsAsFactors = FALSE)
    
    
    ## Process Zotero data to create collaboration network
    collaborations <- zotero_data %>%
      
      # Pulling out unique authors based on the separater ";"
      mutate(authors = strsplit(authors, ";")) %>%
      unnest(authors) %>%
      
      # Using the "standardize_name" function to ensure names have same format
      mutate(author_std = sapply(authors, standardize_name)) %>%
      group_by(author_std) %>%
      
      # Counts the number of collaborations for each author and categorizes field
      summarise(
        n_collaborations = n(),
        keywords = paste(unique(unlist(strsplit(keywords, "[,;]"))), collapse = " "),
        main_field = get_main_field(keywords)
      )
    
    
    ## Create edges from co-authorships
    edges <- zotero_data %>%
      
      # Splitting suthors and standardizing names
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
      summarise(weight = n(), .groups = 'drop')
    
    # Create nodes with project information
    nodes <- collaborations %>%
      left_join(project_data, by = c("author_std" = "name")) %>%
      mutate(
        id = author_std,
        label = author_std,
        color = main_field,
        size = n_collaborations * 5,
        title = sprintf(
          "<p><b>%s</b><br>Field: %s<br>Institution: %s<br>Project: %s<br>Joined: %d<br>Publications: %d</p>",
          author_std, main_field, institution, project, year_joined, n_collaborations
        )
      )
    
    # Update filter choices
    updateSelectInput(session, "projectFilter",
                      choices = unique(nodes$project))
    updateSelectInput(session, "fieldFilter",
                      choices = unique(nodes$main_field))
    updateSelectInput(session, "institutionFilter",
                      choices = unique(nodes$institution))
    updateSliderInput(session, "yearRange",
                      min = min(nodes$year_joined),
                      max = max(nodes$year_joined))
    
    # Store processed network data
    network_data(list(nodes = nodes, edges = edges))
  })
  
  # Render network
  output$network <- renderVisNetwork({
    req(network_data())
    
    # Apply filters with NA handling
    filtered_nodes <- network_data()$nodes %>%
      filter(
        (is.null(input$projectFilter) | project %in% input$projectFilter | is.na(project)),
        (is.null(input$fieldFilter) | main_field %in% input$fieldFilter | is.na(main_field)),
        (is.null(input$institutionFilter) | institution %in% input$institutionFilter | is.na(institution))
      ) %>%
      filter(
        is.na(year_joined) | 
          (year_joined >= input$yearRange[1] & year_joined <= input$yearRange[2])
      )
    
    filtered_edges <- network_data()$edges %>%
      filter(
        from %in% filtered_nodes$id,
        to %in% filtered_nodes$id
      )
    
    # Create network visualization
    visNetwork(filtered_nodes, filtered_edges) %>%
      visOptions(
        highlightNearest = list(enabled = TRUE, degree = 1),
        selectedBy = list(variable = "main_field", multiple = TRUE)
      ) %>%
      visPhysics(stabilization = FALSE) %>%
      visInteraction(
        hover = TRUE,
        zoomView = TRUE,
        dragView = TRUE
      ) %>%
      visEvents(click = "function(nodes) {
        if (nodes.nodes.length > 0) {
          this.fit({
            nodes: [nodes.nodes[0]],
            animation = true
          });
        }
      }")
  })
  
  # Update network statistics handling with NA handling
  output$networkStats <- renderPrint({
    req(network_data())
    
    filtered_nodes <- network_data()$nodes %>%
      filter(
        (is.null(input$projectFilter) | project %in% input$projectFilter | is.na(project)),
        (is.null(input$fieldFilter) | main_field %in% input$fieldFilter | is.na(main_field)),
        (is.null(input$institutionFilter) | institution %in% input$institutionFilter | is.na(institution)),
        is.na(year_joined) | 
          (year_joined >= input$yearRange[1] & year_joined <= input$yearRange[2])
      )
    
    filtered_edges <- network_data()$edges %>%
      filter(
        from %in% filtered_nodes$id,
        to %in% filtered_nodes$id
      )
    
    # Create igraph object for network metrics
    g <- graph_from_data_frame(filtered_edges, vertices = filtered_nodes$id)
    
    cat("Network Summary:\n")
    cat("Number of Researchers:", vcount(g), "\n")
    cat("Number of Collaborations:", ecount(g), "\n")
    cat("Network Density:", edge_density(g), "\n")
    cat("Average Degree:", mean(degree(g)), "\n")
    cat("Most Connected Researchers:\n")
    print(head(sort(degree(g), decreasing = TRUE), 5))
  })
  
  # Also modify the initial year range setup to handle missing data
  observe({
    req(network_data())
    years <- network_data()$nodes$year_joined
    valid_years <- years[!is.na(years)]
    if(length(valid_years) > 0) {
      updateSliderInput(session, "yearRange",
                        min = min(valid_years),
                        max = max(valid_years),
                        value = c(min(valid_years), max(valid_years)))
    }
  })
  
  # Reset filters
  observeEvent(input$resetFilters, {
    updateSelectInput(session, "projectFilter", selected = character(0))
    updateSelectInput(session, "fieldFilter", selected = character(0))
    updateSelectInput(session, "institutionFilter", selected = character(0))
    updateSliderInput(session, "yearRange",
                      value = c(min(network_data()$nodes$year_joined),
                                max(network_data()$nodes$year_joined)))
  })
}
