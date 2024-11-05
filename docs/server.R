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


# Function to read the words from the title of a piece of work
get_title <- function(x){
  
  ## Separate title into the separate words
  x <- unlist(strsplit(x, " "))
  
  ## Remove unnecessary symbols (: and ,)
  x <- unlist(strsplit(x, ":"))
  x <- unlist(strsplit(as.character(x), ","))
  
  ## Remove words that have fewer than 4 letters (and, or, then, etc.)
  x <- x[nchar(x) > 4]
  
  ## Remove weird formatting from Zotero style
  exclude_letters <- "<span|style=|span>|<i|i>"
  x <- x[!grepl(exclude_letters, x)]
  
  ## Collapse filtered words into a list separated by a ";"
  paste(x, collapse = "; ")
  
}


# Function to extract main field based on keywords extracted from the titles
get_main_field <- function(keywords) {
  
  ## Combine all of the titles together, remove ; and spaces and convert
  ## all titles to lower case for consistency
  x <- unlist(strsplit(paste(keywords, collapse = ";"), ";"))
  x <- unlist(strsplit(x, " "))
  x <- tolower(x[nchar(x) > 1])
  
  ## IF the combined titles are longer than a single word, do this:
  if(length(x) > 1){
    
    # Ensure that words which are very similar are counted as the same
    # (i.e., "modelling" and "modeling")
    
    ## Calculate the string distance matrix
    dist_matrix <- stringdistmatrix(x, x, method = "jw")  # Jaro-Winkler method works well for short words
  
    ## Cluster words based on similarity - adjust `h` (height) for clustering threshold
    clusters <- cutree(hclust(as.dist(dist_matrix)), 
                       h = 0.2)  # Lower h for stricter similarity
  
    ## Assign each word to its cluster representative
    clustered_words <- sapply(unique(clusters), 
                              function(i) x[clusters == i][1])
  
    ## Replace words with their cluster representative and count them
    clustered_words_vector <- clustered_words[match(clusters, unique(clusters))]
    keyword_counts <- table(clustered_words_vector)
    
    # Pull out the word with the highest frequency
    main_word <- names(keyword_counts[keyword_counts == max(keyword_counts)])
  
    # Account for multiple words with the same frequency
    if(length(main_word) > 1){
    
      paste(main_word[2])
      
      }else{
        
        paste(main_word)
        
      }
    }else{
      
      #If the collection of title words = 1, paste that word
      paste(x[1])
      
    }
}


# Function to get unique color palette for any number of categories
get_color_palette <- function(n) {
  
  colorRampPalette(c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
                     "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"))(n)
  
}

# Server logic
server <- function(input, output, session) {
  
  # Reactive values to store processed data
  network_data <- reactiveVal()
  
  
  # Load and process data
  observe({
    ## Read data files
    zotero_data <- read.csv("../data/zotero_data.csv", stringsAsFactors = FALSE)
    project_data <- read.csv("../data/project_data.csv", header=TRUE, 
                             stringsAsFactors=FALSE, fileEncoding="latin1")
    
    
    ## Process Zotero data to create collaboration network
    collaborations <- zotero_data %>%
      
      # Pulling out unique authors based on the separator ";"
      mutate(authors = strsplit(authors, ";")) %>%
      unnest(authors) %>%
      
      # Using the "standardize_name" function to ensure names have same format
      # Using the "get_title" function separate/filter words from titles
      mutate(author_std = sapply(authors, standardize_name)) %>%
      mutate(title_words = sapply(Title, get_title)) %>% 
      group_by(author_std) %>%
      
      # Counts the number of collaborations for each author and categorizes field
      summarise(n_collaborations = n(), 
                main_field = get_main_field(title_words))
    
    
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
             main_field = ifelse(is.na(main_field), "Unknown", main_field),
             TPM_project = ifelse(is.na(TPM_project), "None", TPM_project),
             title = sprintf("<p><b>%s</b><br>Field: %s<br>Institution: %s<br>Project: %s<br>Collaborations: %d<br>Status: %s</p>",
                             author_std, 
                             main_field,
                             organization,
                             TPM_project,
                             n_collaborations,
                             ifelse(is_tpm, "TPM Member", "External Collaborator"))
             )
    
    # Store processed network data
    network_data(list(nodes = nodes, edges = edges))
    
    })
  
  # Render network
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
             (input$showExternal | group == "TPM Member"))
    
    
    ## Get unique values for the selected grouping variable and generate the 
    ## color palette
    unique_values <- unique(filtered_nodes[[input$colorBy]])
    color_palette <- get_color_palette(length(unique_values))
    
    # Create color mapping and update the node colors based on the selected
    # variable
    color_mapping <- setNames(color_palette, unique_values)
    filtered_nodes$color <- color_mapping[filtered_nodes[[input$colorBy]]]
    
    
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
  
  # Network statistics
  output$networkStats <- renderPrint({
    req(network_data())
    
    filtered_edges <- network_data()$edges %>%
      filter(weight >= input$minCollabs)
    
    connected_nodes <- unique(c(filtered_edges$from, filtered_edges$to))
    
    filtered_nodes <- network_data()$nodes %>%
      filter(id %in% connected_nodes,
             (input$showExternal | group == "TPM Member"))
    
    g <- graph_from_data_frame(filtered_edges, vertices = filtered_nodes$id)
    
    cat("Network Summary:\n")
    cat("Number of Researchers:", vcount(g), "\n")
    cat("Number of Collaborations:", ecount(g), "\n")
    cat("Network Density:", round(edge_density(g), 4), "\n")
    cat("Average Degree:", round(mean(degree(g)), 2), "\n")
    cat("\nMost Connected Researchers:\n")
    degree_table <- data.frame(Researcher = names(degree(g)),
                               Connections = degree(g)) %>%
      
      arrange(desc(Connections)) %>%
      
      head(5)
    
    print(degree_table)
  })
}