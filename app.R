options(shiny.error = NULL)
# -----------------
# Load Libraries
# -----------------
library(class)
library(scales)
library(shiny)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(igraph)
library(networkD3)



# -----------------
# Read Data
# -----------------
nodes <- read.csv('nodes_filtered.csv')
kgrawo <- read.csv('kg_raw_orig_filtered.csv')
kgrawm <- read.csv('kg_raw_mathys_filtered.csv')


# Set color palette
num_levels <- 10
blue_palette <- colorRampPalette(c("lightblue", "darkblue"))(num_levels)


# -----------------
# Define App Aesthetics
# -----------------
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
      body {
        background-color: black;
        color: #459dcf;
      }
      #main-title {
        background: linear-gradient(90deg, #000000, #0056B3);
        height: 70px;
        padding: 15px 0;
        color: #FFFFFF;
      }
      .sidebar {
        background-color: #000000;
      }
      .well {
        background-color: #000000;
        border: none;
      }
      .checkbox {
        color: lightgray;
      }
      .radio {
        color: lightgray;
      }
      .selectize-input {
        color: #000000;
      }
      .shiny-output-error {
        color: #000;
      }
      .shiny-output-error:before {
        color: #000;
      }
      #kg_stat {
        background: linear-gradient(90deg, #0056B3, #000000);
        border-radius: 10px;
        box-shadow: 5px 5px 10px #000000;
        color: #FFFFFF;
        padding: 15px;
        margin: 10px 0;
      }
      #networkPlot {
        background: linear-gradient(90deg, #0056B3, #000000);
        border-radius: 10px;
        box-shadow: 5px 5px 10px #000000;
        color: #FFFFFF;
        padding: 15px;
        margin: 10px 0;
      }
      #explanation_output {
        background: linear-gradient(90deg, #000000, #0056B3);
        border-radius: 10px;
        box-shadow: 5px 5px 10px #000000;
        color: #FFFFFF;
        padding: 15px;
        margin: 10px 0;
      }
      ")
    )
  ),
  
  
  # -----------------
  # Define App Layout
  # -----------------
  useShinyjs(),  
  titlePanel(h3(id = "main-title", "AlzKG Graph Visualization")),
  sidebarLayout(
    sidebarPanel(
      # Dataset selection
      div(style = "margin-bottom: 20px;",
          selectInput("dataset", "Choose a KG:",
                      choices = c("kgraw_with_mathys" = "kg_raw_m", "kg_raw_filtered" = "kg_raw_o"),
                      selected ="kg_raw_m")),
      
      # Focal node for visualization selection
      div(style = "margin-bottom: 20px;",
          selectizeInput("nodeInterest",
                         label = "Select a Starting Node",
                         choices = c("A1BG" = "1_gene/protein_A1BG", "Mic" = "mathys-c6_celltype_Mic"),
                         selected = "mathys-c6_celltype_Mic")),
      
      # Number of nodes to display in the graph
      div(style = "margin-bottom: 20px;",
          numericInput("numNodesDisplay", "Total #Nodes to Display:",
                       value = 50, min = 1)),
      
      # Configuring graph visualization button
      div(style = "margin-bottom: 20px;",
          actionButton("filter_graph", "Customize Graph Display")),
      
      # Conditional panel for visualization configuration options
      conditionalPanel(
        condition = "input.filter_graph > 0",
        div(style = "padding: 20px; margin-bottom: 20px;",
            radioButtons("mode", "Display Options:",
                         choices = c("Select Node Types" = "bnode", "Select Edge Types" = "bedge"))),
        
        # Node type selection for visualization
        conditionalPanel(
          condition = "input.mode === 'bnode'",
          div(selectInput("x_node_type",
                          "Select Node Types for X:",
                          choices = c('celltype', 'gene/protein', 'disease', 'pathway'),
                          selected = c('celltype', 'gene/protein', 'disease', 'pathway'),
                          multiple = TRUE)),
          div(selectInput("y_node_type",
                          "Select Node Types for Y:",
                          choices = c('celltype', 'gene/protein', 'disease', 'pathway'),
                          selected = c('celltype', 'gene/protein', 'disease', 'pathway'),
                          multiple = TRUE))
        ),
        
        # Edge type selection for visualization
        conditionalPanel(
          condition = "input.mode === 'bedge'",
          div(selectInput("edge_type",
                          "Select Edge Types:",
                          choices = c('no-pathology vs pathology up', 'no-pathology vs pathology down',
                                      'no-pathology vs early-pathology up', 'no-pathology vs early-pathology down',
                                      'early-pathology vs late-pathology up', 'early-pathology vs late-pathology down',
                                      'protein_protein',  'disease_protein', 'disease_disease', 'pathway_pathway', 'pathway_protein'),
                          selected = c('no-pathology vs pathology up', 'no-pathology vs pathology down',
                                       'no-pathology vs early-pathology up', 'no-pathology vs early-pathology down',
                                       'early-pathology vs late-pathology up', 'early-pathology vs late-pathology down',
                                       'protein_protein',  'disease_protein', 'disease_disease', 'pathway_pathway', 'pathway_protein'),  # Default selection
                          multiple = TRUE))
        )
      )
    ),
    
    mainPanel(
      forceNetworkOutput("networkPlot"),  # Visualization output
      uiOutput("kg_stats")
    )
  )
)

server <- function(input, output, session) {
  
  data <- reactive({
    switch(input$dataset,
           "kg_raw_m" = kgrawm,  # When the choice is "mathys_kg_raw_filtered"
           "kg_raw_o" = kgrawo   # When the choice is "kg_raw_filtered"
    )
  })
  
  
  filtered_data <- reactive({
    # Initial dataset without any filtering applied
    dataset <- data()
    
    # Check if the "Filter Graph?" button has been clicked
    if(input$filter_graph > 0) {
      if(input$mode == "bnode") {
        req(input$x_node_type, input$y_node_type)  # Ensure that inputs are not NULL
        
        # Apply filtering based on node types if "Select Node Types" is chosen
        dataset <- dataset %>%
          filter(x_type %in% input$x_node_type & y_type %in% input$y_node_type)
      } else if(input$mode == "bedge") {
        req(input$edge_type)  # Ensure that inputs are not NULL
        
        # Apply filtering based on edge types if "Select Edge Types" is chosen
        dataset <- dataset %>%
          filter(relation %in% input$edge_type)
      }
    }
    
    return(dataset)
  })
  
  create_graph <- reactive({
    fdg <- filtered_data()
    
    # Convert all IDs to character type
    nodes$node_id <- as.character(nodes$node_id)
    nodes$node_type <- as.character(nodes$node_type)
    nodes$node_name <- as.character(nodes$node_name)
    fdg$x_id <- as.character(fdg$x_id)
    fdg$x_type <- as.character(fdg$x_type)
    fdg$x_name <- as.character(fdg$x_name)
    fdg$y_id <- as.character(fdg$y_id)
    fdg$y_type <- as.character(fdg$y_type)
    fdg$y_name <- as.character(fdg$y_name)
    
    # Create unique IDs
    nodes$unique_id <- paste(nodes$node_id, nodes$node_type, nodes$node_name, sep = "_")
    fdg$x_unique_id <- paste(fdg$x_id, fdg$x_type, fdg$x_name, sep = "_")
    fdg$y_unique_id <- paste(fdg$y_id, fdg$y_type, fdg$y_name, sep = "_")
    
    # Remove potential whitespace
    nodes$unique_id <- trimws(nodes$unique_id)
    fdg$x_unique_id <- trimws(fdg$x_unique_id)
    fdg$y_unique_id <- trimws(fdg$y_unique_id)
    
    # Apply filter to fdg based on both x_unique_id and y_unique_id
    initial_row_count <- nrow(fdg)
    fdg_filtered <- fdg[fdg$x_unique_id%in%nodes$unique_id & fdg$y_unique_id%in%nodes$unique_id, ]
    filtered_row_count <- nrow(fdg_filtered)
    
    # Print diagnostic information only if there's a difference
    # igraph requires every node in kg to be defined in nodes
    if (initial_row_count != filtered_row_count) {
      print(paste("Initial row count:", initial_row_count))
      print(paste("Filtered row count:", filtered_row_count))
      print(paste("Rows filtered:", initial_row_count - filtered_row_count))
    }
    
    # Create edges data frame
    edges <- data.frame(from = fdg_filtered$x_unique_id, to = fdg_filtered$y_unique_id)
    
    # Add 'name' attribute for vertices, needed for igraph
    nodes$name <- nodes$unique_id
    # Rearrange the columns to make 'name' the first column
    nodes <- nodes[c("name", setdiff(names(nodes), "name"))]
    
    # Create graph object
    g <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)
    
    
    if (!is_igraph(g)) {
      print("Graph object is not valid.")
      return(NULL)
    } else {
      print("Graph object is valid.")
    }
    return(g)
  })
  
  output$networkPlot <- renderForceNetwork ({
    fdg <- filtered_data()
    
    # Convert all IDs to character type
    nodes$node_id <- as.character(nodes$node_id)
    nodes$node_type <- as.character(nodes$node_type)
    nodes$node_name <- as.character(nodes$node_name)
    fdg$x_id <- as.character(fdg$x_id)
    fdg$x_type <- as.character(fdg$x_type)
    fdg$x_name <- as.character(fdg$x_name)
    fdg$y_id <- as.character(fdg$y_id)
    fdg$y_type <- as.character(fdg$y_type)
    fdg$y_name <- as.character(fdg$y_name)
    
    # Create unique IDs
    nodes$unique_id <- paste(nodes$node_id, nodes$node_type, nodes$node_name, sep = "_")
    fdg$x_unique_id <- paste(fdg$x_id, fdg$x_type, fdg$x_name, sep = "_")
    fdg$y_unique_id <- paste(fdg$y_id, fdg$y_type, fdg$y_name, sep = "_")
    
    # Remove potential whitespace
    nodes$unique_id <- trimws(nodes$unique_id)
    fdg$x_unique_id <- trimws(fdg$x_unique_id)
    fdg$y_unique_id <- trimws(fdg$y_unique_id)
    
    # Apply filter to fdg based on both x_unique_id and y_unique_id
    initial_row_count <- nrow(fdg)
    fdg_filtered <- fdg[fdg$x_unique_id%in%nodes$unique_id & fdg$y_unique_id%in%nodes$unique_id, ]
    filtered_row_count <- nrow(fdg_filtered)
    
    # Print diagnostic information only if there's a difference
    # igraph requires every node in kg to be defined in nodes
    if (initial_row_count != filtered_row_count) {
      print(paste("Initial row count:", initial_row_count))
      print(paste("Filtered row count:", filtered_row_count))
      print(paste("Rows filtered:", initial_row_count - filtered_row_count))
    }
    
    # Create edges data frame
    edges <- data.frame(from = fdg_filtered$x_unique_id, to = fdg_filtered$y_unique_id)
    
    # Add 'name' attribute for vertices, needed for igraph
    nodes$name <- nodes$unique_id
    # Rearrange the columns to make 'name' the first column
    nodes <- nodes[c("name", setdiff(names(nodes), "name"))]
    
    # Create graph object
    g <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)
    
    
    if (!is_igraph(g)) {
      print("Graph object is not valid.")
      return(NULL)
    } else {
      print("Graph object is valid.")
    }
    
    
    # Ensure the node of interest exists in the graph
    if (!input$nodeInterest %in% V(g)$name) {
      stop("Node of interest does not exist in the graph.")
    }
    
    # Find the vertex of the node of interest
    node_vertex <- V(g)[V(g)$name == input$nodeInterest]
    
    #Check if the node has any neighbors
    node_neighbors <- neighbors(g, node_vertex, mode = "all")
    
    # Check if any neighbors were found
    if (length(node_neighbors) > 0) {
      print(paste("Node has", length(node_neighbors), "immediate neighbors."))
    } else {
      print("Node has no immediate neighbors.")
    }
    
    # Use a breadth-first search to find a set of vertices around the node of interest
    # This approach is more straightforward and avoids directly managing bfs callbacks
    neighbor_ids <- unlist(neighborhood(g, order = 2, nodes = node_vertex, mode = c("all")))
    
    if (length(neighbor_ids) == 0) {
      print("No neighbors were found.")
    } else {
      print(paste("Found", length(neighbor_ids), "neighbors."))
    }
    
    # If the number of neighbors (including the node of interest) exceeds the user's limit,
    # we need to select a subset. Here, we prioritize closer neighbors.
    if (length(neighbor_ids) > input$numNodesDisplay) {
      # Create a subgraph that includes up to the desired number of neighbors, prioritizing closeness.
      # This simplistic approach doesn't explicitly rank by connectivity beyond the immediate neighborhood.
      subgraph_vertices <- neighbor_ids[1:input$numNodesDisplay]
    } else {
      subgraph_vertices <- neighbor_ids
    }
    
    # Create the subgraph
    sub_g <- induced_subgraph(g, vids = subgraph_vertices)
    
    edges_df <- igraph::as_data_frame(sub_g, what = "edges")
    vertices_df <- igraph::as_data_frame(sub_g, what = "vertices")
    vertices_df$group <- sapply(vertices_df$name, function(x) {
      if(x %in% nodes$node_type) {
        return(nodes$group[nodes$node_type == x])
      } else {
        return(NA) # or a default group if preferred
      }
    })
    
    # Assuming vertices_df and edges_df are your node and edge data frames, respectively
    # Create a mapping from node names to indices
    name_to_index <- setNames(seq_len(nrow(vertices_df)), vertices_df$name)
    
    
    # Convert 'from' and 'to' in edges_df to indices
    edges_df$source <- name_to_index[edges_df$from]
    edges_df$target <- name_to_index[edges_df$to]
    
    
    # Ensure the source and target columns in edges_df are zero-indexed for networkD3
    edges_df$source <- edges_df$source - 1
    edges_df$target <- edges_df$target - 1
    
    # Assuming 'nodes' dataframe has accurate 'node_type' and corresponding 'group' information
    
    # Example of how to map 'node_type' to 'group' again (conceptual, adjust based on your data)
    unique_node_types <- unique(nodes$node_type)
    node_type_to_group <- setNames(seq_along(unique_node_types), unique_node_types)
    nodes$group <- unname(node_type_to_group[nodes$node_type])
    
    # Now, for each node in 'sub_g', map the correct group based on 'node_type'
    # This requires 'vertices_df' to have a 'node_type' column or a way to determine each node's type
    vertices_df$group <- unname(node_type_to_group[vertices_df$node_type])
    
    # If 'vertices_df' does not have 'node_type', you might need to join or map it from the original 'nodes' dataframe
    
    # Ensure 'nodes11' gets this updated group information
    nodes11 <- data.frame(name = vertices_df$name, group = vertices_df$group)
    
    # Creating simplified_name column
    nodes11$simplified_name <- sapply(nodes11$name, function(x) {
      parts <- strsplit(x, "_", fixed = TRUE)[[1]]
      if(length(parts) > 1) {
        return(paste(parts[-1], collapse = "_"))
      } else {
        return(x)
      }
    })
    
    # Print a summary of group assignments in nodes11
    print(table(nodes11$group))
    
    # Use forceNetwork to create the network visualization
    # Assuming 'edges_df' has a column named 'edge_label' that you want to display as the tooltip for each edge
    forceNetwork(Links = edges_df, Nodes = nodes11, Source = "source", Target = "target",
                 NodeID = "simplified_name", Group = "group", opacity = 0.8, zoom = TRUE, bounded = TRUE,
                 colourScale = JS("d3.scaleOrdinal().domain([1, 2, 3, 4]).range(['#ff7f0e', '#4caf50', '#00bcd4', '#e91e63'])"))
    
    
    
  })
  
  
  
  output$kg_stats <- renderUI({
    g <- create_graph()  
    
    # Basic Graph Properties
    num_nodes <- vcount(g)
    num_edges <- ecount(g)
    graph_density <- edge_density(g)
    is_graph_connected <- is_connected(g)
    
    # Simpler Centrality Measures
    avg_degree_centrality <- mean(degree(g))
    # Commenting out more complex computations for now
    # avg_closeness_centrality <- mean(closeness(g))
    # avg_betweenness_centrality <- mean(betweenness(g))
    
    # Community Detection (can be very expensive on large graphs)
    # communities_detected <- max(membership(cluster_louvain(g)))
    
    # Path Analysis (can also be expensive)
    # avg_path_length <- average.path.length(g)
    # graph_diameter <- diameter(g)
    
    # Format the statistics into a string
    stats_text <- paste(
      "Number of Nodes:", num_nodes, "<br>",
      "Number of Edges:", num_edges, "<br>",
      "Graph Density:", round(graph_density, 4), "<br>",
      "Is Connected:", is_graph_connected, "<br>",
      "Average Degree Centrality:", round(avg_degree_centrality, 4), "<br>",
      # "Average Closeness Centrality:", round(avg_closeness_centrality, 4), "<br>",
      # "Average Betweenness Centrality:", round(avg_betweenness_centrality, 4), "<br>",
      # "Number of Communities Detected:", communities_detected, "<br>",
      # "Average Path Length:", round(avg_path_length, 4), "<br>",
      # "Diameter:", graph_diameter, "<br>",
      sep = ""
    )
    
    # Format the statistics into a string with HTML markup
    stats_html <- paste0(
      "<div style='padding: 10px; margin-bottom: 20px;'>",
      "<div style='color: #000000; background-color: #F8FBFF; padding: 20px; margin: 20px; border-radius: 5px; margin-bottom: 20px;'>",
      # Legend for node colors
      "<p style='font-size: 18px; font-weight: bold; margin-top: 20px; margin-bottom: 10px;'>Legend:</p>",
      "<p style='font-size: 16px;'><span style='height: 15px; width: 15px; background-color: #FF7F0E; border-radius: 50%; display: inline-block;'></span> Gene Nodes: Orange</p>",
      "<p style='font-size: 16px;'><span style='height: 15px; width: 15px; background-color: #00BCD4; border-radius: 50%; display: inline-block;'></span> Celltype/State: Blue</p>",
      "<p style='font-size: 16px;'><span style='height: 15px; width: 15px; background-color: #4CAF50; border-radius: 50%; display: inline-block;'></span> Pathway: Green</p>",
      "<p style='font-size: 18px; font-weight: bold; margin-top: 10px; margin-bottom: 10px;'>Graph Details from igraph:</p>",
      "<p style='font-size: 16px; margin-top: 0; margin-bottom: 5px;'>Number of Nodes: ", num_nodes, "</p>",
      "<p style='font-size: 16px; margin-top: 0; margin-bottom: 5px;'>Number of Edges: ", num_edges, "</p>",
      "<p style='font-size: 16px; margin-top: 0; margin-bottom: 5px;'>Graph Density: ", round(graph_density, 4), "</p>",
      #"<p style='font-size: 16px; margin-top: 0; margin-bottom: 5px;'>Is Connected: ", is_graph_connected, "</p>",
      "<p style='font-size: 16px; margin-top: 0; margin-bottom: 5px;'>Average Degree Centrality: ", round(avg_degree_centrality, 4), "</p>",
      # Additional formatted statistics can be added here
      "</div>",
      "</div>"
    )
    
    # Return the formatted HTML for rendering in the UI
    HTML(stats_html)
  })
  
  
}



# Run the app
shinyApp(ui = ui, server = server)
