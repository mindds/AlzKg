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
library(htmlwidgets)



# -----------------
# Read Data
# -----------------
# Read Data
nodes <- read.csv('nodes_filtered.csv')

kgrawo <- read.csv('kg_raw_orig_filtered.csv')



kgrawMS <- read.csv('Mathys_ast.csv')
kgrawZS <- read.csv('Zhou_ast.csv')

# Add source type
kgrawo$source_type <- "O"
kgrawMS$source_type <- "M"
kgrawZS$source_type <- "Z"

# Combine datasets
kgrawM <- rbind(kgrawo, kgrawMS)
kgrawZ <- rbind(kgrawo, kgrawZS)
kgrawA <- rbind(kgrawo, kgrawMS, kgrawZS)

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
        color: cornflowerblue;
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
        background: #ffffff;
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
      # Celltype selection
      div(style = "margin-top: 20px; margin-bottom: 25px;",
          selectInput("nodeInterest", 
                      label = "Select Celltype/State:",
                      choices = c("Ast" = "c3_celltype/state_Ast"),
                      selected = "c3_celltype/state_Ast")),
      # Dataset selection 
      div(style = "margin-bottom: 25px;", 
          selectInput("dataset", "Select Datasets:", 
                      choices = c("Mathys" = "kg_raw_m", "Zhou" = "kg_raw_z", "All" = "kg_raw_a"), 
                      selected ="kg_raw_a")),
      
      # Focal node for visualization selection
      div(style = "margin-bottom: 25px;",
          selectInput("nodeInterest1", 
                      label = "Select Central Node:",
                      choices = c("Ast" = "c3_celltype/state_Ast"),
                      selected = "c3_celltype/state_Ast")),
      
      # Common Edges
      div(style = "margin-bottom: 25px;",
          selectInput("commonEdges", 
                      label = "Edge Display:",
                      choices = c("All Edges" = "alledges", "Common Edges" = "commonedges"),
                      selected = "alledges")),
      
      # Number of nodes to display in the graph 
      div(style = "margin-bottom: 25px;",
          numericInput("numNodesDisplay", "Total #Nodes to Display:", 
                       value = 50, min = 1)),
      
    ),
    
    mainPanel(
      forceNetworkOutput("networkPlot"),  # Visualization output
      uiOutput("kg_stats")
    )
  )
)

server <- function(input, output, session) {
  
  data <- reactive({
    if (input$commonEdges == "commonedges" && input$dataset == "kg_raw_a") {
      
      # Filter for gene/protein edges
      kgrawMS_genes <- kgrawMS %>% filter(x_type == "gene/protein" | y_type == "gene/protein")
      kgrawZS_genes <- kgrawZS %>% filter(x_type == "gene/protein" | y_type == "gene/protein")
      
      # Create sets of genes for both datasets
      kgrawMS_gene_set <- unique(c(kgrawMS_genes$x_name, kgrawMS_genes$y_name))
      kgrawZS_gene_set <- unique(c(kgrawZS_genes$x_name, kgrawZS_genes$y_name))
      
      # Initialize an empty data frame to store common genes
      common_genes <- data.frame()
      
      # Iterate through each row in kgrawMS_genes to find common genes
      for (i in 1:nrow(kgrawMS_genes)) {
        row <- kgrawMS_genes[i, ]
        if (row$x_type == "gene/protein" && row$x_name %in% kgrawZS_gene_set) {
          row$source_type <- "C"  # Assign new source_type for common genes
          common_genes <- bind_rows(common_genes, row)
        } else if (row$y_type == "gene/protein" && row$y_name %in% kgrawZS_gene_set) {
          row$source_type <- "C"  # Assign new source_type for common genes
          common_genes <- bind_rows(common_genes, row)
        }
      }
      
      # Combine with original data (kgrawo)
      combined <- bind_rows(kgrawo, common_genes)
      combined
      
      
    } else {
      # Use the selected dataset
      combined <- switch(input$dataset,
                         "kg_raw_m" = kgrawM,
                         "kg_raw_z" = kgrawZ,
                         "kg_raw_a" = kgrawA)
    }
    
    # Add source column if it doesn't exist
    if(!"source" %in% names(combined)) {
      combined$source <- "O"  # Original dataset
    }
    
    combined
  })
  
  
  create_graph <- reactive({
    fdg <- data()   # fdg is final data set for graph
    
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
      print(paste("Rows filtered (to make igraph work):", initial_row_count - filtered_row_count))
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
    fdg <- data() # fdg is final data set for graph
    
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
      print(paste("Rows filtered (for igraph to work properly):", initial_row_count - filtered_row_count))
    }
    
    # Create edges data frame
    edges <- data.frame(from = fdg_filtered$x_unique_id, to = fdg_filtered$y_unique_id, source_type = fdg_filtered$source_type)
    
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
    
    # Add source_type to edges_df by matching from and to
    edges_df$source_type <- edges$source_type[match(paste(edges_df$from, edges_df$to), paste(edges$from, edges$to))]
    
    # Define colors for source types
color_M <- "#00c8ff"  # Light Blue
color_Z <- "#9f67bd"  # Purple

# Calculate the midpoint color between M and Z
midpoint_color <- "#4b93e1"  # This is approximately the midpoint color

# Add LinkColour to edges_df based on source_type
edges_df$LinkColour <- ifelse(edges_df$source_type == "M", color_M, 
                              ifelse(edges_df$source_type == "Z", color_Z, 
                                     ifelse(edges_df$source_type == "C", midpoint_color, 
                                            ifelse(edges_df$source_type == "O", "#999999", "#333333"))))
    
    
    
    # Define a fixed color mapping for node types
    node_type_colors <- c(
      "celltype/state" = "#e91e63",  
      "gene/protein" = "#4caf50",    
      "pathway" = "#ff7f0e"         
      # Add more node types and their corresponding colors here
    )
    
    # Ensure 'nodes' dataframe has accurate 'node_type' information
    nodes$color <- node_type_colors[nodes$node_type]
    
    # Ensure 'vertices_df' gets the updated color information
    vertices_df$color <- node_type_colors[vertices_df$node_type]
    
    # Add a node size column
    vertices_df$nodesize <- ifelse(vertices_df$node_type == "celltype/state", 12, 8)  # Make celltype/state nodes larger
    
    # Creating simplified_name column that extracts part after the underscore
    vertices_df$simplified_name <- sapply(vertices_df$name, function(x) {
      parts <- strsplit(x, "_", fixed = TRUE)[[1]]
      if (length(parts) > 1) {
        return(parts[length(parts)])  # Take the part after the last underscore
      } else {
        return(x)
      }
    })
    
    # Add edge labels
    edges_df$label <- edges_df$relation
    
    # Use forceNetwork to create the network visualization
    # Create the network visualization
    network <- forceNetwork(
      Links = edges_df, Nodes = vertices_df,
      Source = "source", Target = "target",
      NodeID = "simplified_name", Group = "color", opacity = 0.8, zoom = TRUE, bounded = TRUE,
      colourScale = JS("d3.scaleOrdinal().domain(['#e91e63', '#4caf50', '#ff7f0e']).range(['#e91e63', '#4caf50', '#ff7f0e'])"),
      linkColour = edges_df$LinkColour,
      fontSize = 20,
      fontFamily = "Arial",
      Nodesize = "nodesize",
      
      # Increase the size of the graph
      charge = -150,  # More negative value spreads nodes out more
      linkDistance = 50,  # Increase this to spread linked nodes further apart
      
      radiusCalculation = JS("d.nodesize")  # Use the nodesize column to set node radius
    )
    
    # Use onRender to customize the node text appearance
    network <- htmlwidgets::onRender(network, '
  function(el, x) {
    // Select all text elements
    var texts = d3.selectAll(".node text")
      .style("fill", "black")
      .style("font-weight", "bold")
      .style("font-size", "16px");
      
    // Add edge labels
    var links = d3.selectAll(".link")
      .append("title")
      .text(function(d) { return d.label; });
      
    // Make the SVG fill its container
    d3.select("svg")
      .attr("width", "100%")
      .attr("height", "100%");

  }
')
    # Render the network visualization
    network 
    
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
      #"Number of Communities Detected:", communities_detected, "<br>",
      # "Average Path Length:", round(avg_path_length, 4), "<br>",
      # "Diameter:", graph_diameter, "<br>",
      sep = ""
    )
    
    # Format the statistics into a string with HTML markup
    stats_html <- paste0(
      "<div style='padding: 20px; margin-bottom: 10px;'>",
      "<div style='color: #000000; background-color: #F8FBFF; padding: 20px; margin: 20px; border-radius: 5px; margin-bottom: 20px;'>",
      "<p style='font-size: 14px; font-weight: bold; margin-top: 10px; margin-bottom: 10px;'>Legend:</p>",
      "<p style='font-size: 12px;'><span style='height: 15px; width: 15px; background-color: #e91e63; border-radius: 40%; display: inline-block;'></span> Node: Celltype/State<span style='display: inline-block; width: 42px;'></span><span style='height: 15px; width: 15px; background-color: #999999; border-radius: 40%; display: inline-block;'></span> Edge (PPI): PrimeKG</p>",
      "<p style='font-size: 12px;'><span style='height: 15px; width: 15px; background-color: #4caf50; border-radius: 40%; display: inline-block;'></span> Node: Genes/Protein<span style='display: inline-block; width: 40px;'></span><span style='height: 15px; width: 15px; background-color: #00c8ff; border-radius: 40%; display: inline-block;'></span> Edge: Mathys</p>",
      "<p style='font-size: 12px;'><span style='height: 15px; width: 15px; background-color: #ff7f0e; border-radius: 40%; display: inline-block;'></span> Node: Pathway<span style='display: inline-block; width: 73px;'></span><span style='height: 15px; width: 15px; background-color: #9f67bd; border-radius: 40%; display: inline-block;'></span> Edge: Zhou</p>",
      "<p style='font-size: 14px; font-weight: bold; margin-top: 30px; margin-bottom: 10px;'>Graph Details from igraph:</p>",
      "<p style='font-size: 12px; margin-top: 0; margin-bottom: 5px;'>Number of Nodes: ", num_nodes, "<span style='display: inline-block; width: 20px;'></span>",
      "Number of Edges: ", num_edges, "</p>",
      "<p style='font-size: 12px; margin-top: 0; margin-bottom: 5px;'>Graph Density: ", round(graph_density, 4), "<span style='display: inline-block; width: 20px;'></span>",
      "Average Degree Centrality: ", round(avg_degree_centrality, 4), "</p>",
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

