options(shiny.fullstacktrace = TRUE)
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
# Read PrimeKG Data
# -----------------
nodes <- tryCatch(read.csv('nodes_filtered.csv'), error = function(e) { message("Error reading nodes_filtered.csv: ", e$message); NULL })
kgrawo <- tryCatch(read.csv('kg_raw_orig_filtered.csv'), error = function(e) { message("Error reading kg_raw_orig_filtered.csv: ", e$message); NULL })
if (!is.null(kgrawo)) kgrawo$source_type <- "ORI"

read_and_add_source <- function(file_name, source_type) {
  tryCatch({
    data <- read.csv(file_name)
    data$source_type <- source_type
    message(paste("Successfully read:", file_name))
    data
  }, error = function(e) {
    message(paste("Error reading", file_name, ":", e$message))
    NULL
  })
}

Mathys_data <- read_and_add_source("Mathys_ast_final.csv", "Mat")
Zhou_data <- read_and_add_source("Zhou_ast_final.csv", "Zho")
Cobos_data <- read_and_add_source("Cobos_Ast_final.csv", "Cob")
Sziraki_data <- read_and_add_source("Sziraki_Ast_final.csv", "Szi")

datasets <- list(
  "Mathys" = Mathys_data,
  "Zhou" = Zhou_data,
  "Cobos" = Cobos_data,
  "Sziraki" = Sziraki_data
)

# Print summary of loaded datasets
message("\nSummary of loaded datasets:")
for (dataset in names(datasets)) {
  if (!is.null(datasets[[dataset]])) {
    message(paste(dataset, "- Rows:", nrow(datasets[[dataset]]), "Columns:", ncol(datasets[[dataset]])))
  } else {
    message(paste(dataset, "- Not loaded"))
  }
}


# Pre-compute gene sets for each dataset
gene_sets <- lapply(datasets, function(data) {
  genes <- data %>% filter(x_type == "gene/protein" | y_type == "gene/protein")
  unique(c(genes$x_name, genes$y_name))
})

# Define colors for source types
color_Mat <- "#00c8ff"  # Light Blue (Mathys)
color_Zho <- "#9f67bd"  # Purple (Zhou)
color_Cob <- "#ff6c00"  # Orange (Cobos)
color_Szi <- "#d4af37"  # Green (Sziraki)
color_COM <- "#4b93e1"  # Midpoint color for common genes
color_ORI <- "#999999"  # Gray (Original dataset)
color_DEFAULT <- "#CCCCCC"  # Default color (light gray)

# Create a named vector of colors based on the source types
source_colors <- c(
  "Mat" = color_Mat,
  "Zho" = color_Zho,
  "Cob" = color_Cob,
  "Szi" = color_Szi,
  "COM" = color_COM,
  "ORI" = color_ORI
)

# Define a fixed color mapping for node types
node_type_colors <- c(
  "celltype/state" = "#e91e63",  
  "gene/protein" = "#4caf50",    
  "pathway" = "#ff7f0e"         
  # Add more node types and their corresponding colors here
)


# Set color palette
num_levels <- 10
blue_palette <- colorRampPalette(c("lightblue", "darkblue"))(num_levels)


# -----------------
# Define App Aesthetics
# -----------------
ui <- fluidPage(
  tags$head(
    tags$title("AlzKG Graph Visualization Tool"),  # Add this line for the tab title
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
      div(
        style = "margin-bottom: 25px;",
        checkboxGroupInput(
          "datasets",
          "Select Datasets:",
          choiceNames = list(
            tags$span(style = "font-weight: bold; font-size: 14px; color: #FFFFFF; background-color: #333333; padding: 4px 8px; border-radius: 4px;", "Mathys"),
            tags$span(style = "font-weight: bold; font-size: 14px; color: #FFFFFF; background-color: #333333; padding: 4px 8px; border-radius: 4px;", "Zhou"),
            tags$span(style = "font-weight: bold; font-size: 14px; color: #FFFFFF; background-color: #333333; padding: 4px 8px; border-radius: 4px;", "Cobos"),
            tags$span(style = "font-weight: bold; font-size: 14px; color: #FFFFFF; background-color: #333333; padding: 4px 8px; border-radius: 4px;", "Sziraki")
          ),
          choiceValues = c("Mathys", "Zhou", "Cobos", "Sziraki"),
          selected = c("Mathys", "Zhou", "Cobos", "Sziraki")
        )
      ),
      
      # Focal node for visualization selection
      div(style = "margin-bottom: 25px;",
          selectInput("nodeInterest1", 
                      label = "Select Central Node:",
                      choices = c("Ast" = "c3_celltype/state_Ast", "SOX2"="6657_gene/protein_SOX2"),
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
    selected_datasets <- input$datasets
    
    if (length(selected_datasets) == 0) {
      combined <- kgrawo
    } else {
      combined <- do.call(rbind, c(list(kgrawo), datasets[selected_datasets]))
    }
    
    if (input$commonEdges == "commonedges" && length(selected_datasets) > 1) {
      # Find common genes across selected datasets
      common_gene_set <- Reduce(intersect, gene_sets[selected_datasets])
      
      # Filter for common edges
      common_edges <- combined %>%
        filter((x_type == "gene/protein" & x_name %in% common_gene_set) |
                 (y_type == "gene/protein" & y_name %in% common_gene_set))
      
      common_edges$source_type <- "COM"
      
      # Combine with original data (kgrawo) and remove duplicate common edges
      combined <- bind_rows(kgrawo, common_edges)
      combined <- distinct(combined)
    }
    
    # Add source column if it doesn't exist
    if (!"source" %in% names(combined)) {
      combined$source <- "ORI"  # Original dataset
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
    if (!input$nodeInterest1 %in% V(g)$name) {
      stop("Node of interest does not exist in the graph.")
    }
    
    # Find the vertex of the node of interest
    node_vertex <- V(g)[V(g)$name == input$nodeInterest1]
    
    
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
    
    
    # Add LinkColour to edges_df based on source_type, with default color for unknown source types
    edges_df$LinkColour <- ifelse(
      edges_df$source_type %in% names(source_colors),
      source_colors[edges_df$source_type],
      color_DEFAULT
    )
    
    # Ensure 'nodes' dataframe has accurate 'node_type' information
    nodes$color <- node_type_colors[nodes$node_type]
    
    # Ensure 'vertices_df' gets the updated color information
    vertices_df$color <- node_type_colors[vertices_df$node_type]
    
    # Add a node size column
    vertices_df$nodesize <- ifelse(vertices_df$node_type == "celltype/state", 12, 8)  # Make celltype/state nodes larger
    # Set the size of the central node to be larger
    vertices_df$nodesize[vertices_df$name == input$nodeInterest] <- 12  # Adjust the size as needed
    # Set the size of the central node (node of interest) to be 12
    vertices_df$nodesize[vertices_df$name == input$nodeInterest1] <- 12
    
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
    avg_degree_centrality <- mean(degree(g))
    
    # Create data frames for node types and edge types
    node_types <- data.frame(
      type = names(node_type_colors),
      color = node_type_colors
    )
    
    edge_types <- data.frame(
      type = names(source_colors),
      color = source_colors
    )
    
    # Format the statistics into a string with HTML markup
    stats_html <- paste0(
      "<div style='padding: 20px; background-color: #FFFFFF; margin-bottom: 10px;'>",
      "<div style='color: #000000; background-color: #F8FBFF; padding: 20px; margin: 20px; border-radius: 5px; margin-bottom: 20px;'>",
      "<div style='display: flex;'>",
      "<div style='flex: 1;'>",
      "<p style='font-weight: bold; margin-bottom: 10px;'>Nodes:</p>",
      paste0(sapply(1:nrow(node_types), function(i) {
        paste0(
          "<div style='display: flex; align-items: center; margin-bottom: 5px;'>",
          "<span style='width: 12px; height: 12px; background-color: ", node_types$color[i], "; display: inline-block; margin-right: 10px; border-radius: 50%;'></span>",
          "<span>", node_types$type[i], "</span>",
          "</div>"
        )
      }), collapse = ""),
      "</div>",
      "<div style='flex: 2;'>",
      "<p style='font-weight: bold; margin-bottom: 10px;'>Edges:</p>",
      "<div style='display: flex; flex-wrap: wrap;'>",
      paste0(sapply(1:nrow(edge_types), function(i) {
        paste0(
          "<div style='flex: 50%; display: flex; align-items: center; margin-bottom: 5px;'>",
          "<span style='width: 20px; height: 3px; background-color: ", edge_types$color[i], "; display: inline-block; margin-right: 10px;'></span>",
          "<span>", edge_types$type[i], "</span>",
          "</div>"
        )
      }), collapse = ""),
      "</div>",
      "</div>",
      "</div>",
      "<p style='font-size: 14px; font-weight: bold; margin-top: 30px; margin-bottom: 10px;'>Graph Details from igraph:</p>",
      "<p style='font-size: 12px; margin-top: 0; margin-bottom: 5px;'>Number of Nodes: ", num_nodes, "<span style='display: inline-block; width: 20px;'></span>",
      "Number of Edges: ", num_edges, "</p>",
      "<p style='font-size: 12px; margin-top: 0; margin-bottom: 5px;'>Graph Density: ", round(graph_density, 4), "<span style='display: inline-block; width: 20px;'></span>",
      "Average Degree Centrality: ", round(avg_degree_centrality, 4), "</p>",
      "</div>",
      "</div>"
    )
    
    # Return the formatted HTML for rendering in the UI
    HTML(stats_html)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
