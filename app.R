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
library(shinydashboard)
library(tidyr)



# -----------------
# Read PrimeKG Data
# -----------------
read_and_add_source <- function(file_name, source_type) {
  tryCatch({
    data <- read.csv(file_name)
    data$source_type <- source_type
    
    # Remove "Mathys " prefix if it exists in the relation column - temp code
    data$relation <- sub("^Mathys ", "", data$relation)
    
    # Combines relation and source_type into relation_info directly
    data$relation_info <- paste(source_type, data$relation, sep=" - ")
    
    # Decides which gene/protein to associate with the relation_info
    data$gene <- ifelse(data$x_type == "gene/protein", data$x_name, data$y_name)
    
    message(paste("Successfully read:", file_name))
    data
  }, error = function(e) {
    message(paste("Error reading", file_name, ":", e$message))
    NULL
  })
}


kgrawo <- read_and_add_source("kg_raw_orig_filtered.csv", "ORI")
Mathys_data <- read_and_add_source("Mathys_ast_final.csv", "Mathys et al. (2019)")
Zhou_data <- read_and_add_source("Zhou_ast_final.csv", "Zhou et al. (2020)")
Cobos_data <- read_and_add_source("Cobos_Ast_final.csv", "Cobos et al. (2022)")
Sziraki_data <- read_and_add_source("Sziraki_Ast_final.csv", "Sziraki et al. (2023)")

# Filter to keep only nodes of type "gene"
kgrawo <- kgrawo[kgrawo$x_type == "gene/protein" & kgrawo$y_type == "gene/protein", ]

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
  genes <- data %>% filter(x_type == "gene/protein")
  unique(c(genes$x_name))
})

# Define colors for source types
color_Mat <- "#00c8ff"  # Light Blue (Mathys)
color_Zho <- "#9f67bd"  # Purple (Zhou)
color_Cob <- "#ff6c00"  # Orange (Cobos)
color_Szi <- "#d4af37"  # Green (Sziraki)
color_COM <- "#999999"  # Midpoint color for common genes
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


ui <- dashboardPage(
  dashboardHeader(title = "AlzKG Graph"),
  dashboardSidebar(
    tags$style(HTML("
      .sidebar-menu { padding-top: 20px; }  /* Add space before first input */
      .sidebar-toggle:hover { background-color: #0056B3 !important; }  /* Change hamburger menu background on hover */
    ")),
    sidebarMenu(
      selectInput("nodeInterest", "Select Celltype or Subtype:",
                  choices = c("Astrocytes" = "c3_celltype/state_Ast"),
                  selected = "c3_celltype/state_Ast"),
      selectInput("nodeInterest1", "Select Central Node:",
                  choices = c("GFAP" = "2670_gene/protein_GFAP", "SOX2" = "6657_gene/protein_SOX2"),
                  selected = "2670_gene/protein_GFAP"),
      selectInput("commonEdges", "Gene Display:",
                  choices = c("All Genes" = "alledges", "Common Genes" = "commonedges"),
                  selected = "alledges"),
      numericInput("numNodesDisplay", "Total #Nodes to Display:",
                   value = 100, min = 1),
      checkboxGroupInput("datasets", "Select Datasets:", 
                         choiceNames = c("Mathys et al. (2019)", "Zhou et al. (2020)", "Cobos et al. (2022)", "Sziraki et al. (2023)"),
                         choiceValues = list(
                           "Mathys", "Zhou", "Cobos", "Sziraki"
                         ),
                         selected = c("Mathys", "Zhou")),
      # Add legends below the input controls
      div(
        style = "padding: 10px; background-color: #dedede; color: #000000; margin-top: 90px; border-radius: 5px; border: 1px solid #ccc; width: 94%; margin-left: 3%;",
        HTML(
          "<div style='color: #000000; padding: 10px; border-radius: 5px;'>",
          "<p style='font-weight: bold; margin-bottom: 5px;'>Nodes:</p>",
          "<div style='display: flex; align-items: center; justify-content: flex-start;'>",
          "<span style='width: 12px; height: 12px; background-color: #4caf50; display: inline-block; margin-right: 10px; border-radius: 50%;'></span>",
          "<span style='line-height: 12px;'>Genes</span>",
          "</div>",
          "<p style='font-weight: bold; margin-bottom: 5px; margin-top: 10px;'>Edges:</p>",
          "<div style='display: flex; align-items: center; justify-content: flex-start;'>",
          "<span style='width: 20px; height: 3px; background-color: grey; display: inline-block; margin-right: 10px;'></span>",
          "<span style='line-height: 10px;'>Protein-Protein Interaction</span>",
          "</div>"
        )
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML('
        /* Custom CSS for gradient header background */
        .main-header .logo {
          background: #000000 !important;
        }
        .main-header .navbar {
          background: linear-gradient(90deg, #000000, #0056B3 ) !important;
        }
        /* Custom CSS for full-size graph */
        .content-wrapper, .right-side {
          background-color: #FFFFFF;
        }
        /* Improve sidebar aesthetics */
        .skin-blue .main-sidebar {
          background-color: #0c0c0c;
        }
        .skin-blue .sidebar a {
          color: #b8c7ce;
        }
        .skin-blue .sidebar-menu>li.active>a, .skin-blue .sidebar-menu>li:hover>a {
          color: #fff;
          background: #1e282c;
        }
      '))
    ),
    fluidRow(
      column(12, 
             div(style = "height: 800px;", # Adjust this value as needed
                 forceNetworkOutput("networkPlot", height = "100%", width = "100%")
             )
      ),
      column(12, 
             div(id = "node-info", style = "margin-top: 20px; padding: 10px; border: 1px solid #ddd; border-radius: 5px;")
      )
    )
  )
)


server <- function(input, output, session) {
  
  observeEvent(input$toggle, {
    # Toggle the display property of the div containing inputs
    shinyjs::toggle(id = "input-container")
  })
  
  data <- reactive({
    selected_datasets <- input$datasets
    
    if (length(selected_datasets) == 0) {
      showNotification("At least one dataset needs to be selected", type = "error")
      return(NULL)
    }
    
    # Get genes from selected datasets
    selected_genes <- unique(unlist(gene_sets[selected_datasets]))
    
    # Find common genes across all selected datasets
    if (length(selected_datasets) > 1) {
      common_genes <- gene_sets[[selected_datasets[1]]]
      for (dataset in selected_datasets[-1]) {
        common_genes <- intersect(common_genes, gene_sets[[dataset]])
      }
    } else {
      common_genes <- gene_sets[[selected_datasets[1]]]
    }
    # Combine and process relation information from datasets
    all_relations <- do.call(rbind, datasets[selected_datasets]) %>%
      filter(x_type == "gene/protein" | y_type == "gene/protein") %>%
      select(gene, relation_info, source_type) %>%
      distinct()
    
    # Group by gene and combine relations
    relation_info <- all_relations %>%
      group_by(gene) %>%
      summarize(relations = paste(unique(relation_info), collapse = "<br>")) %>%
      distinct()
    
  
    # Filter kgrawo based on selected datasets
    combined <- kgrawo %>%
      filter(x_name %in% selected_genes & y_name %in% selected_genes)
    
    # Join relations to combined dataframe for x_name and y_name
    combined <- combined %>%
      left_join(relation_info, by = c("x_name" = "gene")) %>%
      rename(x_relations = relations) %>%
      left_join(relation_info, by = c("y_name" = "gene")) %>%
      rename(y_relations = relations)
    
    # Ensure no relations found is handled
    combined <- combined %>%
      mutate(x_relations = ifelse(is.na(x_relations), "No relations found", x_relations),
             y_relations = ifelse(is.na(y_relations), "No relations found", y_relations))
    
    
    if (input$commonEdges == "commonedges" && length(selected_datasets) > 1) {
      # Further filter for common edges
      combined <- combined %>%
        filter((x_type == "gene/protein" & x_name %in% common_genes) &
                 (y_type == "gene/protein" & y_name %in% common_genes))
      
      combined$source_type <- "COM"  # Mark source type as common
    } else {
      combined$source_type <- "ORI"  # Original dataset
    }
    
    list(
      data = combined, 
      selected_genes = selected_genes, 
      common_genes = common_genes,
      commonEdges = input$commonEdges
    )
  })
  
  output$networkPlot <- renderForceNetwork ({
    result <- data()
    if (is.null(result)) return(NULL)
    
    fdg <- result$data
    selected_genes <- result$selected_genes
    common_genes <- result$common_genes
    commonEdges <- result$commonEdges
    
    # Convert all IDs to character type
    fdg$x_id <- as.character(fdg$x_id)
    fdg$x_type <- as.character(fdg$x_type)
    fdg$x_name <- as.character(fdg$x_name)
    fdg$y_id <- as.character(fdg$y_id)
    fdg$y_type <- as.character(fdg$y_type)
    fdg$y_name <- as.character(fdg$y_name)
    
    # Create unique IDs
    fdg$x_unique_id <- paste(fdg$x_id, fdg$x_type, fdg$x_name, sep = "_")
    fdg$y_unique_id <- paste(fdg$y_id, fdg$y_type, fdg$y_name, sep = "_")
    
    # Remove potential whitespace
    fdg$x_unique_id <- trimws(fdg$x_unique_id)
    fdg$y_unique_id <- trimws(fdg$y_unique_id)
    
    # Create edges data frame
    edges <- data.frame(from = fdg$x_unique_id, to = fdg$y_unique_id, source_type = fdg$source_type)
    
    # Create nodes data frame
    nodes_x <- data.frame(
      unique_id = fdg$x_unique_id,
      node_id = fdg$x_id,
      node_type = fdg$x_type,
      node_name = fdg$x_name,
      relations = fdg$x_relations
    )
    
    nodes_y <- data.frame(
      unique_id = fdg$y_unique_id,
      node_id = fdg$y_id,
      node_type = fdg$y_type,
      node_name = fdg$y_name,
      relations = fdg$y_relations
    )
    
    nodes <- unique(rbind(nodes_x, nodes_y))
    # Add relations to nodes based on node_name
    
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
    
    subgraph_nodes <- V(sub_g)$name
    subgraph_genes <- unique(sapply(strsplit(subgraph_nodes, "_"), function(x) x[length(x)]))
    
    unexpected_genes <- setdiff(subgraph_genes, selected_genes)
    if (length(unexpected_genes) > 0) {
      print("Unexpected genes in subgraph:")
      print(unexpected_genes)
    }
    unexpected_genes_c <- setdiff(subgraph_genes, common_genes)
    if (length(unexpected_genes_c) > 0 && commonEdges == "commonedges") {
      print("Unexpected genes in subgraph with common nodes:")
      print(unexpected_genes_c)
    }
    
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
    vertices_df$nodesize <- ifelse(vertices_df$node_type == "celltype/state", 18, 10)  # Make celltype/state nodes larger
    
    # Add a font size column
    vertices_df$fontsize <- 10  # Default font size
    
    # Set the size and font size of the central node to be larger
    vertices_df$nodesize[vertices_df$name == input$nodeInterest] <- 18  # Adjust the size as needed
    vertices_df$fontsize[vertices_df$name == input$nodeInterest] <- 14  # Adjust the font size as needed
    
    # Set the size and font size of the node of interest to be larger
    vertices_df$nodesize[vertices_df$name == input$nodeInterest1] <- 18
    vertices_df$fontsize[vertices_df$name == input$nodeInterest1] <- 14
    
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
      NodeID = "simplified_name", Group = "relations", opacity = 0.8, zoom = TRUE, bounded = TRUE,
      colourScale = JS("d3.scaleOrdinal().range(['#4caf50'])"),
      linkColour = edges_df$LinkColour,
      fontSize = 10,  # Set a small font size for all labels
      fontFamily = "Arial",
      Nodesize = "nodesize",
      charge = -150,
      linkDistance = 50,
      opacityNoHover = 1,
      radiusCalculation = JS("d.nodesize"),
    )
    
    # Use onRender to customize the node text appearance
    network <- htmlwidgets::onRender(network, '
function(el, x) {
    var svg = d3.select(el).select("svg");
    var nodes = svg.selectAll(".node");
    var links = svg.selectAll(".link");
    
    // Disable default hover behavior
    nodes.on("mouseover", null).on("mouseout", null);
    
    // Select all text elements
    var texts = svg.selectAll(".node text")
      .style("fill", "black")
      .style("font-weight", "bold")
      .style("font-size", function(d) { return d.fontsize + "px"; });
    
    // Click functionality
    nodes.on("click", function(d) {
      console.log("Clicked node:", d);  // Log the clicked node data
      
      // Display node info
     d3.select("#node-info")
        .html("<div style=\'font-size: 16px;\'>" +
              "<strong style=\'font-size: 18px;\'>Selected Gene:</strong> " + "<br>" + d.name + 
              "<br><strong style=\'font-size: 18px;\'>Source:</strong> " + "<br>" + (d.group) +
              "</div>");
      
      // Highlight clicked node and connected nodes/links
      nodes.style("fill", "#4caf50") // default color
       .style("opacity", 0.3);
      links.style("opacity", 0.2);
      
      // Highlight clicked node by changing its color and opacity
     d3.select(this)
      .style("fill", "#2e2e2e") // color of the clicked node
      .style("opacity", 1);
      
      links.filter(function(l) {
        return l.source.name === d.name || l.target.name === d.name;
      }).style("opacity", 1);
      
      nodes.filter(function(n) {
        return n.name === d.name || links.filter(function(l) {
          return (l.source.name === d.name && l.target.name === n.name) ||
                 (l.target.name === d.name && l.source.name === n.name);
        }).size() > 0;
      }).style("opacity", 1);
    });
    
      
    // Make the SVG fill its container
    svg.attr("width", "100%")
       .attr("height", "100%");
  }
')
    # Render the network visualization
    network 
    
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)


