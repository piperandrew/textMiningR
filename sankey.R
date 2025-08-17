#Takes as input 
# hc object
# keyword + frequency list = b


#### Make Sankey Diagram for all k ####

library(networkD3)
library(dplyr)
library(RColorBrewer)

create_cluster_sankey <- function(hc, k_range = 2:7) {
  # Validate inputs
  if (!inherits(hc, "hclust")) {
    stop("hc must be a hierarchical clustering object")
  }
  if (min(k_range) < 2) {
    stop("k must be at least 2")
  }
  
  # Get cluster assignments for each k
  cluster_assignments <- lapply(k_range, function(k) {
    cutree(hc, k = k)
  })
  
  # Create source-target pairs
  links <- data.frame()
  for (i in 1:(length(k_range)-1)) {
    current_k <- cluster_assignments[[i]]
    next_k <- cluster_assignments[[i+1]]
    
    # Create flow counts between clusters
    flow_table <- table(
      paste0("K", k_range[i], "_C", current_k),
      paste0("K", k_range[i+1], "_C", next_k)
    )
    
    # Convert to data frame
    flows <- as.data.frame(as.table(flow_table))
    colnames(flows) <- c("source", "target", "value")
    
    # Filter out zero flows
    flows <- flows[flows$value > 0, ]
    
    links <- rbind(links, flows)
  }
  
  # Create nodes dataframe
  all_names <- unique(c(as.character(links$source), 
                        as.character(links$target)))
  nodes <- data.frame(
    name = all_names,
    stringsAsFactors = FALSE
  )
  
  # Convert source and target to 0-based index
  links$source <- match(links$source, nodes$name) - 1
  links$target <- match(links$target, nodes$name) - 1
  
  # Create Sankey diagram
  sankeyNetwork(
    Links = links, 
    Nodes = nodes,
    Source = "source",
    Target = "target",
    Value = "value",
    NodeID = "name",
    #NodeGroup = "keywords",
    fontSize = 16,
    #nodeWidth = 30,
    fontFamily = "Arial",
    sinksRight = TRUE,
    colourScale = JS("d3.scaleOrdinal().range(nodes.map(d => d.color))")
    #colourScale = JS(color_scale_js),
    # colourScale = JS(
    #   sprintf(
    #     "d3.scaleOrdinal().domain(%s).range(%s)",
    #     jsonlite::toJSON(unique_labels),
    #     jsonlite::toJSON(node_colors)
    #   )
    # )
  )
}
sankey_plot <- create_cluster_sankey(hc, k_range = 2:7)

#subset frequency table by glove table
b<-a[a$word %in% row.names(emb),]

#Modify node names with keywords
get_cluster_info <- function(k_range) {
  # Get cluster assignments for each k
  cluster_assignments <- lapply(k_range, function(k) {
    cutree(hc, k = k)
  })
  
  # Process each k value and cluster
  results <- list()
  for(i in seq_along(k_range)) {
    k <- k_range[i]
    for(c in 1:k) {
      cluster_words <- names(cluster_assignments[[i]][cluster_assignments[[i]] == c])
      moral.df.sub <- b[b$word %in% cluster_words,]
      top3 <- moral.df.sub[order(-moral.df.sub$freq),][1:3,]
      keywords <- paste(top3$word, collapse=", ")
      results[[paste0("k", k, "_c", c)]] <- data.frame(
        keywords = keywords,
        avg_freq = mean(top3$freq)
      )
    }
  }
  return(do.call(rbind, results))
}

results<-get_cluster_info(2:7)

# Convert node names to lowercase to match results
label_lookup <- setNames(results$keywords, tolower(rownames(results)))
sankey_plot$x$nodes$name <- sapply(tolower(sankey_plot$x$nodes$name), function(node) {
  k <- substr(node, 2, 2)  # Extract k number
  paste0("C", k, " ", label_lookup[node])
})

sankey_plot$x$links$source_freq <- results$avg_freq[sankey_plot$x$links$source + 1]
sankey_plot$x$links$target_freq <- results$avg_freq[sankey_plot$x$links$target + 1]
sankey_plot$x$links$value <- (sankey_plot$x$links$source_freq + sankey_plot$x$links$target_freq) / 2

#set colors
# Extract base names by removing the cluster prefix (e.g. "C2 ", "C3 ")
base_names <- sub("^C[2-7] ", "", sankey_plot$x$nodes$name)

# Create color mapping for unique base names
unique_base_names <- unique(base_names)
base_colors <- setNames(
  colorRampPalette(brewer.pal(9, "Set1"))(length(unique_base_names)),
  unique_base_names
)

# Apply colors using base names
sankey_plot$x$nodes$color <- base_colors[base_names]

#Split strings for labels
sankey_plot <- htmlwidgets::onRender(
  sankey_plot,
  'function(el,x) {
   d3.select(el)
       .selectAll(".node text")
       .each(function() {
           var text = d3.select(this);
           var fullText = text.text();
           var keywords = fullText.split(",");
           
           text.text("");
           
           keywords.forEach(function(keyword, i) {
               text.append("tspan")
                   .attr("x", text.attr("x"))
                   .attr("dy", i === 0 ? "0em" : "1.2em")
                   .text(keyword.trim());
           });
       });
}'
)
#library(webshot2)
saveNetwork(sankey_plot, "temp.html", selfcontained = TRUE)
webshot("temp.html", "sankey_diagram.png", delay = 0.5)


