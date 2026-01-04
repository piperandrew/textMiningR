###########################################
#### Traveling Salesman Visualization #####
###########################################

# ============================================================================
# Traveling Salesman Problem (TSP) Simulation for Narrative Trajectory Analysis
# ============================================================================
# This code demonstrates the concept of narrative linearity by comparing
# an actual sequential path through narrative segments (representing the order
# segments appear in a text) with the shortest possible path through the same
# points in semantic space. The greater the difference between these paths,
# the more nonlinear the narrative structure.
#
# The visualization creates two side-by-side plots:
# 1. Actual Path: segments visited in sequential order (1→2→3→4→5→6→7)
# 2. Shortest Path: optimal TSP solution minimizing total distance

# See: Piper, A., & Toubia, O. (2023). A quantitative study of non-linearity 
#in storytelling. Poetics, 98, 101793.

# ============================================================================

library(ggplot2)   # For creating plots
library(gtools)    # General tools for data manipulation
library(TSP)       # For solving the Traveling Salesman Problem
library(gridExtra) # For arranging multiple plots side-by-side

# Generate simulated data: 7 random points in 2D semantic space
# Each point represents a narrative segment's position in embedding space
# runif(7) generates 7 random numbers between 0 and 1
etsp <- ETSP(data.frame(x = runif(7), y = runif(7)))

# Solve the TSP to find the shortest route visiting all points exactly once
# start = 1 specifies that the route should begin at point 1
tour <- solve_TSP(etsp, start = 1)

# ============================================================================
# PLOT 1: SHORTEST PATH (Optimal TSP Solution)
# ============================================================================

# Create data frame from the original coordinates
etsp_df <- data.frame(x = etsp[,1], y = etsp[,2])

# Rearrange points according to the optimal tour order found by TSP solver
# This reorders the rows so they follow the shortest path sequence
tour_order <- tour
etsp_df <- etsp_df[tour_order, ]

# Calculate total Euclidean distance of the optimal tour
# tour_length() sums all segment distances in the optimal path
dist1 <- round(tour_length(tour), digits=2)

# Create plot showing the shortest possible path through all points
p1 <- ggplot() +
  theme_classic() +
  # Draw dotted line connecting points in optimal order
  geom_path(data = etsp_df, aes(x, y), color = "darkgrey", linetype = "dotted") +
  # Label each point with its position in the optimal tour sequence
  geom_text(data = etsp_df, aes(x, y, label = tour_order), hjust=0, vjust = 0) +
  # Add title and distance subtitle
  labs(title = "Shortest Path", subtitle = paste("Distance = ", dist1, sep="")) +
  xlab("Semantic Dimension 1") +
  ylab("Semantic Dimension 2")

# ============================================================================
# PLOT 2: ACTUAL PATH (Sequential Order)
# ============================================================================

# Create second data frame with same original coordinates
etsp_df2 <- data.frame(x = etsp[,1], y = etsp[,2])

# Create sequential order (1, 2, 3, 4, 5, 6, 7)
# This represents visiting segments in the order they appear in the narrative
tour_order2 <- seq(1:7)
etsp_df2 <- etsp_df2[tour_order2, ]

# Calculate distance for sequential path
# dist() creates a distance matrix; [1:6] gets distances between consecutive points
# sum() adds these consecutive distances to get total path length
dist2 <- round(sum(dist(etsp_df2)[1:6]), digits=2)

# Create plot showing the actual sequential narrative path
p2 <- ggplot() +
  theme_classic() +
  # Draw dotted line connecting points in sequential order (1→2→3→4→5→6→7)
  geom_path(data = etsp_df2, aes(x, y), color = "darkgrey", linetype = "dotted") +
  # Label each point with its sequential position
  geom_text(data = etsp_df2, aes(x, y, label = tour_order2), hjust=0, vjust = 0) +
  # Add title and distance subtitle
  labs(title = "Actual Path", subtitle = paste("Distance = ", dist2, sep="")) +
  xlab("Semantic Dimension 1") +
  ylab("Semantic Dimension 2")

# ============================================================================
# DISPLAY RESULTS
# ============================================================================

# Arrange both plots side-by-side for comparison
# Actual Path (left) vs Shortest Path (right)
# The difference in distances quantifies narrative nonlinearity
grid.arrange(p2, p1, ncol = 2)


