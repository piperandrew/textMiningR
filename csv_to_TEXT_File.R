#################################################################
######################## CSV to TXT FILE ########################
#################################################################

# Load the dataframe
df <- read.csv("Validation_BookSum.csv")

# Specify which column to export
column_name <- "text"

# Specify output directory
output_dir <- "Validation_BookSum_TEXT"

# Create unique filename column (if necessary)
generate_id <- function(n = 8) {
  chars <- c(0:9, letters, LETTERS)
  paste(sample(chars, n, replace = TRUE), collapse = "")
}
df$id <- sapply(1:nrow(df), function(x) generate_id(8))

# Use a specific column for filenames
filename_col <- "id"
filename_col <- "bid"

for (i in 1:nrow(df)) {
  filename <- paste0(output_dir, "/", df[[filename_col]][i], ".txt")
  writeLines(df[[column_name]][i], filename)
  cat("Created:", filename, "\n")
}
