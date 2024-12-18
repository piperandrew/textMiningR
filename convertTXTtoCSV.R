## Transform directory of .txt files into .csv
# Function to read .txt files from a directory into a data frame

read_txt_to_dataframe <- function(directory) {
  # Get a list of all .txt files in the directory
  txt_files <- list.files(path = directory, pattern = "\\.txt$", full.names = TRUE)
  
  # Initialize an empty data frame
  data <- data.frame(filename = character(), text = character(), stringsAsFactors = FALSE)
  
  # Loop through each file and read its contents
  for (file in txt_files) {
    filename <- basename(file)
    text <- paste(readLines(file, warn = FALSE), collapse = "\n")
    data <- rbind(data, data.frame(filename = filename, text = text, stringsAsFactors = FALSE))
  }
  
  return(data)
}

# Example usage
# Set your directory path
directory_path <- ""
df <- read_txt_to_dataframe(directory_path)

# View the resulting data frame
print(df)
