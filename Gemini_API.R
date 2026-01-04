#############################################################
######################## Gemini API #########################
#############################################################

# This R function provides an interface to Google's 
#Gemini AI API for processing text data. It accepts two input formats:
#individual .txt files (from a list of file paths) or text data 
#from a CSV column. The function sends each piece of text to Gemini 
#using a customizable prompt template, then returns the AI responses 
#in a structured dataframe format.
#
# Key features include configurable API settings (key, model, prompt), 
#automatic error handling for missing files or API failures, and the 
#ability to add Gemini responses as a new column to existing dataframes. 
#This makes it ideal for batch processing documents or analyzing text 
#data at scale using Gemini's language capabilities.
#For .txt files as inputs it also has the option of chunking them into
#smaller units.

# Example usage:
# 
# 1. With chunking (default - creates multiple rows per file):
# file_list <- c("document1.txt", "document2.txt", "document3.txt")
# result_df <- call_gemini_api(file_list, input_type = "txt_files")
# # OR explicitly: call_gemini_api(file_list, input_type = "txt_files", use_chunks = TRUE)
#
# 2. Without chunking (one row per file):
# result_df <- call_gemini_api(file_list, input_type = "txt_files", use_chunks = FALSE)
# # Processes entire file as one piece, chunk_number will always be 1
#
# 3. Custom number of chunks (when chunking is enabled):
# result_df <- call_gemini_api(file_list, input_type = "txt_files", 
#                             use_chunks = TRUE, num_chunks = 10)
#
# 4. For CSV column (chunking parameter doesn't apply):
# my_df <- data.frame(text_content = c("Text 1", "Text 2", "Text 3"))
# result_df <- call_gemini_api("text_content", 
#                             input_type = "csv_column", 
#                             output_df = my_df)


# Load required libraries
library(httr2)
library(jsonlite)
library(dplyr)

# External configuration variables
GEMINI_API_KEY <- ""
GEMINI_MODEL <- "gemini-1.5-flash"  # or gemini-1.5-pro
PROMPT_TEMPLATE <- "Summarize this text: {text}"


# Main function for Gemini API calls
call_gemini_api <- function(input_data, 
                            input_type = c("txt_files", "csv_column"),
                            output_df = NULL,
                            output_column = "gemini_response",
                            use_chunks = TRUE,
                            num_chunks = 5,
                            api_key = GEMINI_API_KEY,
                            model = GEMINI_MODEL,
                            prompt = PROMPT_TEMPLATE) {
  
  # Validate input type
  input_type <- match.arg(input_type)
  
  # Helper function to split text into chunks
  split_into_chunks <- function(text, num_chunks = 5) {
    if (nchar(text) == 0) return(rep("", num_chunks))
    
    chars_per_chunk <- ceiling(nchar(text) / num_chunks)
    chunks <- character(num_chunks)
    
    for (i in 1:num_chunks) {
      start_pos <- (i - 1) * chars_per_chunk + 1
      end_pos <- min(i * chars_per_chunk, nchar(text))
      
      if (start_pos <= nchar(text)) {
        chunks[i] <- substr(text, start_pos, end_pos)
      } else {
        chunks[i] <- ""
      }
    }
    return(chunks)
  }
  
  # Helper function to make individual API call
  make_api_call <- function(text_content, prompt_template, api_key, model) {
    # Format the prompt with the text content
    formatted_prompt <- gsub("\\{text\\}", text_content, prompt_template)
    
    # Prepare API request
    request_body <- list(
      contents = list(
        list(
          parts = list(
            list(text = formatted_prompt)
          )
        )
      ),
      generationConfig = list(
        temperature = 0,
        maxOutputTokens = 1024
      )
    )
    
    # Make API call
    tryCatch({
      response <- request(paste0("https://generativelanguage.googleapis.com/v1beta/models/", 
                                 model, ":generateContent")) %>%
        req_url_query(key = api_key) %>%
        req_headers("Content-Type" = "application/json") %>%
        req_body_json(request_body) %>%
        req_perform()
      
      # Parse response
      response_data <- response %>% resp_body_json()
      
      if ("candidates" %in% names(response_data) && 
          length(response_data$candidates) > 0) {
        return(response_data$candidates[[1]]$content$parts[[1]]$text)
      } else {
        return("API Error: No response content")
      }
      
    }, error = function(e) {
      return(paste("Error:", e$message))
    })
  }
  
  # Process based on input type
  if (input_type == "txt_files") {
    # Handle .txt files from list of filenames - now with chunking
    if (!is.character(input_data)) {
      stop("For txt_files input_type, input_data should be a character vector of file paths")
    }
    
    # Initialize vectors to store results
    all_filenames <- character()
    all_chunks <- integer()
    all_responses <- character()
    
    for (file_path in input_data) {
      if (file.exists(file_path)) {
        # Read and prepare text
        text_content <- readLines(file_path, warn = FALSE)
        text_content <- paste(text_content, collapse = "\n")
        
        if (use_chunks) {
          # Split into chunks and process each
          chunks <- split_into_chunks(text_content, num_chunks)
          
          # Process each chunk
          for (chunk_num in 1:num_chunks) {
            chunk_response <- make_api_call(chunks[chunk_num], prompt, api_key, model)
            
            # Store results
            all_filenames <- c(all_filenames, basename(file_path))
            all_chunks <- c(all_chunks, chunk_num)
            all_responses <- c(all_responses, chunk_response)
          }
        } else {
          # Process entire file as one piece
          file_response <- make_api_call(text_content, prompt, api_key, model)
          
          # Store results
          all_filenames <- c(all_filenames, basename(file_path))
          all_chunks <- c(all_chunks, 1)  # Single chunk number
          all_responses <- c(all_responses, file_response)
        }
      } else {
        if (use_chunks) {
          # Handle missing file - create error rows for each chunk
          for (chunk_num in 1:num_chunks) {
            all_filenames <- c(all_filenames, basename(file_path))
            all_chunks <- c(all_chunks, chunk_num)
            all_responses <- c(all_responses, paste("File not found:", file_path))
          }
        } else {
          # Handle missing file - create single error row
          all_filenames <- c(all_filenames, basename(file_path))
          all_chunks <- c(all_chunks, 1)
          all_responses <- c(all_responses, paste("File not found:", file_path))
        }
      }
    }
    
    # Create output dataframe
    output_df <- data.frame(
      filename = all_filenames,
      chunk_number = all_chunks,
      stringsAsFactors = FALSE
    )
    output_df[[output_column]] <- all_responses
    
  } else if (input_type == "csv_column") {
    # Handle CSV column with text per row
    if (is.null(output_df)) {
      stop("For csv_column input_type, output_df must be provided")
    }
    
    if (!input_data %in% names(output_df)) {
      stop(paste("Column", input_data, "not found in output_df"))
    }
    
    text_column <- output_df[[input_data]]
    results <- sapply(text_column, function(text_content) {
      if (is.na(text_content) || text_content == "") {
        return("No text content")
      }
      return(make_api_call(as.character(text_content), prompt, api_key, model))
    }, USE.NAMES = FALSE)
    
    output_df[[output_column]] <- results
  }
  
  return(output_df)
}

file_list<-list.files("")

## With Chunking
# result_df <- call_gemini_api(file_list, input_type = "txt_files", 
#                              use_chunks = TRUE, num_chunks = 5)

## No Chunking
result_df <- call_gemini_api(file_list, input_type = "txt_files", use_chunks = FALSE)

############# INSPECT CHUNKS ##############

# Function to extract chunks to a dataframe
extract_chunks_to_df <- function(file_path, num_chunks = 5) {
  # Read the file
  if (!file.exists(file_path)) {
    stop("File not found:", file_path)
  }
  
  text_content <- readLines(file_path, warn = FALSE)
  text_content <- paste(text_content, collapse = "\n")
  
  # Split into chunks (using same logic as main function)
  split_into_chunks <- function(text, num_chunks = 5) {
    if (nchar(text) == 0) return(rep("", num_chunks))
    
    chars_per_chunk <- ceiling(nchar(text) / num_chunks)
    chunks <- character(num_chunks)
    
    for (i in 1:num_chunks) {
      start_pos <- (i - 1) * chars_per_chunk + 1
      end_pos <- min(i * chars_per_chunk, nchar(text))
      
      if (start_pos <= nchar(text)) {
        chunks[i] <- substr(text, start_pos, end_pos)
      } else {
        chunks[i] <- ""
      }
    }
    return(chunks)
  }
  
  chunks <- split_into_chunks(text_content, num_chunks)
  
  # Calculate chunk positions
  chars_per_chunk <- ceiling(nchar(text_content) / num_chunks)
  
  # Create dataframe
  chunk_df <- data.frame(
    filename = basename(file_path),
    chunk_number = 1:num_chunks,
    start_position = sapply(1:num_chunks, function(i) (i - 1) * chars_per_chunk + 1),
    end_position = sapply(1:num_chunks, function(i) min(i * chars_per_chunk, nchar(text_content))),
    chunk_length = nchar(chunks),
    chunk_content = chunks,
    is_empty = chunks == "",
    first_50_chars = substr(chunks, 1, 50),
    last_50_chars = sapply(chunks, function(x) {
      if (nchar(x) > 50) {
        substr(x, nchar(x) - 49, nchar(x))
      } else {
        x
      }
    }),
    stringsAsFactors = FALSE
  )
  
  return(chunk_df)
}

# Function to process multiple files into one dataframe
extract_all_chunks_to_df <- function(file_paths, num_chunks = 5) {
  # Process each file and combine results
  all_chunks <- data.frame()
  
  for (file_path in file_paths) {
    if (file.exists(file_path)) {
      file_chunks <- extract_chunks_to_df(file_path, num_chunks)
      all_chunks <- rbind(all_chunks, file_chunks)
    } else {
      # Create error rows for missing files
      error_df <- data.frame(
        filename = basename(file_path),
        chunk_number = 1:num_chunks,
        start_position = NA,
        end_position = NA,
        chunk_length = 0,
        chunk_content = paste("File not found:", file_path),
        is_empty = TRUE,
        first_50_chars = "ERROR",
        last_50_chars = "ERROR",
        stringsAsFactors = FALSE
      )
      all_chunks <- rbind(all_chunks, error_df)
    }
  }
  
  return(all_chunks)
}

#Insert file path to original .txt file to inspect sample chunking
chunks_df <- extract_chunks_to_df("/Users/akpiper/Desktop/TestNovels/CA_TheInnocents.txt", num_chunks = 5)

#### OLD #####
# Load required libraries
library(httr2)
library(jsonlite)
library(dplyr)

# External configuration variables
GEMINI_API_KEY <- "your_api_key_here"
GEMINI_MODEL <- "gemini-1.5-flash"  # or gemini-1.5-pro
PROMPT_TEMPLATE <- "Please analyze the following text: {text}"

# Main function for Gemini API calls
call_gemini_api <- function(input_data, 
                            input_type = c("txt_files", "csv_column"),
                            output_df = NULL,
                            output_column = "gemini_response",
                            use_chunks = TRUE,
                            chunk_type = c("number", "size"),
                            num_chunks = 5,
                            chunk_size = 10000,  # NOW IN WORDS, NOT CHARACTERS
                            api_key = GEMINI_API_KEY,
                            model = GEMINI_MODEL,
                            prompt = PROMPT_TEMPLATE) {
  
  # Validate input type and chunk type
  input_type <- match.arg(input_type)
  chunk_type <- match.arg(chunk_type)
  
  # Helper function to split text into chunks by number (character-based)
  split_into_chunks_by_number <- function(text, num_chunks = 5) {
    if (nchar(text) == 0) return(rep("", num_chunks))
    
    chars_per_chunk <- ceiling(nchar(text) / num_chunks)
    chunks <- character(num_chunks)
    
    for (i in 1:num_chunks) {
      start_pos <- (i - 1) * chars_per_chunk + 1
      end_pos <- min(i * chars_per_chunk, nchar(text))
      
      if (start_pos <= nchar(text)) {
        chunks[i] <- substr(text, start_pos, end_pos)
      } else {
        chunks[i] <- ""
      }
    }
    return(chunks)
  }
  
  # Helper function to split text into chunks by size - WORD-BASED (drops remainder)
  split_into_chunks_by_size <- function(text, chunk_size = 10000) {
    if (nchar(text) == 0) return(character(0))
    
    # Split text into words
    words <- strsplit(text, "\\s+")[[1]]
    total_words <- length(words)
    
    # Calculate how many complete chunks fit
    num_complete_chunks <- floor(total_words / chunk_size)
    
    if (num_complete_chunks == 0) {
      return(character(0))  # Return empty if text has fewer words than chunk_size
    }
    
    chunks <- character(num_complete_chunks)
    
    for (i in 1:num_complete_chunks) {
      start_word <- (i - 1) * chunk_size + 1
      end_word <- i * chunk_size
      chunk_words <- words[start_word:end_word]
      chunks[i] <- paste(chunk_words, collapse = " ")
    }
    
    return(chunks)
  }
  
  # Helper function to make individual API call
  make_api_call <- function(text_content, prompt_template, api_key, model) {
    # Format the prompt with the text content
    formatted_prompt <- gsub("\\{text\\}", text_content, prompt_template)
    
    # Prepare API request
    request_body <- list(
      contents = list(
        list(
          parts = list(
            list(text = formatted_prompt)
          )
        )
      ),
      generationConfig = list(
        temperature = 0,
        maxOutputTokens = 1024
      )
    )
    
    # Make API call
    tryCatch({
      response <- request(paste0("https://generativelanguage.googleapis.com/v1beta/models/", 
                                 model, ":generateContent")) %>%
        req_url_query(key = api_key) %>%
        req_headers("Content-Type" = "application/json") %>%
        req_body_json(request_body) %>%
        req_perform()
      
      # Parse response
      response_data <- response %>% resp_body_json()
      
      if ("candidates" %in% names(response_data) && 
          length(response_data$candidates) > 0) {
        return(response_data$candidates[[1]]$content$parts[[1]]$text)
      } else {
        return("API Error: No response content")
      }
      
    }, error = function(e) {
      return(paste("Error:", e$message))
    })
  }
  
  # Process based on input type
  if (input_type == "txt_files") {
    # Handle .txt files from list of filenames - now with flexible chunking
    if (!is.character(input_data)) {
      stop("For txt_files input_type, input_data should be a character vector of file paths")
    }
    
    # Initialize vectors to store results
    all_filenames <- character()
    all_chunks <- integer()
    all_responses <- character()
    
    for (file_path in input_data) {
      if (file.exists(file_path)) {
        # Read and prepare text
        text_content <- readLines(file_path, warn = FALSE)
        text_content <- paste(text_content, collapse = "\n")
        
        if (use_chunks) {
          # Split into chunks based on type
          if (chunk_type == "number") {
            chunks <- split_into_chunks_by_number(text_content, num_chunks)
            actual_num_chunks <- num_chunks
          } else { # chunk_type == "size"
            chunks <- split_into_chunks_by_size(text_content, chunk_size)
            actual_num_chunks <- length(chunks)
            
            # If no complete chunks fit, skip this file
            if (actual_num_chunks == 0) {
              # Add a single row indicating file too small
              all_filenames <- c(all_filenames, basename(file_path))
              all_chunks <- c(all_chunks, 1)
              all_responses <- c(all_responses, paste("File too small: fewer than", chunk_size, "words"))
              next
            }
          }
          
          # Process each chunk
          for (chunk_num in 1:actual_num_chunks) {
            chunk_response <- make_api_call(chunks[chunk_num], prompt, api_key, model)
            
            # Store results
            all_filenames <- c(all_filenames, basename(file_path))
            all_chunks <- c(all_chunks, chunk_num)
            all_responses <- c(all_responses, chunk_response)
          }
        } else {
          # Process entire file as one piece
          file_response <- make_api_call(text_content, prompt, api_key, model)
          
          # Store results
          all_filenames <- c(all_filenames, basename(file_path))
          all_chunks <- c(all_chunks, 1)  # Single chunk number
          all_responses <- c(all_responses, file_response)
        }
      } else {
        if (use_chunks) {
          # Handle missing file - create error rows based on chunk type
          if (chunk_type == "number") {
            error_chunks <- num_chunks
          } else { # chunk_type == "size"
            # For missing files with size chunking, default to 1 error row
            error_chunks <- 1
          }
          
          for (chunk_num in 1:error_chunks) {
            all_filenames <- c(all_filenames, basename(file_path))
            all_chunks <- c(all_chunks, chunk_num)
            all_responses <- c(all_responses, paste("File not found:", file_path))
          }
        } else {
          # Handle missing file - create single error row
          all_filenames <- c(all_filenames, basename(file_path))
          all_chunks <- c(all_chunks, 1)
          all_responses <- c(all_responses, paste("File not found:", file_path))
        }
      }
    }
    
    # Create output dataframe
    output_df <- data.frame(
      filename = all_filenames,
      chunk_number = all_chunks,
      stringsAsFactors = FALSE
    )
    output_df[[output_column]] <- all_responses
    
  } else if (input_type == "csv_column") {
    # Handle CSV column with text per row
    if (is.null(output_df)) {
      stop("For csv_column input_type, output_df must be provided")
    }
    
    if (!input_data %in% names(output_df)) {
      stop(paste("Column", input_data, "not found in output_df"))
    }
    
    text_column <- output_df[[input_data]]
    results <- sapply(text_column, function(text_content) {
      if (is.na(text_content) || text_content == "") {
        return("No text content")
      }
      return(make_api_call(as.character(text_content), prompt, api_key, model))
    }, USE.NAMES = FALSE)
    
    output_df[[output_column]] <- results
  }
  
  return(output_df)
}

# Example usage:
# 
# 1. With chunking by number (creates specified number of chunks, character-based):
# file_list <- c("document1.txt", "document2.txt", "document3.txt")
# result_df <- call_gemini_api(file_list, input_type = "txt_files")
# # OR explicitly: call_gemini_api(file_list, input_type = "txt_files", chunk_type = "number", num_chunks = 5)
#
# 2. With chunking by size (creates chunks of specified WORD length, drops remainder):
# result_df <- call_gemini_api(file_list, input_type = "txt_files", 
#                             chunk_type = "size", chunk_size = 10000)
# # For a 105,000 word novel with chunk_size = 10000, creates 10 chunks (drops last 5,000 words)
#
# 3. Without chunking (one row per file):
# result_df <- call_gemini_api(file_list, input_type = "txt_files", use_chunks = FALSE)
#
# 4. Custom number of chunks:
# result_df <- call_gemini_api(file_list, input_type = "txt_files", 
#                             chunk_type = "number", num_chunks = 10)
#
# 5. For CSV column (chunking parameters don't apply):
# my_df <- data.frame(text_content = c("Text 1", "Text 2", "Text 3"))
# result_df <- call_gemini_api("text_content", 
#                             input_type = "csv_column", 
#                             output_df = my_df)
#
# 6. Custom prompt and word-based chunking:
# PROMPT_TEMPLATE <- "Analyze the narrative structure in this text: {text}"
# result_df <- call_gemini_api(input_data, prompt = PROMPT_TEMPLATE, 
#                             chunk_type = "size", chunk_size = 5000)  # 5000 words per chunk
#
# IMPORTANT NOTES:
# - chunk_type = "number": Splits by character count into specified number of chunks (keeps all text)
# - chunk_type = "size": Splits by WORD COUNT into chunks of specified size (drops remainder)
# - chunk_size parameter is now in WORDS, not characters
# - Default chunk_size = 10000 means 10,000 words per chunk
# - For 105,000 word novel: chunk_size = 10000 creates 10 chunks, drops 5,000 words
