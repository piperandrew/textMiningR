##############################################################
############### RANDOM PASSAGE EXTRACTOR #####################
##############################################################
# 
# SUMMARY:
# This script extracts random 100-word passages from a directory of .txt files.
# For each file, it:
#   1. Finds a random starting point within the middle 60% of the document
#   2. Snaps to the nearest sentence boundary
#   3. Extracts exactly 100 words from that point (preserving punctuation)
#   4. Outputs a CSV with: unique ID, source filename, and extracted passage
#
# The middle 60% constraint ensures passages come from the "body" of documents,
# avoiding introductions (first 20%) and conclusions (last 20%).
#
# OUTPUT FORMAT:
#   - id: Random 8-character alphanumeric identifier
#   - fileID: Original filename
#   - text: The 100-word extracted passage
#
# ============================================================

library(tokenizers)

# ============================================================
# CONFIGURATION
# ============================================================

input_dir <- "/Users/akpiper/Data/CONLIT"    # Directory containing .txt files
output_csv <- "randomSentences_CONLIT.csv"   # Output CSV filename

# ============================================================
# FUNCTION: Generate random 8-character alphanumeric ID
# ============================================================
# Creates a unique identifier for each extracted passage
# Uses lowercase, uppercase, and digits (62 possible characters)

generate_id <- function() {
  chars <- c(letters, LETTERS, 0:9)
  paste0(sample(chars, 8, replace = TRUE), collapse = "")
}

# ============================================================
# FUNCTION: Count words in a string (without destroying it)
# ============================================================
# Uses tokenizers package to get accurate word count
# Does not modify the original text

count_words <- function(text) {
  length(unlist(tokenize_words(text)))
}

# ============================================================
# FUNCTION: Extract exactly 100 words while preserving punctuation
# ============================================================
# Splits on whitespace but counts only tokens containing alphanumeric characters
# This preserves punctuation attached to words (e.g., "Hello," stays as "Hello,")

extract_100_words <- function(text) {
  # Split on whitespace to get "word tokens" that include punctuation
  tokens <- unlist(strsplit(text, "\\s+"))
  
  # Count "real" words (ignoring punctuation-only tokens)
  word_count <- 0
  end_index <- 0
  
  for (i in seq_along(tokens)) {
    # Check if token contains at least one alphanumeric character
    if (grepl("[a-zA-Z0-9]", tokens[i])) {
      word_count <- word_count + 1
    }
    # Stop when we've found 100 real words
    if (word_count == 100) {
      end_index <- i
      break
    }
  }
  
  # Handle case where fewer than 100 words available
  if (end_index == 0) {
    end_index <- length(tokens)
  }
  
  # Reconstruct passage with original spacing
  paste(tokens[1:end_index], collapse = " ")
}

# ============================================================
# FUNCTION: Extract 100-word passage starting at sentence boundary
# ============================================================
# Core extraction function that:
#   1. Tokenizes document into sentences
#   2. Finds valid starting sentences in the MIDDLE 60% of the document
#   3. Randomly selects one of these valid starting points
#   4. Extracts 100 words from that point

extract_passage <- function(text) {
  # Tokenize into sentences
  sentences <- unlist(tokenize_sentences(text))
  n_sentences <- length(sentences)
  
  # Calculate cumulative word count at end of each sentence
  words_per_sentence <- sapply(sentences, count_words)
  cumulative_words <- cumsum(words_per_sentence)
  total_words <- sum(words_per_sentence)
  
  # --------------------------------------------------------
  # MIDDLE 60% CONSTRAINT
  # --------------------------------------------------------
  # We want to start somewhere between 20% and 80% of the document
  # This avoids openings and endings
  
  # Calculate word boundaries for middle 60%
  start_boundary <- total_words * 0.20  # 20% mark
  end_boundary <- total_words * 0.80    # 80% mark
  
  # Find which sentences fall within the middle 60% (by cumulative word position)
  # A sentence is "in the middle" if its starting position is >= 20% mark
  # The starting position of sentence i is cumulative_words[i-1] (or 0 for first sentence)
  sentence_start_positions <- c(0, cumulative_words[-n_sentences])
  
  in_middle_60 <- which(
    sentence_start_positions >= start_boundary & 
      sentence_start_positions <= end_boundary
  )
  
  # --------------------------------------------------------
  # FIND VALID STARTING SENTENCES
  # --------------------------------------------------------
  # Valid = in middle 60% AND has at least 100 words remaining after it
  
  words_remaining <- total_words - sentence_start_positions
  has_enough_words <- which(words_remaining >= 100)
  
  # Intersect: must be both in middle 60% AND have enough words remaining
  valid_starts <- intersect(in_middle_60, has_enough_words)
  
  # --------------------------------------------------------
  # SELECT STARTING SENTENCE
  # --------------------------------------------------------
  
  if (length(valid_starts) == 0) {
    # Fallback: if no valid starts in middle 60%, use any sentence with enough words
    # (This handles very short documents)
    valid_starts <- has_enough_words
    if (length(valid_starts) == 0) {
      # Document is very short; start from beginning
      start_sentence <- 1
    } else {
      start_sentence <- sample(valid_starts, 1)
    }
  } else {
    # Normal case: randomly select from valid starting sentences
    start_sentence <- sample(valid_starts, 1)
  }
  
  # --------------------------------------------------------
  # EXTRACT PASSAGE
  # --------------------------------------------------------
  
  # Concatenate sentences from the starting point onward
  remaining_text <- paste(sentences[start_sentence:n_sentences], collapse = " ")
  
  # Extract exactly 100 words, preserving punctuation
  
  passage <- extract_100_words(remaining_text)
  
  return(passage)
}

# ============================================================
# MAIN PROCESSING
# ============================================================

# Find all .txt files in the input directory
txt_files <- list.files(input_dir, pattern = "\\.txt$", full.names = TRUE)

cat("Found", length(txt_files), "text files to process\n")
cat("Extracting passages from middle 60% of each document\n\n")

# Initialize results dataframe
results <- data.frame(
  id = character(),
  fileID = character(),
  text = character(),
  stringsAsFactors = FALSE
)

# Process each file
for (filepath in txt_files) {
  filename <- basename(filepath)
  
  # Read file and collapse into single string
  text <- paste(readLines(filepath, warn = FALSE), collapse = " ")
  
  # Extract random passage from middle 60%
  passage <- extract_passage(text)
  
  # Append to results
  results <- rbind(results, data.frame(
    id = generate_id(),
    fileID = filename,
    text = passage,
    stringsAsFactors = FALSE
  ))
  
  cat("Processed:", filename, "\n")
}

# ============================================================
# SAVE OUTPUT
# ============================================================

write.csv(results, output_csv, row.names = FALSE)
cat("\n")
cat("========================================\n")
cat("COMPLETE\n")
cat("Saved", nrow(results), "passages to", output_csv, "\n")
cat("========================================\n")