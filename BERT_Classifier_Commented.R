##########################################################################
################# BERT Sentence Classification with R ####################
##########################################################################
# This script fine-tunes a BERT model on training data and then annotates new sentence data
# It takes as training input a table of sentences and labels
# It takes as prediction input a table of sentences

# Load required libraries
library(tidyverse)  # For data manipulation and visualization
library(reticulate)  # For Python integration in R
library(LiblineaR)  # For linear classification
library(tidymodels)  # For modeling and machine learning tasks
library(rsample)  # For data splitting
library(text)  # For text processing
library(tensorflow)  # For deep learning

# Set working directory
setwd("/Users/akpiper/Research/Story Morals")

# Set up Python environment
use_python("/Users/akpiper/Library/r-miniconda-arm64/envs/r-reticulate/bin/python", required = TRUE)
use_condaenv("r-reticulate", required = TRUE)  # Ensure the correct conda environment is active

# Import required Python modules
reticulate::py_install(c('transformers', 'tensorflow'), pip = TRUE)
transformer = reticulate::import('transformers')
tf = reticulate::import('tensorflow')

# Load BERT model and tokenizer
model_class = transformer$TFBertForSequenceClassification
tokenizer_class = transformer$BertTokenizer
pretrained_weights = 'bert-base-uncased' #'bert-large-uncased'
tokenizer = tokenizer_class$from_pretrained(pretrained_weights)
model = model_class$from_pretrained(pretrained_weights, num_labels = 2)  # Adjust num_labels based on your classification task

# Load Longformer model and tokenizer
# model_class = transformer$TFLongformerForSequenceClassification
# tokenizer_class = transformer$LongformerTokenizer
# pretrained_weights = 'allenai/longformer-base-4096'
# tokenizer = tokenizer_class$from_pretrained(pretrained_weights)
# model = model_class$from_pretrained(pretrained_weights, num_labels = 2)

# Data Preparation
data <- read_csv("CHICAGO_Sentences_18-20M_GPT.csv")
data <- data[sample(nrow(data), 100),]  # Downsample for testing

# Prepare data for training
data <- data %>%
  mutate(sentence = as.character(sentences),
         label = as.factor(gpt))

#establish max_length parameter, i.e. what is the distribution of length of your sentences
summary(nchar(data$sentence))
n=4096 #512 is the max available

# Split the data into train and test sets
set.seed(123)
split <- initial_split(data, prop = 0.8)
train_data <- training(split)
test_data <- testing(split)

# Function to pad sequences to a specified length
pad_sequence <- function(seq, max_length = n, pad_token = 0) {
  if (length(seq) >= max_length) {
    return(seq[1:max_length])
  } else {
    return(c(seq, rep(pad_token, max_length - length(seq))))
  }
}

# Function to encode a batch of sentences
encode_batch <- function(sentences, max_length = n) {
  results <- lapply(sentences, function(sentence) {
    encoding <- tokenizer$encode_plus(
      as.character(sentence),
      add_special_tokens = TRUE,
      return_attention_mask = TRUE,
      return_tensors = NULL  # Return Python list instead of TensorFlow tensor
    )
    
    input_ids <- pad_sequence(encoding$input_ids, max_length)
    attention_mask <- pad_sequence(encoding$attention_mask, max_length)
    
    list(input_ids = input_ids, attention_mask = attention_mask)
  })
  
  # Combine results into matrices
  input_ids_matrix <- do.call(rbind, lapply(results, `[[`, "input_ids"))
  attention_mask_matrix <- do.call(rbind, lapply(results, `[[`, "attention_mask"))
  
  # Convert to TensorFlow tensors
  list(
    input_ids = tf$constant(input_ids_matrix, dtype = tf$int32),
    attention_mask = tf$constant(attention_mask_matrix, dtype = tf$int32)
  )
}

# Encode training data
tryCatch({
  train_encodings <- encode_batch(train_data$sentence)
  print("Batch encoding successful")
  print(str(train_encodings))
}, error = function(e) {
  print(paste("Error:", e$message))
  print("Python error details:")
  print(reticulate::py_last_error())
})

# Prepare to make the Train Dataset
input_ids_array <- as.array(train_encodings$input_ids)
attention_mask_array <- as.array(train_encodings$attention_mask)
labels_array <- as.array(as.integer(as.numeric(train_data$label) - 1))

features <- list(
  input_ids = tf$constant(input_ids_array, dtype = tf$int32),
  attention_mask = tf$constant(attention_mask_array, dtype = tf$int32)
)

# Create the dataset
train_dataset <- tf$data$Dataset$from_tensor_slices(
  list(
    input_ids = tf$constant(input_ids_array, dtype = tf$int32),
    attention_mask = tf$constant(attention_mask_array, dtype = tf$int32),
    labels = tf$constant(labels_array, dtype = tf$int32)
  )
)

# Apply batching
train_dataset <- train_dataset$batch(as.integer(8))

# Reinitialize the model
model <- transformer$TFBertForSequenceClassification$from_pretrained(
  pretrained_weights,
  num_labels = as.integer(2)  # Adjust this based on your number of classes
)

# Compile the model
optimizer <- tf$keras$optimizers$Adam(learning_rate = 2e-5)
loss <- tf$keras$losses$SparseCategoricalCrossentropy(from_logits = TRUE)
model$compile(optimizer = optimizer, loss = loss, metrics = 'accuracy')

model$compile(
  optimizer = "adam",  # Use string identifier instead of object
  loss = tf$keras$losses$SparseCategoricalCrossentropy(from_logits = TRUE),
  metrics = list("accuracy")  # Use a list for metrics
)

####  Train the model  ######
# This may take awhile
history <- tryCatch({
  model$fit(
    train_dataset,
    epochs = as.integer(3),
    verbose = 1
  )
}, error = function(e) {
  print(paste("Error during model fitting:", e$message))
  print("Python error details:")
  print(reticulate::py_last_error())
})

#####  Prepare test data  ######
test_encodings <- encode_batch(test_data$sentence)
test_labels <- as.integer(as.numeric(test_data$label) - 1)

test_dataset <- tf$data$Dataset$from_tensor_slices(
  list(
    input_ids = tf$constant(as.array(test_encodings$input_ids), dtype = tf$int32),
    attention_mask = tf$constant(as.array(test_encodings$attention_mask), dtype = tf$int32),
    labels = tf$constant(test_labels, dtype = tf$int32)
  )
)

# Apply batching to test dataset
test_dataset <- test_dataset$batch(as.integer(8))

#### Model Evaluation on Test Data #####
evaluate_model <- function(model, test_dataset, test_data) {
  # Get predictions
  predictions <- model$predict(test_dataset)
  logits <- predictions$logits
  probabilities <- tf$nn$softmax(logits, axis = -1L)$numpy()
  predicted_classes <- tf$argmax(logits, axis = -1L)$numpy()
  
  # Convert predictions to labels
  predicted_labels <- levels(test_data$label)[predicted_classes + 1]
  
  # Extract the highest probability for each prediction
  predicted_probabilities <- apply(probabilities, 1, max)
  
  # Create results data frame
  results_df <- test_data %>%
    mutate(
      predicted_label = predicted_labels,
      probability = predicted_probabilities
    )
  
  # Calculate accuracy
  accuracy <- mean(results_df$label == results_df$predicted_label)
  
  # Print accuracy
  print(paste("Test Accuracy:", round(accuracy, 4)))
  
  return(results_df)
}

# Use the function to evaluate the model and get results
test_results_df <- evaluate_model(model, test_dataset, test_data)

# Optionally, save the results to a CSV file
# write_csv(test_results_df, "test_results_with_predictions.csv")

###### Predict new sentences using above model #######
predict_sentences <- function(sentences) {
  sentences <- as.character(sentences)
  
  # Tokenize each sentence
  tokenized <- lapply(sentences, function(sentence) {
    tokens <- tokenizer$encode(sentence)
    tokens
  })
  
  # Pad sequences manually
  max_length <- max(sapply(tokenized, length))
  padded <- lapply(tokenized, function(tokens) {
    c(tokens, rep(0, max_length - length(tokens)))
  })
  
  # Create attention masks
  attention_masks <- lapply(padded, function(tokens) {
    as.integer(tokens != 0)
  })
  
  # Convert to matrices
  input_ids <- do.call(rbind, padded)
  attention_mask <- do.call(rbind, attention_masks)
  
  # Convert to TensorFlow tensors
  input_ids_tensor <- tf$constant(input_ids, dtype = tf$int32)
  attention_mask_tensor <- tf$constant(attention_mask, dtype = tf$int32)
  
  # Make predictions
  outputs <- model(list(input_ids = input_ids_tensor, attention_mask = attention_mask_tensor))
  logits <- outputs$logits
  probabilities <- tf$nn$softmax(logits, axis = -1L)
  predicted_classes <- tf$argmax(probabilities, axis = -1L)$numpy()
  predicted_probabilities <- probabilities$numpy()
  
  # Convert predictions to labels
  labels <- levels(train_data$label)[predicted_classes + 1]
  
  # Prepare results
  results <- data.frame(
    sentence = sentences,
    predicted_label = labels,
    probability = apply(predicted_probabilities, 1, max)  # Get the highest probability for each sentence
  )
  
  results
}

# Test the prediction function with sample sentences
test_sentences <- c("This is a test sentence.", "One of the big things wrong with this world is so many people want to screw with other people's minds, and they cause so much damage.")
result <- predict_sentences(test_sentences)
print(result)



