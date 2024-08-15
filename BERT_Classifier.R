###############################################################
###########  BERT Sentence Classification with R  #############
###############################################################
# This script fine-tunes a BERT model on training data and then annotates new sentence data
# It takes as training input a table of sentences and labels
# It takes as prediction input a table of sentences

library(tidyverse)
library(reticulate)
library(LiblineaR)
library(tidymodels)
library(rsample)
library(text)
library(tensorflow)

# Set working directory
setwd("/Users/akpiper/Research/Story Morals")

# Set Python environment
use_python("/Users/akpiper/Library/r-miniconda-arm64/envs/r-reticulate/bin/python", required = TRUE)
use_condaenv("r-reticulate", required = TRUE)  # Ensure the correct conda environment is active

# Import Python modules
reticulate::py_install(c('transformers', 'tensorflow'), pip = TRUE)
transformer = reticulate::import('transformers')
tf = reticulate::import('tensorflow')

# Load BERT model and tokenizer
#tokenizer <- transformer$AutoTokenizer$from_pretrained('bert-large-uncased') #bert-base-uncased #bert-large-uncased
model_class = transformer$TFBertForSequenceClassification
tokenizer_class = transformer$BertTokenizer
pretrained_weights = 'bert-base-uncased'
tokenizer = tokenizer_class$from_pretrained(pretrained_weights)
model = model_class$from_pretrained(pretrained_weights, num_labels = 2)  # Adjust num_labels based on your classification task

# Data Preparation
data <- read_csv("CHICAGO_Sentences_18-20M_GPT.csv")

#downsample for testing
data<-data[sample(nrow(data), 1000),]

#prepare for training
data <- data %>%
  mutate(sentence = as.character(sentences),
         label = as.factor(gpt))

# Split the data into train and test sets
set.seed(123)
split <- initial_split(data, prop = 0.8)
train_data <- training(split)
test_data <- testing(split)

#prepare for tensorflow
pad_sequence <- function(seq, max_length = 128, pad_token = 0) {
  if (length(seq) >= max_length) {
    return(seq[1:max_length])
  } else {
    return(c(seq, rep(pad_token, max_length - length(seq))))
  }
}

encode_batch <- function(sentences, max_length = 128) {
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

tryCatch({
  train_encodings <- encode_batch(train_data$sentence)
  print("Batch encoding successful")
  print(str(train_encodings))
}, error = function(e) {
  print(paste("Error:", e$message))
  print("Python error details:")
  print(reticulate::py_last_error())
})

#Prepare to make the Train Dataset
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

test_encodings <- encode_batch(test_data$sentence)
test_labels <- as.integer(as.numeric(test_data$label) - 1)

test_dataset <- tf$data$Dataset$from_tensor_slices(
  list(
    input_ids = tf$constant(as.array(test_encodings$input_ids), dtype = tf$int32),
    attention_mask = tf$constant(as.array(test_encodings$attention_mask), dtype = tf$int32),
    labels = tf$constant(test_labels, dtype = tf$int32)
  )
)

# Apply batching
test_dataset <- test_dataset$batch(as.integer(8))
test_results <- model$evaluate(test_dataset)

#predict function
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
  
  # Convert predictions to labels
  labels <- levels(train_data$label)[predicted_classes + 1]
  
  # Prepare results
  results <- data.frame(
    sentence = sentences,
    predicted_label = labels
  )
  
  results
}

# Test with multiple sentences
test_sentences <- c("This is a test sentence.", "One of the big things wrong with this world is so many people want to screw with other people’s minds, and they cause so much damage.")
result <- predict_sentences(test_sentences)
