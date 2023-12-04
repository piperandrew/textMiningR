##########    Calculate F1 using GPT Annotations   #############
#takes as input a table with GPT annotations and human annotations

#function takes as input two vectors with 1 = positive, 2 = negative
#conditions on F1 score for positive case
calculate_metrics <- function(ground_truth, predictions) {
  # Check if the vectors have the same length
  if (length(ground_truth) != length(predictions)) {
    stop("Vectors must have the same length")
  }
  
  # Calculate True Positives, False Positives, False Negatives
  tp <- sum(ground_truth == 1 & predictions == 1)
  fp <- sum(ground_truth == 2 & predictions == 1)
  fn <- sum(ground_truth == 1 & predictions == 2)
  
  # Calculate Precision, Recall, and F1-score
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  f1 <- 2 * (precision * recall) / (precision + recall)
  
  # Create a list to store the results
  metrics <- list(
    precision = precision,
    recall = recall,
    f1 = f1
  )
  
  return(metrics)
}

# Calculate Precision, Recall, F1
ground_truth <- data$human
predictions <- data$gpt
results <- calculate_metrics(ground_truth, predictions)
cor.test(ground_truth, predictions, method = c("spearman"))

ggplot(data, aes(x=avg_overall, y=gpt.ordinal)) +
  geom_jitter(height = 0.1, alpha=.5) +
  theme_bw() +
  xlab("Reader") +
  ylab("GPT")

