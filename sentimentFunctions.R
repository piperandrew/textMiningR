######## Sentiment Functions ########
#Uses vader library to extract sentiment scores
#Input = bookNLP table
#Output = table of sentiment scores

### Needs work !! ####

# concatenate_with_spaces <- function(words) {
#   sentence <- paste(words, collapse = " ")
#   #sentence <- gsub("(\\w)([ '’]+)(\\p{P})", "\\1\\2\\3", sentence, perl = TRUE)
#   sentence <- gsub("(\\w)\\s+(\\p{P})", "\\1\\2", sentence, perl = TRUE)
#   sentence <- gsub(" n’t", "n’t", sentence)
#   #sentence <- gsub("\\s+(\\p{P})", "\\1", sentence, perl = TRUE)
#   #sentence <- replace_contraction(sentence)
#   return(sentence)
# }

# sentiment_score<-function(data_name, column_name){
#   data<-get(data_name)
#
#   #make a vector
#   text_vector <- data[[column_name]]
#
#   #process the vector
#   text<-concatenate_with_spaces(text_vector)
#
#   # Paste the strings into a single string
#   #text <- paste(text_vector, collapse = " ")
#
#   # Perform Vader sentiment analysis on the combined text
#   vader_output <- get_vader(text, neu_set = F, rm_qm = T)[[1]]
#   #take the avg sentiment score for all non-neutral words
#   sentiment_mean<-mean(as.numeric(unlist(str_extract_all(vader_output, "-?\\d+\\.?\\d*"))))
#   sentiment_abs<-mean(abs(as.numeric(unlist(str_extract_all(vader_output, "-?\\d+\\.?\\d*")))))
#   sentiment.df<-data.frame(c("sentiment_mean", "sentiment_abs"), c(sentiment_mean, sentiment_abs))
#   colnames(sentiment.df)<-c("String", "Frequency")
#   return(sentiment.df)
# }