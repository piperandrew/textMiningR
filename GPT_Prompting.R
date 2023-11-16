#########################
##### GPT prompting #####
#########################

### Install Required Packages
library(httr)
library(tidyverse)

# Note: code was adapted by this blog post: https://rpubs.com/nirmal/setting_chat_gpt_R. 
# We highly recommend you read over that blog post in detail if you are stuck at any of these steps 
# First, you must get your ChatGPT API key from here: https://platform.openai.com/overview 

# Then, put your API key in the quotes below: 
my_API <- ""

#The "hey_chatGPT function will help you access the API and prompt GPT 
hey_chatGPT <- function(answer_my_question) {
  chat_GPT_answer <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", my_API)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-4-0613", #gpt-4-1106-preview gpt-3.5-turbo-0301, gpt-3.5-turbo-0613, gpt-3.5-turbo-16k-0613, gpt-4-0613, gpt-4-32k
      temperature = 0,
      messages = list(
        list(
          role = "user",
          content = answer_my_question
        )
      )
    )
  )
  str_trim(content(chat_GPT_answer)$choices[[1]]$message$content)
}

#set working directory
setwd("")

# Read in dataset
data <- read_csv("")

#randomize order
data<-data[sample(nrow(data), nrow(data)),]

# Create a "gpt" column
data$gpt <- NA

#subset for testing
data<-data[1:10,]

# Run a loop over your dataset and prompt ChatGPT - an example prompt for sentiment is given
for (i in 1:nrow(data)) {
  print(i)
  
  question <- "Is there a flashback in this passage? Answer only with a number: 1 if yes, 2 if no. Here is the text:"

  text <- data[i,2] #change column number to match where your text is       
  concat <- paste(question, text)
  result <- hey_chatGPT(concat)
  while(length(result) == 0){
    result <- hey_chatGPT(concat)
    print(result)
  }
  #print(result)
  
  #specify GPT output column
  data$gpt[i] <- result
}

#Take only the first string from gpt and convert to a numeric 
data$gpt <- substr(data$gpt, 1, 1)  
data$gpt <- as.numeric(data$gpt)
write.csv(data, "", row.names = F)



