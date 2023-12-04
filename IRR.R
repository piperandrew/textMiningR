#############   Inter-Rater Agreement   ############
library(irr)
setwd("/Users/akpiper/Research/Character Cognition")
setwd("/Users/akpiper/Desktop")
#these scripts help measure inter-rater agreement under different conditions

#Input: 
#Metadata
m<-read.csv("")

#Table of n rows of observations and m columns of annotators
a<-read.csv("final.csv")

#subset table
a1<-a[,2:3]

#Fleiss's Kappa on m raters
#kappam.fleiss(a1)

#Cohen's kappa on 2 raters
kappa2(a1)

######## Per observation agreement function #######
#this calculates how many annotators agreed on the max annotation
calculate_agreement <- function(df) {
  # Initialize an empty vector to store the agreement counts
  agreement_counts <- numeric(nrow(df))
  
  # Loop through each row of the data frame
  for (i in 1:nrow(df)) {
    # Get the values in the current row
    row_values <- unlist(unname(as.vector(df[i, ])))
    
    # Count the number of unique values in the row
    unique_values <- unique(row_values)
    
    # Calculate the agreement count as the number of times the most common value appears
    agreement_count <- max(table(row_values))
    
    # Store the agreement count in the vector
    agreement_counts[i] <- agreement_count
  }
  
  # Return the agreement counts vector
  return(agreement_counts)
}

##### Majority Label Function #####
#it supplies NA if there is no majority
calculate_majority <- function(df) {
  # Initialize an empty vector to store the agreement counts
  majority_label <- numeric(nrow(df))
  
  # Loop through each row of the data frame
  for (i in 1:nrow(df)) {
    # Get the values in the current row
    row_values <- unlist(unname(as.vector(df[i, ])))
    
    # Count the number of unique values in the row
    unique_values <- unique(row_values)
    
    # Calculate the agreement count as the number of times the most common value appears
    if (max(table(row_values)) > 1){
      maj_label <- names(sort(table(row_values), decreasing = T))[1]
      majority_label[i] <- maj_label
    } else {
      majority_label[i]<-NA
    }
  }
  
  # Return the agreement counts vector
  return(majority_label)
}

# Run function to get level of agreement for each observation
agreement_counts <- calculate_agreement(a1)

#add to table
a$agreement<-agreement_counts

#order by agreement, class, label
a<-a[order(-a$agreement, factor(a$Class), -a$Value),]

#Percent of disagreement
table(agreement_counts)/length(agreement_counts)

#run function to get majority label
majority_label<-calculate_majority(a)

#table to get % for each label
table(majority_label)/ length(majority_label)

##### review labels #####

#table annotations for each annotator
table(a$anya.labelle.mail.mcgill.ca)
table(a$aarshiyaa.sharma.gmail.com)

#for each class being evaluated (cognition, emotion, etc) how many times did GPT
#think it was present (1) versus not-present (0)
tapply(a$Value, factor(a$Class), function (x) table(x))

#for each class being evaluated (cognition, emotion, etc) how many times did annotators
#agree with each other about GPTs label?
tapply(a$agreement, factor(a$Class), function (x) table(x))

#for each GPT value (present(1)/not-present(0)) the level of annotator agreement (2,1)
tapply(a$agreement, a$Value, function (x) table(x))

#for a given coder the rate of "agreement" based on GPT positive/negative
tapply(a$anya.labelle.mail.mcgill.ca, a$Value, function (x) table(x))
tapply(a$aarshiyaa.sharma.gmail.com, a$Value, function (x) table(x))





