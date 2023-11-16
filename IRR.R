#############   Inter-Rater Agreement   ############
library(irr)
setwd("/Users/akpiper/Research/Character Cognition")

#these scripts help measure inter-rater agreement under different conditions

#Input: 
#Metadata
m<-read.csv("Metadata_Test_Round_06.csv")

#Table of n rows of observations and m columns of annotators
a<-read.csv("Agreement_Test_Round_06.csv")

#Fleiss's Kappa on m raters
kappam.fleiss(a)

#Cohen's kappa on 2 raters
#kappa2(a)

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
agreement_counts <- calculate_agreement(a)

#table to get % for each level of agreement
table(agreement_counts)/length(agreement_counts)

#run function to get majority label
majority_label<-calculate_majority(a)

#table to get % for each label
table(majority_label)/ length(majority_label)

##### review labels #####

#attach majority label to annotator table to observe disagreements with majority label
a$majority<-majority_label







