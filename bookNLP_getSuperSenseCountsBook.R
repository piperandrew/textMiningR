##### Get Supersense Counts from BookNLP Tables ######

library(dplyr)
library(tidyr)
library(reshape2)

# Specify the directory where the CSV files are located
csv_directory <- "/Users/akpiper/Data/bookNLP_gut_folk"

#set working directory
setwd(csv_directory)

# Get the list of CSV files in the directory
csv_files <- list.files(csv_directory, full.names = FALSE)

# Initialize an empty data frame to store the final output
final_table <- data.frame(filename = character(),
                          stringsAsFactors = FALSE)

# Loop through each CSV file
for (i in 1:length(csv_files)){
  
  print(i)
  
  # Read the CSV file
  data <- read.csv(csv_files[i], sep = "\t", quote="")
  
  # if chunking
  if (nrow(data) > 10000){
  chunk.size<-5000
  no.chunks<-floor(nrow(data)/chunk.size)
  #this skips the first / last 5K words
  for (j in 1:(no.chunks-1)){
    sub<-data[(j*chunk.size):(((j+1)*chunk.size)-1),]
    if (nrow(sub) > 4900){
    sub<-sub[-grep("I-", sub$supersense),]
    sub<-sub[-grep("O", sub$supersense),]
    if (nrow(sub) > 10) {
      # Get the counts of each level of the "SuperSense" factor
      counts <- table(sub$supersense)
      
      # Create a data frame with the counts and filename
      filename<-paste(csv_files[i], j, sep="_")
      counts_df <- data.frame(filename, counts)
      
      # Append the counts to the final table
      final_table <- bind_rows(final_table, counts_df)
    }
  }
  }
  }
  
  # #if no chunking
  # data<-data[-grep("I-", data$supersense),]
  # data<-data[-grep("O", data$supersense),]
  # 
  # if (nrow(data) > 10) {
  #   # Get the counts of each level of the "SuperSense" factor
  #   counts <- table(data$supersense)
  #   
  #   # Create a data frame with the counts and filename
  #   counts_df <- data.frame(filename = basename(csv_files[i]), counts)
  #   
  #   # Append the counts to the final table
  #   final_table <- bind_rows(final_table, counts_df)
  # }
}

#if chunking divide by chunk.size
final_table$Freq<-final_table$Freq/chunk.size

# Reshape the final table to have one column for each factor level
wide_table <- reshape(final_table, idvar = "filename", timevar = "Var1", direction = "wide")

#fix column names
colnames(wide_table)<-gsub("Freq.B-", "",colnames(wide_table))
#remove final column
wide_table<-wide_table[,-43]
#replace NA with 0
wide_table[is.na(wide_table)] <- 0

#get word counts to divide values by (if not chunking)
token.df<-NULL
for (i in 1:length(csv_files)){
  print(i)
  # Read the CSV file
  data <- read.csv(csv_files[i], sep = "\t", quote="")
  #calculate rows
  token.count<-nrow(data)
  id<-csv_files[i]
  temp.df<-data.frame(id, token.count)
  token.df<-rbind(token.df, temp.df)
}

#subset by matching
token.df2<-token.df[token.df$id %in% wide_table$filename,]
token.df2<-token.df2[order(token.df2$id),]
wide_table<-wide_table[order(wide_table$filename),]
which(token.df2$id != wide_table$filename)
wide_table2<-wide_table[,2:42]/token.df2$token.count
wide_table2$id<-token.df2$id
setwd("")
write.csv(wide_table2, file="Reddit_bookNLP_Counts.csv", row.names = F)

write.csv(wide_table, file="bookNLP_gut_folk_SupersenseCounts.csv", row.names = F)


