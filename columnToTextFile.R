#### convert column of a data frame to a text file ######

library(tm)

setwd()
q<-read.csv(, stringsAsFactors = F)

#write text
for (i in 1:nrow(q)){
  #create variable of words as single vector
  work<-unlist(strsplit(q$review[i], " "))
  #collapse into single chunk for writing to file
  text.whole<-paste(work, collapse=" ")
  text.char<-as.String(text.whole)
  #create unique filename and remove punctuation and add extension
  filename<-gsub(" ", "",q$booktitle[i])
  filename<-gsub("[[:punct:]]", "",filename)
  filename<-paste(filename, ".txt", sep="")
  #write file
  write(text.char, file=filename)
}
