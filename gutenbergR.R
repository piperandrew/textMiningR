##########  GutenbergR    ####################
######### by Andrew Piper ####################
######### CC By 4.0 License ##################

#This script helps you use the gutenbergR library. It allows you to search by
#subject heading or use a prior list of texts to download books.

library(gutenbergr)
library(plyr)

#get subject headings
a<-as.data.frame(gutenberg_subjects)

#get author metadata
auth<-as.data.frame(gutenberg_authors)

#get book metadata
meta<-as.data.frame(gutenberg_metadata, mirror = myMirror)

#subset by language and full text
meta<-meta[meta$has_text == "TRUE",]
meta.en<-meta[meta$language == "en",]

#use external list of books
#you want a vector of Gutenberg IDs
id<-read.csv("16k Texts.txt", header=F, sep="\t")
id$V1<-gsub("PG", "", id$V1)

#subset metadata by your list
sub.meta<-meta.en[meta.en$gutenberg_id %in% id$V1,]
sub.a<-a[a$gutenberg_id %in% sub.meta$gutenberg_id,]
sub.auth<-auth[auth$gutenberg_author_id %in% sub.meta$gutenberg_author_id,]

#map author birth-dates onto book IDs
sub.meta$birthdate<-mapvalues(sub.meta$gutenberg_author_id, from=sub.auth$gutenberg_author_id, to=sub.auth$birthdate)

#download full text of books
my_mirror <- "http://mirrors.xmission.com/gutenberg/"
books<-gutenberg_download(sub.meta$gutenberg_id, mirror = my_mirror)

#export full text of books to directory on your computer
#set working directory
setwd("~/Data/gut_BIO")

#for every book
for (i in 1:length(unique(books$gutenberg_id))){
  print(i)
  #subset by individual book
  sub<-books[books$gutenberg_id == as.numeric(levels(as.factor(books$gutenberg_id)))[i],]
  #collapse into a single unit
  sub1<-paste(sub$text, collapse = " ")
  #create custon filename
  file.name<-paste("PG", sub$gutenberg_id[1], ".txt", sep = "")
  
  #if you downloaded with title information
  #removes spaces, removes line breaks (twice!), removes chars after the first 20
  #file.name<-paste(sub$gutenberg_id[1],"_", gsub("\n", "", substr(gsub("[[:punct:]]", "",gsub(" ", "", sub$title[1], fixed = TRUE)), 1,20), fixed=TRUE),".txt", sep="")
  #file.name<-gsub("\r", "", file.name, fixed = T)
  
  #write file
  write(sub1, file=file.name)
}


