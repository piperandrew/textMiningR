###### Text Difficulty #######

#this model takes in individual documents and takes N random samples of 
#X sentence-units from the entire document and calculates two average readability 
#measures for all passages: Flesch reading ease and the Tuldava measure.
#it takes as input a directory of .txt files and creates a table of the 
#mean scores for every document

#here are the libraries you’ll need

library("koRpus")
require("NLP")
library("openNLP")
library("openNLPdata")
library("stringr")

#establish your working directory
wd<-c("WorkingDirectoryHere")

#establish your directory of text files
texts<-c("DirectoryofTextFilesHere")

#establish the number of random samples
n<-20

#establish the number of sentences per passage
x<-15

#set working directory
setwd(wd)
filenames<-list.files(texts)
setwd(paste(wd, texts, sep="/"))
      
#set your annotator — requires openNLP
sent_token_annotator <- Maxent_Sent_Token_Annotator(language = "en") #de=German
      
#run
difficulty.df<-NULL
for (j in 1:length(filenames)) {
  #load text
  work<-scan(filenames[j], what="character", quote="")
        
  #make sure the document is long enough
  if (length(work) > 30000){
          
    #remove punctuation
    work.clean<- gsub("\\d", "", work)
    #collapse into a single chunk
    text.whole<-paste(work.clean, collapse=" ")
    text.char<-as.String(text.whole)
          
    #annotate by sentence boundaries
    a1 <- annotate(text.char, sent_token_annotator)
    sentences<-text.char[a1]
          
    #get n random starting points
    start.v<-sample(1:(length(sentences)-x),n)
    
    #initialize empty vectors
    flesch.v<-vector()
    tuldava.v<-vector()
          
    #take n samples of x sentence units
    for (i in 1:n){
      #create subset of x continuous sentences
      sentence.chunk<-paste(sentences[start.v[i]:(start.v[i]+(x-1))], collapse = '')
      #annotate sentence -- NOTE: change language if necessary
      tag.doc<-tokenize(sentence.chunk, format = "obj", lang="en") 
      #calculate flesch score
      flesch.score<-flesch(tag.doc, quiet=T)
      flesch.score<-flesch.score @ Flesch $RE
      #calculate tuldava score -- uses log, less sensitive to outliers, better across languages
      tuldava.mod<-tuldava(tag.doc, quiet=T)
      tuldava.score<-tuldava.mod @ Tuldava $Tuldava
      flesch.v<-append(flesch.v, flesch.score)
      tuldava.v<-append(tuldava.v, tuldava.score)
    }
    #take mean for all passages
    flesch.mean<-mean(flesch.v)
    tuldava.mean<-mean(tuldava.v)
    work<-filenames[j]
    temp.df<-data.frame(work,flesch.mean, tuldava.mean)
    difficulty.df<-rbind(difficulty.df, temp.df)
  }
}

