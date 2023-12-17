######### Select All Sentences bookNLP   ####################
######### by Andrew Piper ####################
######### CC By 4.0 License ##################
library(stringi)
library(stringr)

#this script takes as input a directory of bookNLP files
#it outputs all sentences from a sample of books into a table

#load metadata
setwd("/Users/akpiper/Data")
meta<-read.csv("CONLIT_META.csv")

#set working directory where your source texts are located
wd.root<-c("/Users/akpiper/Data/CONLIT_NLP/")

setwd(wd.root)

#get list of files
fn<-list.files()

#subset by categories
#fn<-fn[fn %in% meta$ID[meta$Category == "FIC"]]
fn<-fn[fn %in% meta$ID[meta$Genre == "NYT"]]

#sample n documents
n<-200
fn.s<-sample(fn, n)
#fn.s<-fn

#create empty final vector
sentence.v<-vector()
work.v<-vector()

#loop through ingest and write
for (i in 1:length(fn.s)){
  print(i)
  
  #setwd to the i-th book
  wd.file<-paste(wd.root, fn.s[i], sep="")
  setwd(wd.file)
  
  #load tokens table
  book.files<-list.files()
  tokens<-book.files[grep(".tokens", book.files)]
  tokens.df<-read.csv(tokens, quote="", sep="\t")
  
  #subset by sentences with more than 5 words
  #remove punctuation from tokens table
  tokens.nopunct<-tokens.df[-grep("[[:punct:]]", tokens.df$word), ]
  #table sentence ID to get word total per sentence
  sentence_keep<-table(tokens.nopunct$sentence_ID)
  #subset by integer
  sentence_keep<-sentence_keep[sentence_keep > 5]
  #subset tokens table by those sentences
  tokens.df<-tokens.df[tokens.df$sentence_ID %in% as.numeric(names(sentence_keep)),]
  
  #create temporary sentence vector
  sentence.sub<-vector(mode="character", length=length(unique(tokens.df$sentence_ID)))
  
  #for every sentence
  for (j in 1:length(unique(tokens.df$sentence_ID))){
    tokens.s<-tokens.df[tokens.df$sentence_ID == unique(tokens.df$sentence_ID)[j],]
    p<-paste(tokens.s$word, sep=" ", collapse=" ")
    p<-str_replace_all(p, "\\s+(?=\\p{P})", "")
    p<-str_replace_all(p, "\\s+(?=n’t\\b)", "")
    sentence.sub[j]<-p
  }
  
  #append to main vector
  sentence.v<-append(sentence.v, sentence.sub)
  
  #create temporary work vector
  work.sub<-rep(fn.s[i], length(sentence.sub))
  
  #append to work vector
  work.v<-append(work.v, work.sub)
  
}

work<-work.v
sentences<-sentence.v
df<-data.frame(work, sentences)
setwd("/Users/akpiper/Research/Generalization_Literary")
write.csv(sentence.v, file="CONLIT_NYT_Sample_200_Books.csv", row.names = F)


