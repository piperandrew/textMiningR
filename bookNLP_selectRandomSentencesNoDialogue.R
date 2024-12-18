######### Select Random Sentences bookNLP - no dialogue   ####################
######### by Andrew Piper ####################
######### CC By 4.0 License ##################

#this script takes as input a directory of bookNLP files
#it outputs a random sample of sentences into a table
#it only conditions on sentences without dialogue

library(stringr)

########## Version 1a ###########

#metadata
setwd("/Users/akpiper/Data")
meta<-read.csv("CONLIT_META.csv")

#set root working directory
wd.root<-c("/Users/akpiper/Data/CONLIT_NLP/")

setwd(wd.root)

#get list of folders (i.e. books)
filenames<-list.files()

#OPTION: subset by metadata

#first subset bookNLP files by .tokens & dialogue files
file.tok<-filenames[grep(".tokens", filenames)]
file.quotes<-filenames[grep("quotes", filenames)]

#next subset by metadata

#only FIC
file.fic<-meta$ID[meta$Category == "FIC"]

#only 3P FIC
#fic.df<-meta[meta$ID %in% meta$ID[meta$Category == "FIC"],]
#fic.df<-fic.df[fic.df$ID %in% fic.df$ID[fic.df$Probability1P < 0.05],]
#file.fic<-fic.df$ID
#with specific genres
#fic.df<-fic.df[fic.df$ID %in% fic.df$ID[fic.df$Genre %in% c("NYT", "PW")],]
#file.fic<-fic.df$ID

#only specific genres
#file.fic<-meta$ID[meta$Genre %in% c("NYT", "BS", "PW", "MY")]

#begin subsetting
filenames_01 <- file.tok
filenames_01a <-file.quotes
filenames_02 <- file.fic

# Strip extensions
filenames_02_no_ext <- gsub("\\.txt$", "", filenames_02)
filenames_01_no_ext <- gsub("\\.tokens$", "", filenames_01)
filenames_01a_no_ext <- gsub("\\.quotes$", "", filenames_01a)

# Subset
file.sub<-filenames_01_no_ext[filenames_01_no_ext %in% filenames_02_no_ext]

# Add .tokens
file.tok<-paste(file.sub, ".tokens", sep="")
file.quotes<-paste(file.sub, ".quotes", sep="")

# sample n documents
n<-10
file.tok<-sample(file.tok, n)
file.quotes<-sample(file.quotes, n)

#establish parameters

#set number of sample sentences per book (these are sequential)
s=3

#select number of passages to extract per book (of s sequential sentences)
pass<-1

#create empty final table
final.df<-NULL

#loop through ingest and write
for (i in 1:length(file.tok)){
  print(i)
  
  #load tokens table
  tokens.df<-read.csv(file.tok[i], quote="", sep="\t")
  
  #remove sentences with dialogue
  quotes.df<-read.csv(file.quotes[i], quote="", sep="\t")
  
  #get all sentences in dialogue
  sub<-tokens.df[tokens.df$token_ID_within_document %in% quotes.df$quote_start,]
  
  #subset tokens table by removing these sentences
  tokens.sub<-tokens.df[!tokens.df$sentence_ID %in% sub$sentence_ID,]
  
  #then select S consecutive sentences
  for (j in 1:100){
    #get random starting point from sentences
    start<-sample(tokens.sub$sentence_ID, 1)
    samp.s<-seq(from=start, to=(start+(s-1)), by=1)
    tokens.s<-tokens.sub[tokens.sub$sentence_ID %in% samp.s,]
    #check if sentence is 5 or more words and has no quotes
    if (length(tokens.s$sentence_ID) > 4 && !any(grepl("``", tokens.s$fine_POS_tag))) {
      break
    }
  }
  
  #combine into a single passage
  text<-paste(tokens.s$word, sep=" ", collapse=" ")
  
  #remove spaces between punctuation and n't
  text<-str_replace_all(text, "\\s+(?=\\p{P})", "")
  text<-str_replace_all(text, "\\s+(?=n’t\\b)", "")
  
  #build table
  fileID<-file.tok[i]
  temp.df<-data.frame(fileID,text)
  final.df<-rbind(final.df, temp.df)
}


