######### Select Random Sentences bookNLP   ####################
######### by Andrew Piper ####################
######### CC By 4.0 License ##################

#this script takes as input a directory of bookNLP files
#it outputs a random sample of sentences into a table
#it has the following conditions
#a. only keeps nameed characters
#b. who are in the subject position
#c. in sentences not in dialogue

#load metadata
setwd("/Users/akpiper/Data")
meta<-read.csv("CONLIT_META.csv")

#set working directory where your source texts are located
#make sure to include final backslash
wd.root<-c("/Users/akpiper/Data/CONLIT_NLP/")

setwd(wd.root)

#get list of files
fn<-list.files()

#subset by genre
fn<-fn[fn %in% meta$ID[meta$Category == "FIC"]]

#sample n files
n<-100
fn.s<-sample(fn, n)

#sample s sentences per file
s=2

#create empty final table
final.df<-NULL

#loop through ingest and write
for (i in 1:length(fn.s)){
  print(i)
  
  #setwd to the i-th book
  wd.file<-paste(wd.root, fn.s[i], sep="")
  setwd(wd.file)
  
  #get all book files
  book.files<-list.files()
  
  #load tokens table
  tokens<-book.files[grep(".tokens", book.files)]
  tokens.df<-read.csv(tokens, quote="", sep="\t")
  
  #load dialogue table
  quotes<-book.files[grep(".quotes", book.files)]
  quotes.df<-read.csv(quotes, quote="", sep="\t")
  
  #load character table
  char<-book.files[grep(".entities", book.files)]
  char.df<-read.csv(char, quote="", sep="\t")
  char.df<-char.df[char.df$cat == "PER",]
  #keep only proper names
  char.df<-char.df[char.df$prop == "PROP",]
  
  #subset by sentences where there is a character
  keep.sentences<-unique(tokens.df$sentence_ID[tokens.df$token_ID_within_document %in% char.df$start_token])
  tokens.df1<-tokens.df[tokens.df$sentence_ID %in% keep.sentences,]
  
  #further subset by sentences where the character is in the subject position
  
  #first subset tokens by characters
  tokens.char<-tokens.df1[tokens.df1$token_ID_within_document %in% char.df$start_token | tokens.df1$token_ID_within_document %in% char.df$end_token,]
  
  #remove any that aren't nsubj
  tokens.char<-tokens.char[tokens.char$dependency_relation == "nsubj",]
  
  #remove plurals
  plurs<-grep("S", tokens.char$fine_POS_tag)
  if (length(plurs) > 0){
    tokens.char<-tokens.char[-plurs,]
  }
  
  #keep only sentences with these characters
  tokens.df2<-tokens.df1[tokens.df1$sentence_ID %in% tokens.char$sentence_ID,]
  
  #further remove sentences in dialogue
  #get all sentences in dialogue
  sub<-tokens.df[tokens.df$token_ID_within_document %in% quotes.df$quote_start,]
  
  #subset tokens table by removing these sentences
  tokens.df2<-tokens.df2[!tokens.df2$sentence_ID %in% sub$sentence_ID,]
  
  for (j in 1:200){
    #get random starting point from sentences
    start<-sample(unique(tokens.df2$sentence_ID), 1)
    samp.s<-seq(from=start, to=(start+(s-1)), by=1)
    tokens.s<-tokens.df[tokens.df$sentence_ID %in% samp.s,]
    #check if those sentences are in dialogue table, resample if so
    if (length(which(unique(tokens.s$sentence_ID) %in% sub$sentence_ID)) == 0){
      break
    }
  }
  
  #combine into a single passage
  p<-paste(tokens.s$word, sep=" ", collapse=" ")
  
  #remove spaces between punctuation and n't
  p<-str_replace_all(p, "\\s+(?=\\p{P})", "")
  p<-str_replace_all(p, "\\s+(?=n’t\\b)", "")
  
  #build table
  fileID<-fn.s[i]
  temp.df<-data.frame(fileID,p)
  final.df<-rbind(final.df, temp.df)
}




