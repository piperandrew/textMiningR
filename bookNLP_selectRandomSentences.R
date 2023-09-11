######### Select Random Sentences bookNLP   ####################
######### by Andrew Piper ####################
######### CC By 4.0 License ##################

#this script takes as input a directory of bookNLP files
#it outputs a random sample of sentences into a table

#load metadata
setwd("/Users/akpiper/Data")
meta<-read.csv("CONLIT_META.csv")

#set working directory where your source texts are located
wd.root<-c("/Users/akpiper/Data/CONLIT_NLP/")

setwd(wd.root)

#get list of files
fn<-list.files()

fn<-fn[fn %in% meta$ID[meta$Category == "FIC"]]

#sample n files
n<-100
fn.s<-sample(fn, n)

#set number of sample sentences S
s=5

#create empty final table
final.df<-NULL

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
  
  #get random starting point from sentences
  start<-sample(tokens.df$sentence_ID, 1)
  
  #then select S consecutive sentences
  samp.s<-seq(from=start, to=(start+(s-1)), by=1)
  tokens.s<-tokens.df[tokens.df$sentence_ID %in% samp.s,]
  
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




