######### Select Random Sentences bookNLP   ####################
######### by Andrew Piper ####################
######### CC By 4.0 License ##################

#this script takes as input a directory of bookNLP files
#it outputs a random sample of sentences into a table

#set working directory where your source texts are located
wd.root<-c("")

setwd(wd.root)

#get list of files
fn<-list.files()

#subset by external list
#sub<-read.csv("")
#fn<-fn[fn %in% sub$FILENAME]

#sample n files
n<-100
fn.s<-sample(fn, n)

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
  s=5
  samp.s<-seq(from=start, to=(start+(s-1)), by=1)
  tokens.s<-tokens.df[tokens.df$sentence_ID %in% samp.s,]
  
  #combine into a single passage
  p<-paste(tokens.s$word, sep=" ", collapse=" ")
  
  #build table
  fileID<-fn.s[i]
  temp.df<-data.frame(fileID,p)
  final.df<-rbind(final.df, temp.df)
}


