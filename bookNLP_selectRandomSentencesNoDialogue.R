######### Select Random Sentences bookNLP - no dialogue   ####################
######### by Andrew Piper ####################
######### CC By 4.0 License ##################

#this script takes as input a directory of bookNLP files
#it outputs a random sample of sentences into a table
#it only conditions on sentences without dialogue

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
  
  #remove sentences with dialogue
  quotes<-book.files[grep(".quotes", book.files)]
  quotes.df<-read.csv(quotes, quote="", sep="\t")
  
  #get all sentences in dialogue
  sub<-tokens.df[tokens.df$token_ID_within_document %in% quotes.df$quote_start,]
  
  #subset tokens table by removing these sentences
  tokens.sub<-tokens.df[!tokens.df$sentence_ID %in% sub$sentence_ID,]
  
  #then select S consecutive sentences
  s=5
  for (j in 1:100){
    #get random starting point from sentences
    start<-sample(tokens.sub$sentence_ID, 1)
    samp.s<-seq(from=start, to=(start+(s-1)), by=1)
    #check if those sentences are all still there
    tokens.s<-tokens.sub[tokens.sub$sentence_ID %in% samp.s,]
    if (length(unique(tokens.s$sentence_ID)) == 5) {
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


