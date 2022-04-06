#########     Text Difficulty Measures    ####################
#########     by Andrew Piper             ####################
#########     CC By 4.0 License           ####################

#This script provides three text difficulty measures using bookNLP data:
#- avg. type-token ratio
#- avg. sentence length
#- avg. word length

#It takes as input a directory of bookNLP "tokens" files
#It outputs a table of scores for each work

#set your working directory to where your txt files are located
setwd("~/Data/bookNLP_gut_child_select")

#get list of files in target directory
f.names<-list.files()

#create empty table that will store your results
difficulty.df<-NULL

#for every file
for (i in 1:length(f.names)){
  print(i)
  
  #ingest each file - IF tab separated
  a<-read.csv(f.names[i], sep="\t", quote = "", stringsAsFactors = F)
  
  #ingest each file - IF comma separated 
  #a<-read.csv(f.names[i], stringsAsFactors = F)
  
  #average sentence length
  avg.sent.length<-mean(table(a$sentenceID))
  
  #average word length
  #remove punctuation first
  a.sub<-a[-grep("[[:punct:]]", a$originalWord),]
  avg.word.length<-mean(nchar(a.sub$originalWord))
  
  #average type token ratio
  #this takes n samples of s.size length and calculates the type token ration for each sample
  #it then takes the mean type-token ratio for all samples as the mean TTR score for the document
  
  #establish number of samples to take
  n=20
  
  #establish length of sample (i.e. # words to test)
  s.size<-499
  
  #establish empty vector for all samples
  ttr.v<-vector(mode="numeric", length=n)
  
  #for each sample
  for (j in 1:n) {
    beg<-sample(1:(nrow(a.sub)-s.size),1)
    s<-a.sub$originalWord[beg:(beg+s.size)]
    ttr.v[j]<-length(unique(s))/length(s)
  }
  
  #get average of all samples
  avg.ttr<-mean(ttr.v)
  
  #store work name
  work<-f.names[i]
  
  #combine all scores
  temp.df<-data.frame(work, avg.sent.length, avg.word.length, avg.ttr)
  
  #add to meta-table
  difficulty.df<-rbind(difficulty.df, temp.df)
}

#change wd back to your home directory
setwd("")

#save
write.csv(difficulty.df, file="Difficulty.csv", row.names = F)

