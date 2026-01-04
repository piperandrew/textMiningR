######### Select Random Sentences bookNLP with Supersense Features   ####################
######### by Andrew Piper ####################
######### CC By 4.0 License ##################
library(stringi)
library(stringr)

#Version 1a:
#this script takes as input a directory of bookNLP files
#it outputs a random sample of sentences into a table
#it conditions on sentences containing a given supersense feature

#Version 1b:
#this script takes as input a directory of bookNLP *directories*
#it outputs a random sample of sentences into a table

########## Version 1a ###########

#load metadata
setwd("/Users/akpiper/Data")
meta<-read.csv("CONLIT_META.csv")

#set root working directory
wd.root<-c("/Users/akpiper/Data/CONLIT_NLP/")

setwd(wd.root)

#get list of folders (i.e. books)
filenames<-list.files()

#subset by .tokens files
file.sub<-filenames[grep(".tokens", filenames)]

#subset by .supersense files
file.super<-filenames[grep(".supersense", filenames)]

#normalize filenames
file.sub<-gsub(".tokens",".txt", file.sub)
file.super<-gsub(".supersense",".txt", file.sub)

#subset by FIC
file.sub<-file.sub[file.sub %in% meta$ID[meta$Category == "FIC"]]
file.super<-file.super[file.super %in% file.sub]

#establish sample parameters

#sample n documents
n<-1
#set number of sample sentences per book (these are sequential)
s=3
#select number of passages to extract per book (of s sequential sentences)
pass<-1
#supersense category to sample from
cat<-c("verb.emotion", "noun.feeling")

#sample
fn.s<-sample(file.sub, n)
fn.super<-file.super[file.super %in% fn.s]

#keep all
#fn.s<-file.sub
#fn.super<-file.super

#reorder files
fn.s<-fn.s[order(fn.s)]
fn.super<-fn.super[order(fn.super)]
which(fn.s != fn.super)

#rename files
fn.s<-gsub(".txt", ".tokens", fn.s)
fn.super<-gsub(".txt",".supersense", fn.super)

#create empty final table
final.df<-NULL

#for every book
for (i in 1:length(fn.s)){
  
  print(i)
  
  #load tokens file
  tokens.df<-read.csv(fn.s[i], quote="", sep="\t")
  
  #load supersense file
  super.df<-read.csv(fn.super[i], quote="", sep="\t")
  
  #subset by target supersense(s)
  super.df<-super.df[super.df$supersense_category %in% cat,]
  
  for (j in 1:pass){
    
    #to get random starting point, select sentence that contains target supersense(s)
    sub.tokens<-tokens.df[tokens.df$token_ID_within_document %in% super.df$start_token,]
    
    #get random starting point from the above sample of sentences
    start<-sample(sub.tokens$sentence_ID, 1)
    
    #then select s/2 consecutive sentences before and after the target sentence
    s2<-floor(s/2)
    samp.s<-seq(from=(start-s2), to=(start+s2), by=1)
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
}

write.csv(final.df, file="", row.names = F)
