######### Select Random Sentences bookNLP   ####################
######### by Andrew Piper ####################
######### CC By 4.0 License ##################
library(stringi)
library(stringr)
library(dplyr)

#Version 1a:
#this script takes as input a directory of bookNLP files
#it outputs a random sample of sentences into a table

#Version 1b:
#this script takes as input a directory of bookNLP directories
#it outputs a random sample of sentences into a table

#Version 2:
#outputs new bookNLP tables that are sequential subsets of the originals (i.e. sampling within the bookNLP space)


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

#first subset by .tokens files
file.sub<-filenames[grep(".tokens", filenames)]

#only FIC
file.fic<-meta$ID[meta$Category == "FIC"]

#only 3P FIC
fic.df<-meta[meta$ID %in% meta$ID[meta$Category == "FIC"],]
fic.df<-fic.df[fic.df$ID %in% fic.df$ID[fic.df$Probability1P < 0.05],]
#file.fic<-fic.df$ID
#with specific genres
fic.df<-fic.df[fic.df$ID %in% fic.df$ID[fic.df$Genre %in% c("NYT", "PW")],]
file.fic<-fic.df$ID

#only specific genres
file.fic<-meta$ID[meta$Genre %in% c("NYT", "BS", "PW", "MY")]

#begin subsetting
filenames_01 <- file.sub
filenames_02 <- file.fic

# Strip extensions
filenames_02_no_ext <- gsub("\\.txt$", "", filenames_02)
filenames_01_no_ext <- gsub("\\.tokens$", "", filenames_01)

# Subset
file.sub<-filenames_01_no_ext[filenames_01_no_ext %in% filenames_02_no_ext]

# Add .tokens
file.sub<-paste(file.sub, ".tokens", sep="")

#establish parameters

#sample n documents
n<-250
fn.s<-sample(file.sub, n)
#fn.s<-file.sub

#set number of sample sentences per book (these are sequential)
s=3

#select number of passages to extract per book (of s sequential sentences)
pass<-2

#check if all matching
which(!fn.s %in% filenames)

#create empty final table
final.df<-NULL

#for every book
for (i in 1:length(fn.s)){
  
  print(i)
  
  #load tokens file
  tokens.df<-read.csv(fn.s[i], quote="", sep="\t")
  
  for (j in 1:pass){
    
    #get random starting point from sentences
    start<-sample(tokens.df$sentence_ID, 1)
    
    #then select s consecutive sentences
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
}

final.df2<-final.df

final.df<-final.df[sample(nrow(final.df), nrow(final.df)),]

write.csv(final.df, file="NarrativeDiscourse_WORLDIT_EN_Sample.csv", row.names = F)
 

###### Version 1b ########
#incomplete
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
  
  #sample from end of book only 
  #tokens.df<-tokens.df[(nrow(tokens.df)-5000):nrow(tokens.df),]
  #tokens.df<-tokens.df[-which(tokens.df$sentence_ID == min(tokens.df$sentence_ID)),]
  
  for (j in 1:pass){
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
}
write.csv(final.df, file="CONLIT_Sample_100_ForConcreteness.csv", row.names = F)



###### Version 2 ######

#load metadata
setwd("/Users/akpiper/Data")
meta<-read.csv("CONLIT_META.csv")

#set working directory where your source texts are located
wd.root<-c("/Users/akpiper/Data/CONLIT_NLP/")

setwd(wd.root)

#get list of files
fn<-list.files()

#subset
#fn<-fn[fn %in% meta$ID[meta$Category == "FIC"]]

#sample n files
n<-20
fn.s<-sample(fn, n)

#set number of sample sentences s
#s=25

#set number of sample words
T=350

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
  
  super<-book.files[grep(".supersense", book.files)]
  super.df<-read.csv(super, quote="", sep="\t")
  
  #get random starting point from sentences
  start<-sample(unique(tokens.df$sentence_ID)[1:(length(unique(tokens.df$sentence_ID))-(s+1))], 1)
  
  #then get T tokens ahead and get all sentence IDs within that window
  end<-tokens.df$sentence_ID[tokens.df$token_ID_within_document == tokens.df$token_ID_within_document[tokens.df$sentence_ID == start][1]+T]
  
  #then select S consecutive sentences
  samp.s<-seq(from=start, to=end)
  
  #sample from tokens table
  tokens.s<-tokens.df[tokens.df$sentence_ID %in% samp.s,]
  
  #sample from supersense table
  #get list of tokens
  super.s<-super.df[super.df$start_token %in% tokens.s$token_ID_within_document,]
  
  #write as file
  #for tokens
  #filename<-paste(fn.s[i], "_sample.csv", sep="")
  #setwd("/Users/akpiper/Data/CONLIT_NLP_PageSamples")
  #write.csv(tokens.s, file=filename)
  
  #for supersense
  filename<-paste(fn.s[i], "_sampleSuper.csv", sep="")
  setwd("/Users/akpiper/Data/CONLIT_NLP_PageSamples_Supersense")
  write.csv(super.s, file=filename)
  #print(nrow(tokens.s))
}

#Faster version for just supersenses
for (i in 1:length(fn.s)){
  print(i)
  
  #setwd to the i-th book
  wd.file<-paste(wd.root, fn.s[i], sep="")
  setwd(wd.file)
  
  #load tokens table
  book.files<-list.files()
  super<-book.files[grep(".supersense", book.files)]
  super.df<-read.csv(super, quote="", sep="\t")
  
  #get random starting point 
  start<-sample((nrow(super.df)-100), 1)
  
  #then select 100 consecutive words
  samp.s<-seq(from=start, to=start+99)
  super.s<-super.df[samp.s,]
  
  #for supersense
  filename<-paste(fn.s[i], "_sampleSuper.csv", sep="")
  setwd("/Users/akpiper/Data/CONLIT_NLP_PageSamples_Supersense")
  write.csv(super.s, file=filename)
  #print(nrow(tokens.s))
}


###### Version 3 #######
#Conditions on Realis events
#Takes only single sentence

#load metadata
setwd("/Users/akpiper/Data")
meta<-read.csv("CONLIT_META.csv")

#set working directory where your source texts are located
wd.root<-c("/Users/akpiper/Data/CONLIT_NLP/")

setwd(wd.root)

#get list of files
fn<-list.files()

#subset
fn<-fn[fn %in% meta$ID[meta$Category == "FIC"]]

#sample n files
#n<-100
#fn.s<-sample(fn, n)
fn.s<-fn

#set number of sample sentences S per passage
s=1

#set number of total passages to sample
p=5

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
  quote.tokens<-tokens.df[tokens.df$token_ID_within_document %in% quotes.df$quote_start,]
  
  #subset tokens table by removing these sentences
  tokens.sub<-tokens.df[!tokens.df$sentence_ID %in% quote.tokens$sentence_ID,]
  
  #subset by sentences with realis events as ROOT verb
  sub<-tokens.sub[tokens.sub$event == "EVENT",]
  sub<-sub[sub$dependency_relation == "ROOT",]
  
  #remove "said"
  #sub<-sub[sub$lemma != "said",]
  real.df<-tokens.df[tokens.df$sentence_ID %in% sub$sentence_ID,]
  
  #get p sample sentences
  for (j in 1:p){
    
    #get random starting point from sentences
    start<-sample(real.df$sentence_ID, 1)
    
    #then select S consecutive sentences
    samp.s<-seq(from=start, to=(start+(s-1)), by=1)
    tokens.s<-real.df[real.df$sentence_ID %in% samp.s,]
    
    #combine into a single passage
    passage<-paste(tokens.s$word, sep=" ", collapse=" ")
    
    #remove spaces between punctuation and n't
    passage<-str_replace_all(passage, "\\s+(?=\\p{P})", "")
    passage<-str_replace_all(passage, "\\s+(?=n’t\\b)", "")
    
    #append to final table
    fileID<-fn.s[i]
    temp.df<-data.frame(fileID,passage)
    final.df<-rbind(final.df, temp.df)
  }
}

############ Takes as input a single table with concatenated documents ############
a<-read.csv("cnn_booknlp_outputs_w_gender.csv")
a<-read.csv("fox_booknlp_outputs_w_gender.csv")

#get file names
files<-unique(a$filename)

#sample n passages
n=3
files.sub<-sample(files, n)

#define passage length
s=4

#run
final.df<-NULL
for (i in 1:length(files.sub)){
  sub<-a[a$filename == files.sub[i],]
  sent<-unique(sub$sentenceID)
  if(length(sent) > 10){
    start<-sample(1:(max(sent)-10), 1)
    sub2<-sub[sub$sentenceID %in% (seq(start, start+(s-1))),]  

    #combine into a single passage
    p<-paste(sub2$originalWord, sep=" ", collapse=" ")
    
    #remove spaces between punctuation and n't
    p<-str_replace_all(p, "\\s+(?=\\p{P})", "")
    p<-str_replace_all(p, "\\s+(?=n’t\\b)", "")
    
    #build table
    fileID<-files.sub[i]
    temp.df<-data.frame(fileID,p)
    final.df<-rbind(final.df, temp.df)
  }
}
all.df<-final.df
all.df<-rbind(all.df, final.df)


############ Takes as input a single directory with .tokens files ############
setwd("/Users/akpiper/Data/FanFic_A03/FanFic_A03_15K_Sample_Queer_Short_BookNLP")

#get file names
files<-list.files()

#sample n passages
n=75
files.sub<-sample(files, n)

#define passage length
s=4

#define number of passages per book
pass=1

#create empty final table
final.df<-NULL

#for every book
for (i in 1:length(files.sub)){
  
  print(i)
  
  #load tokens file
  sub<-read.csv(files.sub[i], quote="", sep="\t")
  
  for (j in 1:pass){
    
    sent<-unique(sub$sentenceID)
    
    if(length(sent) > 10){
      start<-sample(1:(max(sent)-10), 1)
      sub2<-sub[sub$sentenceID %in% (seq(start, start+(s-1))),]  
      
      #combine into a single passage
      p<-paste(sub2$originalWord, sep=" ", collapse=" ")
      
      #remove spaces between punctuation and n't
      p<-str_replace_all(p, "\\s+(?=\\p{P})", "")
      p<-str_replace_all(p, "\\s+(?=n’t\\b)", "")
      
      #build table
      fileID<-files.sub[i]
      temp.df<-data.frame(fileID,p)
      final.df<-rbind(final.df, temp.df)
    }
  }
}
write.csv(final.df, file="ValidationTable_Human_FanFic.csv", row.names = F)
