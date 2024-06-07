######### Select Random Sentences with Characters ####################
######### by Andrew Piper ####################
######### CC By 4.0 License ##################
library(stringr)

#this script takes as input a directory of bookNLP files
#it outputs a random sample of sentences into a table
#it has the following conditions
#a. only keeps named characters
#b. who are in the subject position
#c. in sentences not in dialogue (optional)

setwd("/Users/akpiper/Data")
meta<-read.csv("CONLIT_META.csv")

#set root working directory
wd.root<-c("/Users/akpiper/Data/CONLIT_NLP/")

setwd(wd.root)

#get list of folders (i.e. books)
filenames<-list.files()

#OPTION: subset by metadata
file.sub<-sub("\\..*$", "", filenames)
#fiction
file.fic<-sub("\\..*$", "", meta$ID[meta$Category == "FIC"])
file.sub<-file.sub[file.sub %in% file.fic]
#3P
file.3p<-sub("\\..*$", "", meta$ID[meta$Probability1P < 0.05])
file.sub<-file.sub[file.sub %in% file.3p]
#remove ROM
file.rom<-sub("\\..*$", "", meta$ID[meta$Genre == "ROM"])
file.sub<-file.sub[!file.sub %in% file.rom]
file.sub<-unique(file.sub)

#establish parameters

#sample n documents
n<-20
fn.s<-sample(file.sub, n)
#fn.s<-file.sub

#set number of context sentences prior to target sentence with character
c=1
#set number of following sentences (c+s+1 = total sentences per passage)
s=3
#set max passage length in words
mx=125
#set number of repetitions per character per passage
max.repeat<-2

#select number of passages to extract per book (of s sequential sentences)
pass<-1

#supersense category to sample from
cat<-c("verb.emotion", "noun.feeling")

#create empty final table
final.df<-NULL

#for every book
for (i in 1:length(fn.s)){
  
  print(i)
  
  #load tokens file
  tokens.df<-read.csv(paste(fn.s, ".tokens", sep="")[i], quote="", sep="\t")
  
  #load dialogue table
  quotes.df<-read.csv(paste(fn.s, ".quotes", sep="")[i], quote="", sep="\t")
  
  #load supersense table
  super.df<-read.csv(paste(fn.s, ".supersense", sep="")[i], quote="", sep="\t")
  
  #load character table
  char.df<-read.csv(paste(fn.s, ".entities", sep="")[i], quote="", sep="\t")
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
  
  ### remove sentences in dialogue ###
  #get all sentences with dialogue
  #sequences <- unlist(mapply(seq, from=quotes.df$quote_start, to=quotes.df$quote_end))
  #dia.df<-tokens.df[tokens.df$token_ID_within_document %in% sequences,]
  #subset tokens table by removing these sentences
  #tokens.df2<-tokens.df2[!tokens.df2$sentence_ID %in% dia.df$sentence_ID,]
  
  ### subset by supersense ###
  #subset by target supersense(s)
  super.df<-super.df[super.df$supersense_category %in% cat,]
  #get sentence IDs
  super.sent<-tokens.df$sentence_ID[tokens.df$token_ID_within_document %in% super.df$start_token]
  #subset tokens table by removing these sentences
  tokens.df2<-tokens.df2[tokens.df2$sentence_ID %in% super.sent,]
  
  for (j in 1:pass){
    
    for (j in 1:200){
      #get random starting point from sentences
      start<-sample(unique(tokens.df2$sentence_ID), 1)
      #start c sentences prior to target sentence and s sentences after
      samp.s<-seq(from=(start-c), to=(start+s), by=1)
      tokens.s<-tokens.df[tokens.df$sentence_ID %in% samp.s,]
      
      #### no dialogue condition ####
      #check if those sentences are in dialogue table, resample if so
      #if (length(which(unique(tokens.s$sentence_ID) %in% dia.df$sentence_ID)) == 0){
      #  break
      #}
      
      ### minimum character count condition ####
      #get char.id
      char.id<-char.df$COREF[char.df$start_token %in% tokens.df2$token_ID_within_document[tokens.df2$sentence_ID == start]][1]
      #get all chars for that passage
      char.sub<-char.df[char.df$start_token %in% tokens.s$token_ID_within_document,]
      #table character Ids
      char.list<-table(char.sub$COREF)
      #subset by char.id
      char.list<-char.list[names(char.list) == char.id]
      #main character must appear at least 2 times
      if (unname(char.list) > max.repeat & length(tokens.s) < mx){
        break
      }
    }
  
    #get character name
    character<-char.df$text[char.df$start_token %in% tokens.df2$token_ID_within_document[tokens.df2$sentence_ID == start]][1]
    char.id<-char.df$COREF[char.df$start_token %in% tokens.df2$token_ID_within_document[tokens.df2$sentence_ID == start]][1]
    
    #combine into a single passage
    p<-paste(tokens.s$word, sep=" ", collapse=" ")
    
    #remove spaces between punctuation and n't
    p<-str_replace_all(p, "\\s+(?=\\p{P})", "")
    p<-str_replace_all(p, "\\s+(?=n’t\\b)", "")
    
    #build table
    fileID<-fn.s[i]
    temp.df<-data.frame(fileID,p, character)
    final.df<-rbind(final.df, temp.df)
  }
}

#add id to each row
#library(stringi)
#final.df$ID<-paste(stri_rand_strings(nrow(final.df), 6, pattern = "[A-Za-z0-9]"), ".txt", sep="")

#create new column for Lighttag
final2<-final.df
final2$combined<-paste(final2$character, final2$p, sep="\n\n")

#save
write.csv(final.df, file="Emotions02.csv", row.names = F)


#write texts to files
setwd("/Users/akpiper/Research/Character Cognition/CharacterCognition_Test")

for (i in 1:nrow(final.df)){
  write(final.df[i,2], file=final.df$ID[i])
}



