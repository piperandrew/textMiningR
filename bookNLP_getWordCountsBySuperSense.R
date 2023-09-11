#######  Get Token Counts by Supersense bookNLP  #########
#This gets word counts for all words associated with each supersense in bookNLP
#Helps identify most frequent words for each supersense given an input corpus

library(dplyr)

#set root working directory
wd.root<-c("/Users/akpiper/Data/CONLIT_NLP/")
setwd(wd.root)

#get list of folders (i.e. books)
filenames<-list.files()

#create empty final table
tokens.df<-NULL

#for every book
for (i in 1:length(filenames)){
  
  print(i)
  
  #setwd to the i-th book
  wd.file<-paste(wd.root, filenames[i], sep="")
  setwd(wd.file)
  
  #get list of files
  book.files<-list.files()
  
  #check to see if the directory has all necessary files
  if (length(book.files) == 6){
    
    #load specific files
    super<-book.files[grep(".supersense", book.files)]
    super.df<-read.csv(super, quote="", sep="\t")
    
    #summarize token counts by supersense category
    combined.df<-super.df %>% group_by(supersense_category, text) %>% count(name="Word_Count")
    
    if (i == 1){
    tokens.df<-combined.df
    }
    
    #add to meta table
    tokens.df<-full_join(tokens.df, combined.df, by=c("supersense_category", "text"))
    
    #remove NAs
    tokens.df[,3][is.na(tokens.df[,3])]<-0
    tokens.df[,4][is.na(tokens.df[,4])]<-0
    
    #sum columns
    tokens.df[,3]<-tokens.df[,3]+tokens.df[,4]
    
    #remove excess column
    tokens.df<-tokens.df[,-4]
    
    if (i==1){
      tokens.df[,3]<-tokens.df[,3]/2
    }
    
    #trim values of 1 after each merge
    if (i > 1){
    tokens.df<-tokens.df[tokens.df$Word_Count.x > (i/10),]
    }
  }
}
    
#condition on top N for each class
tokens.sub<-NULL
for (i in 1:nlevels(factor(tokens.df$supersense_category))){
  sub<-tokens.df[tokens.df$supersense_category == levels(factor(tokens.df$supersense_category))[i],]
  sub<-sub[order(-sub$Word_Count.x),]
  sub<-sub[1:30,]
  tokens.sub<-rbind(tokens.sub, sub)
}


