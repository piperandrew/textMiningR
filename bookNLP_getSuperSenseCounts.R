###### get bookNLP supersense counts from corpus ######
#INPUT: directory of bookNLP tables
#OUPUT: single table of supersense counts by book

library(dplyr)

#set root working directory
wd.root<-c("/Users/akpiper/Data/CONLIT_NLP/")
setwd(wd.root)

#get list of folders (i.e. books)
filenames<-list.files()

#create empty final table
final.df<-NULL

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
    
    #load supersense file
    super<-book.files[grep(".supersense", book.files)]
    super.df<-read.csv(super, quote="", sep="\t")
    
    #table supersense types
    super.table<-table(super.df$supersense_category)
    
    #normalize by word count
    super.table<-super.table/max(super.df$end_token)
    
    #turn into data frame
    super.table<-as.data.frame(super.table)
    
    if (i == 1){
      final.df<-super.table
    } else {
    #add to meta table
    final.df<-merge(final.df, super.table, by=c("Var1"))
    }
    #rename columns
    colnames(final.df)[i+1]<-filenames[i]
  }
}

#transpose columns to rows
df<-as.data.frame(t(final.df))
colnames(df)<-df[1,]
df<-df[-1,]
write.csv(df, file="superSense.csv", row.names = T)
