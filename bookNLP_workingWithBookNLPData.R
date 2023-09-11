############################################
###### Working with BookNLP Outputs ########
############################################

#set root working directory
wd.root<-c("~/Data/txtlab_CONLIT_NLP/")
setwd(wd.root)

#get list of folders (i.e. books)
filenames<-list.files()

#################################################
###### Measuring number of characters ###########
#################################################
#Takes as input the .entities file produced by bookNLP
#https://github.com/booknlp/booknlp

#Outputs: 
#A. character counts per book
#B. Concentration ratio of main character

#create empty final table
count.df<-NULL
#for every book
for (i in 1:length(filenames)){
  
  print(i)
  
  #setwd to the i-th book
  wd.file<-paste(wd.root, filenames[i], sep="")
  setwd(wd.file)
  
  #get list of files
  book.files<-list.files()
  
  #check to see if the directory has all necessary files
  if (length(book.files) < 6){
    
    #filename
    fileID<-filenames[i]
    #dummy variables
    no.chars<-0
    cr1<-0
    #store as data frame
    temp.df<-data.frame(fileID, no.chars, cr1)
    count.df<-rbind(count.df, temp.df)
  } else {

  #load specific files
  
  #entities
  ent<-book.files[grep(".entities", book.files)]
  ent.df<-read.csv(ent, quote="", sep="\t")
  
  #calculate total number of characters
  #keep only persons
  char.df<-ent.df[ent.df$cat == "PER",]
  #remove pronouns and non-proper names
  char.df<-char.df[char.df$prop == "PROP",]
  
  #remove only pronouns
  #char.df<-char.df[char.df$prop != "PRON",]
  
  #get length of ALL unique character IDs
  char.total<-length(unique(char.df$COREF))
  #table and sort
  char.sort<-sort(table(char.df$COREF), decreasing = T)
  #truncate by N mentions or more
  char.n<-char.sort[unname(char.sort) > 5]

  #inspect names
  #char.n.df<-char.df[char.df$COREF %in% names(char.n),]
  #char.n.df<-char.n.df[!duplicated(char.n.df$COREF),]
  
  #get final count of total characters
  no.chars<-length(char.n)
  
  #get CR1
  cr1<-unname(char.n)[1]/sum(unname(char.n))
  
  #filename
  fileID<-filenames[i]
  
  #store as data frame
  temp.df<-data.frame(fileID, no.chars, cr1)
  count.df<-rbind(count.df, temp.df)
  }
}

  
  