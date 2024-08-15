#########   Chunk Texts         ###################
#########   by Andrew Piper     ###################
#########   CC By 4.0 License   ###################

#This splits texts into uniform length chunks
#Default length = 1000 words
#takes a directory of .txt files as input
#works are "cleaned" prior to chunking:
#-punctuation removed
#-numbers removed
#-all lowercase

#create a function that reads and cleans texts
text.prep<-function(x){
  
  #scan in the document
  work<-scan(x, what="character", quote="", quiet=T)
  
  #remove numbers
  work<-gsub("\\d", "", work)
  
  #remove punctuation
  #work<-gsub("[[:punct:]]", " ", work)
  work<-unlist(strsplit(work, "[[:punct:]]"))
  
  #make all lowercase
  work<-tolower(work)
  
  #remove blanks
  work<-work[work != ""]
}

#set the chunk size
chunk<-500

#setwd where your .txt files are stored 
setwd("")

#get filenames from directory where files are stored
f.names<-list.files()

#for evey file ingest, clean, chunk, rename, and export
for (i in 1:length(f.names)){
  
  #INPUT FOLDER
  setwd("")
  
  #measure time to completion
  print(i)
  
  #ingest and clean each text
  work.v<-text.prep(f.names[i])
  
  #OUTPUT FOLDER NAME - you must create in advance
  setwd("")
  
  #set file counter to 0
  n=0
  
  #go through document and divide into equal sized chunks
  
  for (j in seq(from=1, to=length(work.v)-chunk, by=chunk)){
    #set counter
    n=n+1
    
    #create chunk
    sub<-work.v[j:(j+(chunk-1))]
    
    #collapse into a single paragraph
    sub<-paste(sub, collapse = " ")
    
    #create unique filename like this: filename_001.txt
    new.name<-gsub(".txt", "", f.names[i])
    new.name<-paste(new.name, sprintf("%03d", n), sep="_")
    new.name<-paste(new.name, ".txt", sep="")
    
    #write to output folder
    write(sub, file=new.name)
  }
}

