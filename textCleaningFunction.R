################################################
#######   Text Cleaning Function   #############
######### by Andrew Piper          #############
######### CC By 4.0 License        #############
################################################

#takes as input a .txt file or vector of filenames
text.prep<-function(x){
  #first scan in the document
  work<-scan(x, what="character", quote="", quiet=T)
  #remove numbers
  work<-gsub("\\d", "", work)
  #split on punctuation
  work<-unlist(strsplit(work,"[[:punct:]]"))
  #make all lowercase
  work<-tolower(work)
  #remove blanks
  work<-work[work != ""]
}

#example
work.v<-text.prep(file.names[1]) #stores cleaned work from first position of a vector of filenames as a vector
