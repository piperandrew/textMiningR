###################################################
############ Calculate Words per book #############
###################################################

#text cleaning function
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

#get filenames in a directory
filenames<-list.files()

#create final word count vector
length.v<-vector(mode="numeric", length=length(filenames))

#for each file
for (i in 1:length(filenames)){
  print(i)
  length.v[i]<-length(text.prep(filenames[i]))
}
