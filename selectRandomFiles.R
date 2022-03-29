######### Select Random Files   ####################
######### by Andrew Piper ####################
######### CC By 4.0 License ##################

#this script takes as input a directory of text files
#it outputs a random sample of files from that directory into a new directory
#you can add cleaning steps if you want to alter the files

#set working director where your source texts are located
#source.texts<-c("~/Desktop/LordOfTheRings_FanFic")
source.texts<-c("/Volumes/Samsung_T5/Peter_Stuff/FANFICTION/FINAL_DATASET/texts")
target.texts<-c("~/Desktop/FanFic")
setwd(source.texts)

#get list of files
fn<-list.files()

#sample n files
n<-50
fn.s<-sample(fn, n)

#or get list of files in table
t<-read.csv("")
final.df1$filename<-paste(final.df1$id, ".txt", sep="")
fn.s<-fn[fn %in% final.df1$filename]

#loop through ingest and write
for (i in 1:length(fn.s)){
  
  print(i)
  
  #setwd to source texts directory
  setwd(source.texts)
  #ingest
  work<-scan(fn.s[i], what="character", quote="", quiet=T)
  
  ### insert cleaning steps here ###
  #remove numbers
  #work<-gsub("\\d", "", work)
  #split on punctuation
  #work<-unlist(strsplit(work,"[[:punct:]]"))
  #make all lowercase
  #work<-tolower(work)
  #remove blanks
  #work<-work[work != ""]
  
  #paste as single string
  work.all<-paste(work, sep=" ", collapse=" ")
  #set working directory to target directory
  setwd(target.texts)
  #write
  write(work.all, file=fn.s[i])
}
