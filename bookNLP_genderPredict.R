#########     Add predicted gender to bookNLP data   ####################
#########     by Andrew Piper                        ####################
#########     CC By 4.0 License                      ####################

#This script offers two methods for predicting the gender of characters in bookNLP data
#Note that the newer version of bookNLP contains an option to do this already

########         Pronoun Method              ######################
########   for directory of multiple files   ######################

#Takes as input a directory of bookNLP tokens tables
#Outputs same table with a new column called "gender" that predicts the gender
#of each characterID. All instances of that characterID are labeled.
#If the character has multiple sequential names (Al Capone) then only the
#first instance is tagged to avoid over-counting.

#NOTE: toggle read.csv() function based on comma or tab separated files

setwd("")

#get list of files in target directory
f.names<-list.files()

#REQUIRES you to set working directory 2x -- Input and Output folders

#for every file
for (i in 1:length(f.names)){
  
  #****Set working directory here and at end of code
  setwd("~/Desktop/JUV_BookNLP/MIDDLE_21C_bookNLP")
  
  print(i)
  
  #ingest each file - IF tab separated
  a<-read.csv(f.names[i], sep="\t", quote = "", stringsAsFactors = F)
  
  #ingest each file - IF comma separated 
  #a<-read.csv(f.names[i], stringsAsFactors = F)
  
  #create empty column for predicted genders
  a$gender<-vector(mode="numeric", length=nrow(a))
  
  #subset by characters
  char.v<-unique(a$characterId[a$characterId > -1])
  
  #for each character
  for (j in 1:length(char.v)){
    
    #subset by character
    char<-a[a$characterId == char.v[j],]
    
    #get pronoun totals
    he<-length(char$lemma[char$lemma == "he"])
    she<-length(char$lemma[char$lemma == "she"])
    
    #check if they are not empty
    if (he+she > 0){
      if (he > she){
        a$gender[a$characterId == char.v[j]]<-c("male")
      } else {
        a$gender[a$characterId == char.v[j]]<-c("female")
      }
    }
  }
  
  #remove repeated labels
  for (k in 2:nrow(a)){
    if (a$gender[k] != 0){
      if (a$gender[k] == a$gender[k-1]){
        a$gender[k]<-c("0")
      }
    }
  }
  
  #write new file
  #*****Set working directory to OUTPUT FILE
  setwd("~/Desktop/JUV_BookNLP/JUV_NEW")
  write.csv(a, file=f.names[i], row.names = F)
}
  
#######################################################################
############         Lexicon Method              ######################
#######################################################################

#this applies the gender package to bookNLP data
#it takes as input a .tokens table and outputs a new column with the predicted gender
#you can add columns for the probability of the predicted gender if you wish
#runtime = extremely slow!

library(gender)
library(remotes)
remotes::install_github("lmullen/genderdata")

setwd("")

#ingest bookNLP table
a<-read.csv("")

#create empty column for predicted genders
a$gender<-vector(mode="numeric", length=nrow(a))

#establish year or year range for data
year<-2012

#for every token
for (i in 2:nrow(a)){
  #check to see if it is a person
  if (a$ner[i] == "PERSON"){
    #then check to see if the prior token is a person -- if so then skip [because it is a sequential name]
    if (a$ner[i-1] != "PERSON"){
      #measure progress
      #print(i)
      
      #predict gender using social security records
      #you can change "method" based on historical date range of your data
      g<-gender(a$originalWord[i], years=year, method="ssa")[[4]]
      
      #check to see if there was a match
      if (length(g)>0){
        a$gender[i]<-g
      }
    }
  }
}



