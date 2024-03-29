######### Word Embeddings ###########
######### by Andrew Piper ####################
######### CC By 4.0 License ##################

#####  Method 1 ######
library(word2vec)

####  build model ####
#takes as input a single document of all documents from a directory that have been concatenated
#to generate a single doc from a directory of documents see Method 2 below
#type = word2vec model (cbow or skip-gram)
#window = skip length between words -- larger equals larger context of words that can be similar
#dim = dimensions of the word vectors, 50 is default, large models use 300
model<-word2vec("CONLIT_Cleaned.txt", type = "cbow", window=10, threads=3, dim=100, min_count=10)

#save model
write.word2vec(model, "CONLIT_Cleaned.bin", type = "bin")

#read model
model<-read.word2vec("NYT_Model.bin", normalize = T) #normalize = T!!!

#create matrix
emb <- as.matrix(model)

#find nearest words
#top_n = the number of similar words to return
predict(model, c("human"), type = "nearest", top_n = 10)

#create artificial vectors by adding or subtracting word vectors
vector <- emb["human", ] - emb["animal", ]
predict(model, vector, type = "nearest", top_n = 10)



###### Method 2 ######
#Especially useful if models are .txt
#Also a function for exporting data

library("devtools")
library(magrittr)
library(wordVectors)
#devtools::install_github("bmschmidt/wordVectors")
remotes::install_github("bmschmidt/wordVectors", ref="lite")

#export directory of txt files as a single txt file to build model
#first value is directory, second is output file
prep_word2vec("Novel_English_19C","Novel_English_19C.txt",lowercase=T) 

#import .txt model
#sample from https://nlp.stanford.edu/projects/glove/
model<-read.vectors("glove.6B.100d.txt")

#find nearst words
nearest_to(model, model[[c("frog")]], 10)
nearest_to(model, model[[c("frog", "frogs")]], 10)
nearest_to(model,model[["girl"]]-model[["boy"]])
cosineSimilarity(model[["rock"]], model[["boulder"]])
cosineSimilarity(model[[c("rock", "rocks")]], model[["lamp"]])

cosine(emb[rownames(emb)=="human",], emb[rownames(emb)=="humans",])

cosineSimilarity(emb["rock",], emb["boulder",])


####### Prepare texts for making model in Terminal #######
#Takes as input all .txt files in a directory you are in and outputs them to a folder
#in that directory called "combined"
cat *.txt > combined/combined.txt

####### Prepare Texts for Making Model by first cleaning them #######
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

setwd("/Users/akpiper/Data/")
meta<-read.csv("CONLIT_META.csv")

setwd("/Users/akpiper/Data/CONLIT/")

fn<-list.files()
fn<-fn[fn %in% meta$ID[meta$Category == "FIC"]]

for (i in 1:length(fn)){
  
  print(i)
  
  #ingest and clean work
  work<-text.prep(fn[i])
  
  #paste
  
  #change wd 
  
  #write as .txt file
  
}
  



