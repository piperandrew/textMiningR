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
nyt<-word2vec("NYT.txt", type = "cbow", window=5, threads=3, dim=50, min_count=10)

#save model
write.word2vec(nyt, "NYT.bin", type = "bin")

#read model
model<-read.word2vec("20CPOetryAll.bin", normalize = T) #normalize = T!!!

#create matrix
emb <- as.matrix(model)

#find nearest words
#top_n = the number of similar words to return
predict(model, c("human"), type = "nearest", top_n = 10)

#find similarity between two terms
word2vec_similarity(emb["human", ], emb["nature", ], top_n = 1)

#create artificial vectors by adding or subtracting word vectors
vector <- emb["human", ] - emb["animal", ]
predict(model, vector, type = "nearest", top_n = 10)



###### Method 2 ######
#Especially useful if models are .txt
#Also a function for exporting data

library("devtools")
library(magrittr)
library(wordVectors)
devtools::install_github("bmschmidt/wordVectors")
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

