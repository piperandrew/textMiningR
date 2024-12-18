#######################################
###########  BERT with R  #############
#######################################
#this script facilitates the use of BERT for text analysis

############ Sentence Vectorization ###############
#takes as input a table of sentences
#loads a BERT model for vectorizing each sentence
#outputs a document-term-matrix, where rows = sentences, columns = features

library(tidyverse)
library(reticulate)
library(LiblineaR)
library(tidymodels)
library(rsample)

#set python environment
#use_python("/Users/akpiper/Library/r-miniconda-arm64/envs/r-reticulate/bin/python", required = TRUE)

#set R environment
use_condaenv("r-reticulate", required = TRUE)# Ensure the correct conda environment is active

#prepare Python-R interaction (good luck!)
reticulate::py_install('transformers', pip = TRUE)
transformer = reticulate::import('transformers')
tf = reticulate::import('tensorflow')
builtins <- import_builtins()

#establish tokenizer
tokenizer <- transformer$AutoTokenizer$from_pretrained('bert-large-cased') #bert-base-uncased #bert-large-uncased

#load sentences
lit<-read_csv("CHICAGO_GENERICS_ALL_FINAL.csv")

#clean (lowercase, remove punctuaton and numbers)
#lit <- lit %>% mutate(gpt= tolower(gpt))
#lit <- lit %>% mutate(gpt=gsub("[^[:alnum:][:space:].]", "",substr(gpt,1,nchar(gpt)-1)))

#get sentences as single vector
train_texts <- lit %>% select(sentences) %>% pull()
#train_texts <-train_texts[1:10]

#load model
train_encodings = tokenizer(train_texts, truncation=TRUE, padding=TRUE,max_length=250L)
BERT = transformer$TFBertModel$from_pretrained("bert-large-cased") #bert-base-uncased #bert-large-uncased

#prepare
ntexts_train = length(train_texts)
no.cols<-1024 #base = 768 #large = 1024
features_train = matrix(NA, nrow=ntexts_train, ncol=no.cols)

#Build document term matrix
for (i in 1:(ntexts_train)){
  print(i)
  encodings_i = tokenizer(train_texts[i], truncation=TRUE, padding=TRUE,max_length=250L, return_tensors='tf')
  features_train[i,] = py_to_r(array_reshape(BERT(encodings_i)[[1]][[0]][[0]],c(1, no.cols)))
}

# Convert to data frame
dtm <- as.data.frame(features_train)
colnames(dtm) <- paste0("Feature_", 1:no.cols)

# Save the matrix as a .csv file
write_csv(dtm, "document_term_matrix.csv")

# #Clustering
# install.packages("umap")
# library(umap)
# 
# # UMAP settings
# umap_config <- umap.defaults
# umap_config$n_neighbors <- 15  # You can adjust this
# umap_config$min_dist <- 0.1    # Lower values = more detailed clustering
# umap_config$n_components <- 100 # Reducing to 100 dimensions
# 
# # Apply UMAP on the dtm
# umap_reduced <- umap(features_train, config = umap_config)
# 
# #HDBSCAN
# install.packages("dbscan")
# 
# # Load the package
# library(dbscan)
# 
# # 'minPts' is the minimum number of points to form a cluster (you can experiment with this parameter)
# hdbscan_result <- hdbscan(pca, minPts = 30)
# 
# # The clustering labels will be in hdbscan_result$cluster
# # -1 indicates noise (points that don't belong to any cluster)
# test<-table(hdbscan_result$cluster)
