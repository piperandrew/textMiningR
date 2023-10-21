#### Shortest Distance Functions #####

#### OLD ######

#Physical Distance
pathway.df<-NULL
for (j in 1:nrow(places)){
  #get geo ID for country
  #geo_id<-country$geonameid[country$Country == places$Element[j]]
  #x<-geo[geo$V1 == geo_id,5]
  #y<-geo[geo$V1 == geo_id,6]
  #if 0 then get lat/long for city
  if (length(geo_id) == 0){
    sub<-city[city$V3 == places$Element[j],]
    #if no match try grep
    if (nrow(sub) == 0){
      sub<-city[grep(places$Element[j], city$V4),]
    }
    #if more than 1 match
    if (nrow(sub) > 1){
      #take city with largest population
      sub<-sub[sub$V15 == max(sub$V15),]
    }
    #store lat/long
    x<-sub$V5
    y<-sub$V6
  }
  #check to make sure there is a match
  if (length(x) != 0){
    place<-places$Element[j]
    temp.df<-data.frame(place,x,y)
    pathway.df<-rbind(pathway.df, temp.df)
  }
  
}

#load geo info
#country<-read.csv("allCountries_Countries.txt", sep="\t", quote="")
#city<-read.csv("allcities15000.txt", sep="\t", quote="", header=F)

#create function to extract all places in sequence
splitElements <- function(input_column) {
  # Remove the square brackets at the beginning and end of each string in the column
  cleaned_text <- gsub("^\\[|\\]$", "", input_column)
  
  # Split each string into a list of elements based on ', ' and remove single quotes
  elements_list <- lapply(strsplit(cleaned_text, "', '"), function(x) gsub("'", "", x))
  
  # Convert the list of elements into a data frame
  result_df <- data.frame(Element = unlist(elements_list))
  
  return(result_df)
}

#function to compute distance of a route
distRoute <- function(adjmat, route) {
  #route<-route[route %in% colnames(adjmat)]
  d <- 0
  for(n in 2:nrow(adjmat)) {
    d <- d + adjmat[route[n-1],route[n]]
  }
  return(d)
}

distRoute2 <- function(adjmat, route) {
  route<-route[route %in% colnames(adjmat)]
  d <- 0
  for(n in 2:nrow(adjmat)) {
    d <- d + adjmat[route[n-1],route[n]]
  }
  return(d)
}

#function for cleaning the 

# example
#route1 <- c("A","C","E","B","D","A")
#distRoute(adjmat, route1)

#2-opt traveling salesman algorithm
randomRoute <- function(cities) {
  start <- cities[1]
  route <- sample(cities[-1])
  route <- c(start,route,start) # return back home
  return(route)
}

swap <- function(route,i,k) {
  new_route <- route[1:(i-1)]
  new_route <- c( new_route, route[k:(i)] )
  new_route <- c( new_route, route[(k+1):length(route)] )
  return(new_route)
}

# two-opt algorithm
twoOpt <- function(cities, sleep=.05) {
  
  # compute adjacency matrix
  #adjmat <- compAdjMat(cities)
  
  adjmat<- compute_distances(cities, glove_embeddings)
  
  # start with random route
  best_route <- randomRoute(cities)
  min_d <- distRoute2(adjmat,best_route)
  
  # variable tracking
  track_dist <- c()
  
  # while-loop to perform complete 2-opt swap
  while(T) { 
    
    # record distance before looping through i and k
    old_d <- min_d
    
    # for-loops through values for i and k
    break_loop <- F
    for(i in 2:length(cities)) {
      for(k in (i+1):length(cities)) {
        
        # perform swap
        route <- swap(best_route,i,k)
        new_d <- distRoute2(adjmat,route)
        
        # update distance and plot
        if(new_d < min_d) {
          min_d <- new_d
          best_route <- route
          #plotRoute(cities, route, min_d)
          Sys.sleep(sleep)
          break_loop <- T
          break() # break out of inner loop
        } # end outer if-statement
        
      } # close inner loop
      
      # break out of outer loop
      if(break_loop) break()
      
    } # close outer loop
    
    # update on variable tracking
    track_dist <- c(track_dist,new_d)
    
    # check if the for-loops made any improvements
    if(old_d == new_d) break() # break out of while loop
    
  } # close while loop
  
  # return
  return(list(distance = min_d,
              route = best_route,
              track_dist = track_dist)
  )
  
}

#remove stopwords
remove_words <- function(text_vector, remove) {
  # Construct a single regex pattern that matches any word in the remove vector
  pattern <- paste0("\\b(", paste(remove, collapse="|"), ")\\b")
  
  # Replace matched words with empty string
  cleaned_text <- gsub(pattern, "", text_vector)
  
  # Remove leading and trailing whitespace
  return(trimws(cleaned_text))
}


##### Semantic Distance #######

#load stopwords
library(tm)
stop<-stopwords("en")
stop<-unlist(strsplit(stop,"[[:punct:]]"))
stop<-unique(stop)

#load embedding model
setwd("/Users/akpiper/Data/glove.6B")
glove_embeddings<-read.csv("glove.6B.100d.txt", sep=" ", quote="", header=F)
row.names(glove_embeddings)<-glove_embeddings[,1]
glove_embeddings<-glove_embeddings[,-1]

#function for getting pairwise distances for all words and phrases in a vector
library(data.table)
library(proxy)

# Function to get embeddings
# For each element of a vector this function:
#a. gets the embedding for a single word
#b. takes the average embedding for word phrases of all word vectors in the embedding
get_embedding <- function(token, glove_embeddings) {
  if (!grepl(' ', token)) {
    if (token %in% row.names(glove_embeddings)) {
      return(glove_embeddings[which(row.names(glove_embeddings) %in% token),])
    } else {
      return(NULL)
    }
  }
  
  words <- unlist(strsplit(token, ' '))
  valid_embeddings <- lapply(words, function(word) {
    if (word %in% row.names(glove_embeddings)) {
      return(glove_embeddings[which(row.names(glove_embeddings) %in% word),])
    } else {
      return(NULL)
    }
  })
  
  # Remove NULLs
  valid_embeddings <- Filter(Negate(is.null), valid_embeddings)
  if (length(valid_embeddings) == 0) {
    return(NULL)
  }
  
  avg_embedding <- colMeans(do.call(rbind, valid_embeddings))
  return(avg_embedding)
}

#Function to compute pairwise distance matrix
compute_distances <- function(word_list, glove_embeddings) {
  embeddings_list <- lapply(word_list, get_embedding, glove_embeddings=glove_embeddings)
  embeddings_list <- setNames(lapply(word_list, get_embedding, glove_embeddings=glove_embeddings), word_list)
  embeddings_list <- Filter(Negate(is.null), embeddings_list)
  
  # Convert list of embeddings to matrix
  embedding_matrix <- do.call(rbind, embeddings_list)
  
  # Compute pairwise cosine similarity
  distance_matrix <- as.matrix(dist(embedding_matrix, method="cosine"))
  
  return(distance_matrix)
}
