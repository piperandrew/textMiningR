####### Collocate Analysis ########

#This script:
#Ingests txt files in a directory 1-by-1
#Input "keyword" of interest
#Input collocate window +/- N words (recommended: either 1 or 9)
#Extracts all collocated words within the window and creates a table of those words
#Normalizes counts by PMI (Pointwise Mutual Information)

#set your working directory to where your txt files are located
setwd("~/Data/txtlab_Novel150_English")

#get list of files in target directory
f.names<-list.files()

#we're going to create a function that reads in the texts, cleans them,
#and stores them in a variable called "work"
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

#define keyword
keyword<-c("love")

#define window +/-
n<-9

#create an empty vector where you will save your results
collocate.v<-NULL
#create another empty vector to save word counts for PMI normalization
word.count.v<-NULL
#create a loop that is as long as the number of documents
for (i in 1:length(f.names)){
  #see how fast things are going
  print(i)
  #ingest and clean each text
  work.v<-text.prep(f.names[i])
  
  #store all words to word count vector
  word.count.v<-append(word.count.v, work.v)
  
  #create index of locations of keyword in the vector
  key.index<-which(work.v == keyword) # position of keyword
  #only continue if the keyword is in the text
  if (length(key.index) > 0){
    #create empty table to store collocates for each work
    #for every occurrence of keyword
    for (j in 1:length(key.index)){
      #make sure keyword is greater than window away from beginning or end of text
      if (key.index[j]-n > 1 & key.index[j]+n < length(work.v)){
        #get all words prior to and after keyword up to window
        before<-work.v[(key.index[j]-n):(key.index[j]-1)]
        after<-work.v[(key.index[j]+1):(key.index[j]+n)]
        #combine
        col<-c(before, after)
        #store in a single vector
        collocate.v<-append(collocate.v, col)
      }
    }
  }
}

#Create table of counts
collocate.df<-data.frame(sort(table(collocate.v), decreasing = T))
word.count.df<-data.frame(sort(table(word.count.v), decreasing = T))

##### Normalize by PMI (Pointwise Mutual Information) #######

#PMI = log(P(x,y)/P(x)*P(y))
#where 
#P(x,y) = count of collocate / all words
#P(x) = count of collocate's total word count / all words
#P(y) = count of keyword / all words

#remove words that appear less than m times
m<-50
collocate.sub<-collocate.df[collocate.df$Freq > m,]

#for every word
collocate.sub$pmi<-vector(mode="numeric",length=nrow(collocate.sub))
for (i in 1:nrow(collocate.sub)){
  #collocate count P(x,y)
  xy<-collocate.sub[i,2]/length(word.count.v)
  #count of collocate's total word count P(x)
  x<-word.count.df[as.character(word.count.df$word.count.v) == as.character(collocate.sub$collocate.v)[i],2]/length(word.count.v)
  #count of keyword's total word count P(y)
  y<-word.count.df[as.character(word.count.df$word.count.v) == keyword,2]/length(word.count.v)
  collocate.sub$pmi[i]<-log(xy/(x*y))
}
#sort by pmi
collocate.sub<-collocate.sub[order(-collocate.sub$pmi),]


##########     CORPUS #2     ############
#set your working directory to where your txt files are located
setwd("~/Data/gut_ChildrenStories")

#get list of files in target directory
f.names2<-list.files()

#create an empty vector where you will save your results
collocate.v2<-NULL
#create another empty vector to save word counts for PMI normalization
word.count.v2<-NULL
#create a loop that is as long as the number of documents
for (i in 1:length(f.names2)){
  #see how fast things are going
  print(i)
  #ingest and clean each text
  work.v<-text.prep(f.names2[i])
  
  #store all words to word count vector
  word.count.v2<-append(word.count.v2, work.v)
  
  #create index of locations of keyword in the vector
  key.index<-which(work.v == keyword) # position of keyword
  #only continue if the keyword is in the text
  if (length(key.index) > 0){
    #create empty table to store collocates for each work
    #for every occurrence of keyword
    for (j in 1:length(key.index)){
      #make sure keyword is greater than window away from beginning or end of text
      if (key.index[j]-n > 1 & key.index[j]+n < length(work.v)){
        #get all words prior to and after keyword up to window
        before<-work.v[(key.index[j]-n):(key.index[j]-1)]
        after<-work.v[(key.index[j]+1):(key.index[j]+n)]
        #combine
        col<-c(before, after)
        #store in a single vector
        collocate.v2<-append(collocate.v2, col)
      }
    }
  }
}

#Create table of counts
collocate.df2<-data.frame(sort(table(collocate.v2), decreasing = T))
word.count.df2<-data.frame(sort(table(word.count.v2), decreasing = T))

##### Normalize by PMI (Pointwise Mutual Information) #######

#PMI = log(P(x,y)/P(x)*P(y))
#where 
#P(x,y) = count of collocate / all words
#P(x) = count of collocate's total word count / all words
#P(y) = count of keyword / all words

#remove words that appear less than m times
m<-50
collocate.sub2<-collocate.df2[collocate.df2$Freq > m,]

#for every word
collocate.sub2$pmi<-vector(mode="numeric",length=nrow(collocate.sub2))
for (i in 1:nrow(collocate.sub2)){
  #collocate count P(x,y)
  xy<-collocate.sub2[i,2]/length(word.count.v2)
  #count of collocate's total word count P(x)
  x<-word.count.df2[as.character(word.count.df2$word.count.v2) == as.character(collocate.sub2$collocate.v2)[i],2]/length(word.count.v2)
  #count of keyword's total word count P(y)
  y<-word.count.df2[as.character(word.count.df2$word.count.v2) == keyword,2]/length(word.count.v2)
  collocate.sub2$pmi[i]<-log(xy/(x*y))
}
#sort by pmi
collocate.sub2<-collocate.sub2[order(-collocate.sub2$pmi),]


###### Undertake Comparison Using Vector Subtraction #######

#rename and unfactor columns
colnames(collocate.sub)<-c("word", "freq", "pmi")
colnames(collocate.sub2)<-c("word", "freq", "pmi")
collocate.sub$word<-as.character(collocate.sub$word)
collocate.sub2$word<-as.character(collocate.sub2$word)

#merge your PMI vectors
collocates.all<-merge(collocate.sub, collocate.sub2, by="word", all=TRUE)
collocates.all[is.na(collocates.all)]<-0

#subtract in each direction and observe differences
collocates.all$x_minus_y<-collocates.all$pmi.x-collocates.all$pmi.y
collocates.all$y_minus_x<-collocates.all$pmi.y-collocates.all$pmi.x

#sort by each column
#x minus y
collocates.all<-collocates.all[order(-collocates.all$x_minus_y),]
#y minus x
collocates.all<-collocates.all[order(-collocates.all$y_minus_x),]

######## For SHORTER documents ###################
######## Within Document Collocation ############# 
#for Documents that are short you can measure "collocate" as collocated in 
#the same document. When a poem uses a keyword X, what other words does it use?
#You can also shorten longer documents by chunking them and then using this approach.
library(tm)
library(infotheo)
corpus1 <- VCorpus(DirSource("20thCPoetry"), readerControl=list(language="English"))
corpus1 <- tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
f<-content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus1 <- tm_map(corpus1, f, "[[:punct:]]")
corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace)) 
corpus1.dtm<-DocumentTermMatrix(corpus1)
dtm.sparse<-removeSparseTerms(corpus1.dtm, 0.99)
sparse.m<-as.matrix(dtm.sparse)
stop<-stopwords("en")
stop<-unlist(strsplit(stop,"[[:punct:]]"))
stop<-unique(stop)
sparse.m<-sparse.m[,!colnames(sparse.m) %in% stop]

#run Mutual Information on keyword relative to all words in the corpus
keyword<-c("love")
pmi.df<-NULL
for (i in 1:ncol(sparse.m)){
  pmi<-mutinformation(sparse.m[,which(colnames(sparse.m) == keyword)], sparse.m[,i], method="emp")
  temp.df<-data.frame(colnames(sparse.m)[i], pmi)
  pmi.df<-rbind(pmi.df, temp.df)
}
pmi.final.source<-pmi.df[order(-pmi.df[,2]),]
pmi.final.source<-pmi.final.source[-1,]

#run for second corpus and compare as above
corpus2 <- VCorpus(DirSource("Pop_Songs_Sample"), readerControl=list(language="English"))
corpus2 <- tm_map(corpus2, content_transformer(tolower))
corpus2 <- tm_map(corpus2, content_transformer(removeNumbers))
f<-content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus2 <- tm_map(corpus2, f, "[[:punct:]]")
corpus2 <- tm_map(corpus2, content_transformer(stripWhitespace)) 
corpus2.dtm<-DocumentTermMatrix(corpus2)
dtm.sparse2<-removeSparseTerms(corpus2.dtm, 0.995)
sparse.m2<-as.matrix(dtm.sparse2)
stop<-stopwords("en")
stop<-unlist(strsplit(stop,"[[:punct:]]"))
stop<-unique(stop)
sparse.m2<-sparse.m2[,!colnames(sparse.m2) %in% stop]

#run Mutual Information on keyword relative to all words in the corpus
keyword<-c("love")
pmi.df2<-NULL
for (i in 1:ncol(sparse.m2)){
  pmi<-mutinformation(sparse.m2[,which(colnames(sparse.m2) == keyword)], sparse.m2[,i], method="emp")
  temp.df<-data.frame(colnames(sparse.m2)[i], pmi)
  pmi.df2<-rbind(pmi.df2, temp.df)
}
pmi.final.source2<-pmi.df2[order(-pmi.df2[,2]),]
pmi.final.source2<-pmi.final.source2[-1,]

#compare vectors of MI scores
colnames(pmi.final.source2)<-c("word", "mi")
colnames(pmi.final.source)<-c("word", "mi")

mi.all<-merge(pmi.final.source, pmi.final.source2, by="word", all=T)
mi.all[is.na(mi.all)]<-0
mi.all$x_minusy<-mi.all$mi.x-mi.all$mi.y
mi.all$y_minusx<-mi.all$mi.y-mi.all$mi.x


