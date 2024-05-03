###### Making a Document Term Matrix ########
######### by Andrew Piper ####################
######### CC By 4.0 License ##################

#assumes you have a directory of .txtfiles

library("tm")
library("slam")
#library("textstem")

setwd("~/Data")
#### Efficient version no explanation (see below) #####

#for texts from a directory
corpus1 <- VCorpus(DirSource("PWBS", encoding = "UTF-8"), readerControl=list(language="English"))

#for texts from a table
#corpus1 <- VCorpus(VectorSource(my_table$text_column), readerControl = list(language = "English"))
#add metadata
#meta(my_corpus2, tag = "From") <- c("String1", "String2", "String3")

corpus1 <- tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
f<-content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus1 <- tm_map(corpus1, f, "[[:punct:]]")
corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace)) 
corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(3,Inf)))
dtm.scaled<-corpus1.dtm/row_sums(corpus1.dtm)
dtm.tfidf<-weightTfIdf(corpus1.dtm, normalize = TRUE)

#DTM W ONLY STOPWORDS
stop<-stopwords("en")
stop<-unlist(strsplit(stop,"[[:punct:]]"))
stop<-unique(stop)
#add extra stopwords (specific to novels)
stop<-append(stop, c("said", "one", "will"))
stop<-append(stop, tolower(as.roman(1:1000)))
dtm.stop<-as.matrix(dtm.scaled[ ,which(colnames(dtm.scaled) %in% stop)])

#DTM W NO STOPWORDS + NON-SPARSE WORDS
dtm.nostop<-dtm.scaled[ ,which(!colnames(dtm.scaled) %in% stop)]
dtm.sparse<-removeSparseTerms(dtm.nostop, 0.3)

#DTM W TFIFDF VERSION NO STOP, NONSPARSE
dtm.tfidf<-weightTfIdf(dtm.sparse, normalize = TRUE)


######   Now with explanations for toggling on / off   ########## 

#Read in corpus
#the name in "" is the name of your folder where your texts are
corpus1 <- VCorpus(DirSource("txtlab_Novel150_English", encoding = "UTF-8"), readerControl=list(language="English"))
#make all lowercase
corpus1 <- tm_map(corpus1, content_transformer(tolower))
#remove numbers
corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
#remove punctuation
f<-content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus1 <- tm_map(corpus1, f, "[[:punct:]]")
#strip white space
corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace)) 

#Option: lemmatize your data (not often recommended)
#corpus1.lemma <- tm_map(corpus1, lemmatize_strings)
#corpus1.lemma <- tm_map(corpus1, PlainTextDocument)

######## Make a document term matrix ###########

#run the function on your corpus variable
corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf))) #(1,Inf) refers to the range of word lengths kept

#### Bigrams ### 

BigramTokenizer2 <- function(x)unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
BigramTokenizer12 <- function(x)unlist(lapply(ngrams(words(x), 1:2), paste, collapse = " "), use.names = FALSE)
dtm.bigram <- DocumentTermMatrix(corpus1, control=list(tokenize = BigramTokenizer2, wordLengths=c(1,Inf)))

### Normalization 1: Scaling ####
#divide the counts by the total number of words in each document.
dtm.scaled<-corpus1.dtm/row_sums(corpus1.dtm)

### Normalization 2: Tf-Idf ####
dtm.tfidf<-weightTfIdf(corpus1.dtm, normalize = TRUE)

### Feature Selection #####

### Stop Words ####
stopwords("en") #notice how the punctuation is still there
stop<-stopwords("en")
stop<-unlist(strsplit(stop,"[[:punct:]]"))
stop<-unique(stop)
#if you want to add additional words
#stop<-append(stop, c("INSERTWORD", "INSERTWORD", "ETC"))

#KEEP ONLY STOPWORDS
dtm.stop<-as.matrix(dtm.scaled[ ,which(colnames(dtm.scaled) %in% stop)])

#REMOVE STOPWORDS
dtm.nostop<-dtm.scaled[ ,which(!colnames(dtm.scaled) %in% stop)]

#Extended Stopwords
#remove words less than 3 letters
dtm.nostop<-dtm.nostop[, which(!nchar(colnames(dtm.nostop)) < 3)]
#extra stopwords
stop.xtra<-c("said", "one", "will")
#append a list of roman numerals
stop.xtra<-append(stop.xtra, tolower(as.roman(1:1000)))
#remove these words
dtm.nostop<-dtm.nostop[, which(!colnames(dtm.nostop) %in% stop.xtra)]

#### Top N Words ####
top.words<-sort(col_means(dtm.nostop), decreasing = T)[1:10000]
dtm.topN<-dtm.scaled[,which(colnames(dtm.scaled) %in% names(top.words))]

### Sparse Words ####
dtm.sparse<-removeSparseTerms(dtm.nostop, 0.4)


