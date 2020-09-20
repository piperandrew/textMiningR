###### Preparing Text

#load libraries (you need to do this every time)
library("tm")
#library("textstem")
library("slam")

#Set working directory
setwd("~/Data")

#Read in corpus
#the name in "" is the name of your folder where your texts are
corpus1 <- VCorpus(DirSource("txtlab_Novel150_English", encoding = "UTF-8"), readerControl=list(language="English"))
#make all lowercase
corpus1 <- tm_map(corpus1, content_transformer(tolower))
#remove numbers
corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
#remove punctuation
#corpus1 <- tm_map(corpus1, content_transformer(removePunctuation))
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


