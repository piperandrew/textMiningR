######### Corpus Comparison - Distinctive Features ##########
######### by Andrew Piper ####################
######### CC By 4.0 License ##################

library("tm")
library("slam")
setwd("~/Data")

### Datasets used here:
#Sherlock Holmes Collection. https://doi.org/10.6084/m9.figshare.17425568.v1 
#Short Stories Collection. https://doi.org/10.6084/m9.figshare.17425571.v1 

### This script uses two different methods to measure "distinctiveness"
### The first measure uses the log-likelihood ratio (Dunning's or G-test)
### The second measure uses the rank sum test

#### Corpus A ####
corpus1 <- VCorpus(DirSource("SherlockHolmes", encoding = "UTF-8"), readerControl=list(language="English"))
corpus1 <- tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
f<-content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus1 <- tm_map(corpus1, f, "[[:punct:]]")
corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace)) 
corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf))) #(1,Inf) refers to the range of word lengths kept
dtm1.scaled<-corpus1.dtm/row_sums(corpus1.dtm)

#### Corpus B ####
corpus2 <- VCorpus(DirSource("ShortStories_English", encoding = "UTF-8"), readerControl=list(language="English"))
corpus2 <- tm_map(corpus2, content_transformer(tolower))
corpus2 <- tm_map(corpus2, content_transformer(removeNumbers))
f<-content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus2 <- tm_map(corpus2, f, "[[:punct:]]")
corpus2 <- tm_map(corpus2, content_transformer(stripWhitespace)) 
corpus2.dtm<-DocumentTermMatrix(corpus2, control=list(wordLengths=c(1,Inf))) #(1,Inf) refers to the range of word lengths kept
dtm2.scaled<-corpus2.dtm/row_sums(corpus2.dtm)

############   Log-likelihood (G-test)  ##############
#The way we will model this is to ask how much more likely is WORD X to appear in CORPUS A than B
#The way calculate this is by making what are known as "contingency tables".
#This allows us to compare the rate of a given word relative to the overall number of words in that corpus
#They have the following structure:

#         Corpus A    Corpus B
#Word       x           y
#NotWord    z           w

#NOTE: we use raw counts not scaled counts for this measure

### Subset Corpora by intersecting words
#To run this we first need to subset our two corpora by words they have in common

#First reduce our corpora to non-sparse words
#adjust integer based on the size of your data and length of documents
#because we are using short stories we require words to be in at least 20% of documents
corpus1.sparse<-removeSparseTerms(corpus1.dtm, 0.8) #adjust integer here accordingly
corpus2.sparse<-removeSparseTerms(corpus2.dtm, 0.8) #adjust integer here accordingly

#keep only words in both sets
keep<-intersect(colnames(corpus1.sparse), colnames(corpus2.sparse))

#subset 1 and 2 by these words
#this is your DTM1 and DTM2
dtm1<-corpus1.dtm[,colnames(corpus1.dtm) %in% keep]
dtm2<-corpus2.dtm[,colnames(corpus2.dtm) %in% keep]

#first get individual word counts for each corpus
word1<-col_sums(dtm1)
word2<-col_sums(dtm2)
#then get total counts for each corpus
all1<-sum(word1)
all2<-sum(word2)

#entropy function
H = function(k) {N = sum(k); return(sum(k/N*log(k/N+(k==0))))}

#store empty results in a table
results <- data.frame(word = colnames(dtm1), 
                      group1=word1,
                      group2=word2,
                      G2 = 0,
                      fisher.OR = 0,
                      fisher.p = 0)
#create loop to go through every word
for (j in 1:ncol(dtm1)){
  print(j)
  #create contingency table for each word
  cont.table<-data.frame(c(word1[j], all1-word1[j]), c(word2[j], all2-word2[j]))
  #get straight odds ratio
  fish<-fisher.test(cont.table)
  #Dunning's
  LLR = 2*sum(cont.table)*(H(cont.table)-H(rowSums(cont.table))-H(colSums(cont.table)))
  results$G2[j] = LLR
  results$fisher.OR[j] = fish$estimate
  results$fisher.p[j] = fish$p.value
}

#keep only words that pass a significance threshold
#because we just ran as many tests as there are words we need to do a Bonferroni Correction
#which divides our cut-off of significance by the total number of tests we ran
#establish correction
#the first value is your threshold and the denominator is the number of words tested
cut<-0.05/ncol(dtm1)

#keep only words whose p-value is below the cut
results<-results[results$fisher.p < cut,]

#create a new column that turns the G2 score negative if it applies to group2
#this gives you a way of sorting for G2 by group or just strongest words in general
#negative values indicate distinctive words for group2, positive for group 1
results$G2_Sorted<-vector(mode="numeric", length=nrow(results))
for (i in 1:nrow(results)){
  if (results$fisher.OR[i] < 1){
    results$G2_Sorted[i]<--results$G2[i]
  } else {
    results$G2_Sorted[i]<-results$G2[i]
  }
}
results<-results[order(-results$G2_Sorted),]


############    Wilcoxon Rank Sum Test Method     #############
#This method avoids cases where you have small corpora and highly frequent words in a few documents
#will appear distinctive of the whole corpus
#The rank sum test observes the distribution of each word across the whole corpus
#This script outputs the median values for each corpus, their ratio, the Wilcoxon statistic and a p-value

## Make sure to used the ***scaled*** versions ##
corpus1.sparse<-removeSparseTerms(dtm1.scaled, 0.4)
corpus2.sparse<-removeSparseTerms(dtm2.scaled, 0.4)

keep<-intersect(colnames(corpus1.sparse), colnames(corpus2.sparse))

#subset 1 and 2 by these words
dtm1<-corpus1.sparse[,colnames(corpus1.sparse) %in% keep]
dtm2<-corpus2.sparse[,colnames(corpus2.sparse) %in% keep]

#check to see if the column names are identical
which(colnames(dtm1) != colnames(dtm2))

#turn into a matrix
dtm1<-as.matrix(dtm1)
dtm2<-as.matrix(dtm2)

### Rank Sum Test ###

#run a test for each feature (i.e. word)
rank.df<-NULL
for (j in 1:ncol(dtm1)){
  print(j)
  rnk<-wilcox.test(dtm1[,j], dtm2[,j])
  median.a<-median(dtm1[,j])
  median.b<-median(dtm2[,j])
  p<-rnk$p.value
  word<-colnames(dtm1)[j]
  ratio<-median.a/median.b
  temp.df<-data.frame(word, median.a, median.b, p, ratio)
  rank.df<-rbind(rank.df, temp.df)
}
#sort your table by the ratio of the medians for each word
wilcox.df<-rank.df[order(-rank.df$ratio),]

#we use bonferoni correction and divide 0.05 by the total number of words tested
cut<-0.05/ncol(dtm1)

#remove all words above this threshold (i.e. not significant)
wilcox.df<-wilcox.df[wilcox.df$p < cut,]

#save the table to your hard drive
#write.csv(final.sort, file="MDW_Sherlock_RankSum.csv", row.names=F)

