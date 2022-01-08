######### Emotion Analysis ##########
######### by Andrew Piper ####################
######### CC By 4.0 License ##################

#in this script you can observe the distribution of different emotion vocabulary across corpora
#it reuses the "Corpus Comparison" scripts

### Datasets used here:
#Sherlock Holmes Collection. https://doi.org/10.6084/m9.figshare.17425568.v1 
#Short Stories Collection. https://doi.org/10.6084/m9.figshare.17425571.v1 

library("tm")
library("slam")

setwd("~/Data")

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

#ingest emotion dictionary
#it contains 3 columns, one with words, the second with the emotions (plus positive/negative), and the third with 0/1 for yes/no
emolex<-read.csv("NRC-Emotion-Lexicon-Wordlevel-v0.92.txt", sep="\t", header=F)

#subset our DTM by a particular emotion:
#to see your options:
levels(factor(emolex$V2))

#choose an emotion to subset your DTM by
dic<-emolex[emolex$V2 == "negative",]
#to select two or more
dic<-emolex[emolex$V2 == "joy" | emolex$V2 == "trust",]

#keep only words with a positive value for that emotion type
dic<-dic[dic$V3 == 1,]

#subset your DTMs by those words
dtm1.dic<-dtm1.scaled[, which(colnames(dtm1.scaled) %in% as.character(dic$V1))]
dtm2.dic<-dtm2.scaled[, which(colnames(dtm2.scaled) %in% as.character(dic$V1))]

#run significance test
library(effsize)
feature.sum1<-row_sums(dtm1.dic)
feature.sum2<-row_sums(dtm2.dic)
hist(feature.sum1, prob=T)
curve(dnorm(x, mean=mean(feature.sum1), sd=sd(feature.sum1)), add=TRUE)
hist(feature.sum2, prob=T)
curve(dnorm(x, mean=mean(feature.sum2), sd=sd(feature.sum2)), add=TRUE)
# !!!! when p < 0.05 it means your data is NOT normally distributed
shapiro.test(feature.sum1) 
shapiro.test(feature.sum2)
t.test(feature.sum1, feature.sum2)
mean(feature.sum1)
mean(feature.sum2)
sd(feature.sum1)
sd(feature.sum2)
plot(density(feature.sum1), main = "Feature Distributions for\nCorpus A (black line) and Corpus B (dotted line)")
lines(density(feature.sum2), lty=2)
cohen.d(feature.sum1, feature.sum2)
wilcox.test(feature.sum1, feature.sum2)
median(feature.sum1)
median(feature.sum2)
median(feature.sum1)/median(feature.sum2)



