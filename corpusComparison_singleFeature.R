######### Corpus Comparison - Single Feature ##########
######### by Andrew Piper ####################
######### CC By 4.0 License ##################

#This script compares the frequency of a single feature across two corpora 
#Can we say that Feature X occurs significantly more frequently in Corpus A compared to Corpus B?

library("tm")
library("slam")

setwd("~/Data")

### Sample Datasets used here:
#Sherlock Holmes Collection. https://doi.org/10.6084/m9.figshare.17425568.v1 
#Short Stories Collection. https://doi.org/10.6084/m9.figshare.17425571.v1 

##### Ingest corpora #####

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

###### Identify Feature of Interest

#create a list of words to extract
myWords<-c("the", "this", "that", "these", "those")

#or ingest a table of words
myWords<-read.csv("myListOfWords.csv", header=F) #no header!

#subset your DTM by the words in your feature
corpus1.feature<-dtm1.scaled[,colnames(dtm1.scaled) %in% myWords]
corpus2.feature<-dtm2.scaled[,colnames(dtm2.scaled) %in% myWords]
#sum the frequencies of all words in your feature
feature.sum1<-row_sums(corpus1.feature)
feature.sum2<-row_sums(corpus2.feature)

#Compare the mean / median value of the feature for each corpus
#In order to compare the means of two samples those samples need to be NORMALLY distributed

### Step 1: Visualize the distribution of your feature using a histogram
#Here we will add a curve of the normal distribution so you can see how well your histogram fits the curve

#Dummy example using artificial data
hist(rnorm(100, mean = 5, sd = 3), prob=T)
curve(dnorm(x, mean=5, sd=3), add=TRUE)

#Same thing using your data
hist(feature.sum1, prob=T)
curve(dnorm(x, mean=mean(feature.sum1), sd=sd(feature.sum1)), add=TRUE)
hist(feature.sum2, prob=T)
curve(dnorm(x, mean=mean(feature.sum2), sd=sd(feature.sum2)), add=TRUE)

#### Step 2: Use a statistical test for normality
#Here we use the "Shapiro Test" to see if we can assume our data is normally distributed
# when p < 0.05 it means your data is NOT normally distributed
shapiro.test(feature.sum1) 
shapiro.test(feature.sum2)

#### Step 3: IF YES, there is a normal distribution, then run a Welch's two sample t-test
#Here we can get the value for the Mean of Corpus A and Mean of Corpus B for our Feature
#We can also get a t-statistic to tell us how strong this difference is
#And we can get a p-value
t.test(feature.sum1, feature.sum2)

#when you report your results you report the means of both samples along with the standard deviation
mean(feature.sum1)
mean(feature.sum2)
sd(feature.sum1)
sd(feature.sum2)

#to observe how similar your distributions are, plot the two distributions on the same graph
#this uses a density plot
#the dark line = Corpus A
#the dotted line = Corpus B
plot(density(feature.sum1), main = "Feature Distributions for\nCorpus A (black line) and Corpus B (dotted line)")
lines(density(feature.sum2), lty=2)
#abline(v=mean(feature.sum1))
#abline(v=mean(feature.sum2), lty=2)

#you can also use a boxplot
#the dark line = median
boxplot(feature.sum1, feature.sum2)

#finally you can estimate what is known as the "effect size", a way of estimating just how strong this difference is
#this is like asking what percent of Group A observations are below/above the mean of Group B
#rule of thumb .2 or less = small, .5 or less = medium, .8 and above = large
#see https://www.simplypsychology.org/effect-size.html
library(effsize)
cohen.d(feature.sum1, feature.sum2)

#### Step 4: IF NO, your data is NOT normally distributed, then run a Wilcoxon Rank Sum Test
wilcox.test(feature.sum1, feature.sum2)
#report the medians rather than the means
median(feature.sum1)
median(feature.sum2)
#report how much higher/lower median1 is to median2
median(feature.sum1)/median(feature.sum2)
#2.5 = "The median value of my feature is 2.5 times higher in group 1 than it is in group 2..."





