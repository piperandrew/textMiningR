######### Machine Learning   ####################
######### by Andrew Piper ####################
######### CC By 4.0 License ##################

library("kernlab")
library("caret")
library("tm")
library("slam")

#set working directory
setwd("~/Data")

######## Create DTM ##########
corpus1 <- VCorpus(DirSource("txtlab_Novel150_English", encoding = "UTF-8"), readerControl=list(language="English"))
corpus1 <- tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
f<-content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus1 <- tm_map(corpus1, f, "[[:punct:]]")
corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace)) 
corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
dtm.scaled<-corpus1.dtm/row_sums(corpus1.dtm)
dtm.tfidf<-weightTfIdf(corpus1.dtm, normalize = TRUE)

#DTM W ONLY STOPWORDS
stop<-stopwords("en")
stop<-unlist(strsplit(stop,"[[:punct:]]"))
stop<-unique(stop)
dtm.stop<-as.matrix(dtm.scaled[ ,which(colnames(dtm.scaled) %in% stop)])

#DTM W NO STOPWORDS + NON-SPARSE WORDS
#add extra stopwords (specific to novels)
stop.plus<-append(stop, c("said", "one", "will"))
stop.plus<-append(stop.plus, tolower(as.roman(1:1000)))
dtm.nostop<-dtm.scaled[ ,which(!colnames(dtm.scaled) %in% stop.plus)]
dtm.sparse<-removeSparseTerms(dtm.nostop, 0.4)

#DTM W TFIFDF VERSION NO STOP, NONSPARSE
dtm.tfidf<-weightTfIdf(dtm.sparse, normalize = TRUE)


###### Label your documents #######
#This involves creating a column called "corpus" which contains the "class" to which each document belongs

#first turn DTM into a data frame
dtm<-as.data.frame(as.matrix(dtm.sparse))

### Method 1 = Extract class from metadata
#use this for txtlab_Novel150_English dataset
#load metadata
meta<-read.csv("txtlab_Novel150_English.csv")
#order metadata and DTM by filenames
meta<-meta[order(meta$filename),]
dtm<-dtm[order(row.names(dtm)),]
which(row.names(dtm) != meta$filename) #if this line does not return "integer(0)" you've got a problem!

#append gender column to DTM
dtm$corpus<-factor(meta$gender)

#### Method 2 = Extract class from filenames
#extract corpus data from filename
#split on underscores
#List1<-strsplit(row.names(dtm), "_")
#extract just the first element (text before the first underscore) and turn it into a column
#dtm$corpus<-as.factor(sapply(List1,"[[",1))

#### Create Folds ######
#folds are your data divided into equal sized units that are balanced by your classes
# k = number of folds (1-10) where 10 is best
# it depends on how much data you have. you generally want at least 20 docs per fold
folds<-createFolds(dtm$corpus, k=5) 

#### Establish positive class #####
#this is the class you want to predict
category<-c("female")

##### Run SVM #######

######## x-fold cross validation ##########
#this runs the process for all folds
cv.results<-lapply(folds, function(x){
  df.train<-dtm[-x,]
  df.test<-dtm[x,]
  df.model<-ksvm(corpus ~ ., data=df.train, kernel="rbfdot")
  df.pred<-predict(df.model, df.test)
  con.matrix<-confusionMatrix(df.pred, df.test$corpus, positive = category)
  f1<-con.matrix$byClass[[7]] 
})
unlist(cv.results) #this shows you the F1 score for each fold
mean(unlist(cv.results)) #this is the average F1 score for all folds
sd(unlist(cv.results))

#to observe a single fold
#x<-folds$Fold01 #use this line if k = 10 otherwise use next line
x<-folds$Fold1

#create training data
df.train<-dtm[-x,]
#create test data
df.test<-dtm[x,]
#build model
df.model<-ksvm(corpus ~ ., data=df.train, kernel="rbfdot", prob.model = T) ##rbfdot = Gaussian kernel; vanilladot = linear kernel
#predict on test data
df.pred<-predict(df.model, df.test)

#if you wish to preserve the probability of each document belonging to the class run this line
#df.pred<-predict(df.model, df.test, type="probabilities")

#make confusion matrix for one fold
#this does not work with type=probabilities
con.matrix<-confusionMatrix(df.pred, df.test$corpus, positive = category)
#observe all accuracy scores
con.matrix$byClass
#subset by particular score
con.matrix$byClass[[5]] #Precision
con.matrix$byClass[[6]] #Recall
con.matrix$byClass[[7]] #F1 score
con.matrix$overall[[6]] #accuracy p-value

#observe which documents were mispredicted

#for binary classification
predictions<-data.frame(row.names(df.test), df.test$corpus, df.pred)
predictions$match<-predictions$df.test.corpus == predictions$df.pred
colnames(predictions)<-c("document", "actual", "predicted", "match")

#for probabilities
#run this if you ran the type = "probabilities" line above
predictions<-data.frame(row.names(df.test), df.test$corpus, df.pred)
predictions$predicted<-vector(length=nrow(predictions))
### NOTE: make sure to change the below values to match your class names
for (i in 1:nrow(predictions)){
  if(predictions[i,which(colnames(predictions) == category)] > .5){
    predictions$predicted[i]<-c("female") #this is your positive class (i.e. your "category" variable)
  } else {predictions$predicted[i]<-c("male")} #this is your other class
}
predictions$match<-predictions$df.test.corpus == predictions$predicted


####### Random Forests ########
library(randomForest)

#randomForests requires fixing column names
names(dtm) <- make.names(names(dtm))

#same procedure as above
x<-folds$Fold1
df.train<-dtm[-x,]
df.test<-dtm[x,]
df.model<-randomForest(corpus ~ ., data=df.train) #this is the only line that changes
df.pred<-predict(df.model, df.test)
con.matrix<-confusionMatrix(df.pred, df.test$corpus, positive = category)
con.matrix$byClass[[7]] #F1 score

#one advantage to RFs is we can extract the feature weights that contribute to the model's success
weights<-data.frame(importance(df.model))
weights$feature<-factor(row.names(weights))
weights<-weights[order(-weights$MeanDecreaseGini),]

#plot subset
#subset by top 50 features
weights.sub<-weights[1:50,]
#plot
library("ggplot2")
ggplot(weights.sub, aes(x=reorder(feature, MeanDecreaseGini), y=MeanDecreaseGini)) +
  geom_bar(stat='identity', fill="blue") +
  coord_flip() +
  theme_classic() + 
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"))+
  xlab("Feature") +
  ylab("Weight") 

######## cross validation ##########
#this runs the process for all folds
cv.results<-lapply(folds, function(x){
  df.train<-dtm[-x,]
  df.test<-dtm[x,]
  df.model<-randomForest(corpus ~ ., data=df.train) #this is the only line that changes
  df.pred<-predict(df.model, df.test)
  con.matrix<-confusionMatrix(df.pred, df.test$corpus, positive = category)
  f1<-con.matrix$byClass[[7]] 
})
unlist(cv.results) #this shows you the F1 score for each fold
mean(unlist(cv.results)) #this is the average F1 score for all folds
sd(unlist(cv.results))



