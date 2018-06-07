### Measure Topic Stability for Every Topic ###
#this script takes as input word-topic probabilities from a topic model
#output using the library "topicmodels"
#it assumes that you have run numerous models and have saved
#the outputs for the word-topic probability tables from the model into a single directory

#here is the code to do this, where topicmodel is the name of the model
#outputted by the function LDA():
#probabilities<-posterior(topicmodel)
#topic_word_probs<-as.data.frame(probabilities$terms)
#topic_word_probs is thus the output you need from every model run
#save these in a directory to be used here

#the script then assumes a primary model (the one you want to work with)
#it then calculates the average divergence between
#every topic in that model and the most similar topic from every other model
#the goal is to identify which topics are more "stable", i.e. have a lower
#overall KL-divergence (information loss) across different models
#the assumption is that lower avg. KLD == more semantic stability to the topic
library(topicmodels)
library(entropy)

#load all models
setwd("TK")
filenames<-list.files("WordProbs")
setwd("TK/WordProbs")

#load primary model
twp<-read.csv(filenames[1])
#clean
twp<-twp[,-1]
twp<-t(twp)
colnames(twp)<-seq(1,ncol(twp))
#initialize final table
stable.df<-NULL
#this is the table you can use to assess your topic stabilities for your
#primary model

#run
for (i in 1:nrow(topic_word_probs)){
  print(i)
  #subset by ith topic
  sub1<-twp[,i]
  #go through each model and find most similar topic
  test.t<-NULL
  for (j in 2:length(filenames)){
    #load next model
    comp<-read.csv(filenames[j])
    #clean
    comp<-comp[,-1]
    comp<-t(comp)
    colnames(comp)<-seq(1,ncol(comp))
    #go through every topic to find most similar
    #calculate KLD for every topic pair with the ith topic from primary model
    kld.v<-vector()
    for (k in 1:ncol(comp)){
      kld.v[k]<-KL.plugin(sub1, comp[,k])
    }
    #find minimum value, i.e. most similar topic
    top.t<-which(kld.v == min(kld.v))
    #create data frame
    model<-j
    kld.score<-kld.v[which(kld.v == min(kld.v))]
    temp.df<-data.frame(model, top.t, kld.score)
    test.t<-rbind(test.t, temp.df)
  }
  #calculate mean and sd for the ith topic compared to best topic of all other models
  mean.kld<-mean(test.t$kld.score)
  sd.kld<-sd(test.t$kld.score)
  topic<-i
  temp.df<-data.frame(topic, mean.kld, sd.kld)
  stable.df<-rbind(stable.df, temp.df)
}


