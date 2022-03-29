######### Add predicted gender to bookNLP data   ####################
######### by Andrew Piper                        ####################
######### CC By 4.0 License                      ##################

#this applies the gender package to bookNLP data
#it takes as input a .tokens table and outputs a new column with the predicted gender
#you can add columns for the probability of the predicted gender if you wish
#runtime = extremely slow!

library(gender)
library(remotes)
remotes::install_github("lmullen/genderdata")

setwd("")

#ingest bookNLP table
df<-read.csv("cnn_booknlp_outputs.csv")

#create empty column for predicted genders
df$gender<-vector(mode="numeric", length=nrow(cnn))

#establish year or year range for data
year<-2012

#for every token
for (i in 1:nrow(df)){
  #check to see if it is a person
  if (df$ner[i] == "PERSON"){
  #then check to see if the prior token is a person -- if so then skip [because it is a sequential name]
    if (df$ner[i-1] != "PERSON"){
      #measure progress
      print(i)
      #predict gender using social security records
      g<-gender(df$originalWord[i], years=year, method="ssa")[[4]]
      #check to see if there was a match
      if (length(g)>0){
        df$gender[i]<-g
      }
    }
  }
}

