###########################################
#######   Collocate Builder   #############
######### by Andrew Piper     #############
######### CC By 4.0 License   #############

################################################################################
########################    Distinctive Collocates    ##########################
################################################################################

#A 'collocate' is a word that appears near another word ("co-located").
#Using the theory of distributional semantics that a word's meaning is
#determined by its usage, we can measure words that are distinctive of 
#a given keyword in a given corpus. If we want to know how
#a word is used *differently* in different types of texts (or different communities of writers)
#we can use this method to better understand the differences in the semantic fields
#surrounding a given keyword. 
#Alternately, we can use this method to understand the different semantic fields surrounding
#two related keywords in the same collection of texts, such as "mother" and "father".

#To do this we need two steps:
#A. Build the list of collocates for a given keyword in a given corpus
#B. Use our distinctive words test to identify words that are semantically distinctive of each keyword

##############################     A. Collocate Builder    #################################

#this script takes as input:
#- a directory of .txt files
#- a "keyword"
#- a window size of +/- N words (default = 9)

#it outputs a table of word counts associated with the keyword

#because you can either compare:
#- the same keyword in two different collections (e.g. collocates of "mother" in children's lit v. adult lit)
#- or two keywords in the same collection (e.g. collocates of mother v. father in children's lit)
#you will run the following script TWICE by changing either the KEYWORD or the txt files DIRECTORY
#notice at the end where you save your outputted list of words -- you need to create 2 variables, one for each run

#text cleaning function: run this
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

#set working directory to txt files location -- set to the directory where they are actually located
#change this for second run if testing 2 collections
setwd("~/Data/gut_ChildrenStories")
#setwd("~/Data/gut_AdultStories")

#get list of files in target directory
f.names<-list.files()

#define keyword
#change this on second run if comparing two different keywords
keyword<-c("mother")
#keyword<-c("father")

#define window +/- (Default = 9)
n<-9

#create an empty vector where you will save your collocates
collocate.v<-NULL
#create a loop that is as long as the number of documents
for (i in 1:length(f.names)){
  #see how fast things are going
  print(i)
  #ingest and clean each text
  work.v<-text.prep(f.names[i])
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

#*****Create table of counts -- remember to adjust which line you run depending on first or second run!!!

#For your FIRST keyword/text collection
collocate.df1<-data.frame(sort(table(collocate.v), decreasing = T))

#For your SECOND keyword/text collection
#collocate.df2<-data.frame(sort(table(collocate.v), decreasing = T))

#####################      B. Identify Distinctive Collocates      ######################

#this script runs the same distinctive words test using the G2 test from earlier
#The way we will model this is to ask:
#how much more likely is COLLOCATE X to appear as a collocate in CORPUS A than B? Or near KEYWORD A than B?
#The way calculate this is by making what are known as "contingency tables".
#This allows us to compare the rate of a given word relative to the overall number of words in that corpus
#They have the following structure:

#           Corpus A/Keyword A    Corpus B/Keyword B
#Collocate       x                  y
#NotCollocate    z                  w

#we match the two data frames of word counts and we only keep words that are in both

#first turn into strings not factors
collocate.df1$collocate.v<-as.character(collocate.df1$collocate.v)
collocate.df2$collocate.v<-as.character(collocate.df2$collocate.v)

#then sort alphabetically
collocate.df1<-collocate.df1[order(collocate.df1$collocate.v),]
collocate.df2<-collocate.df2[order(collocate.df2$collocate.v),]

#merge
collocate.all<-merge(collocate.df1, collocate.df2, by="collocate.v", all = T)
collocate.all[is.na(collocate.all)]<-0

#remove rows where at least one column is not > X (default = 50)
collocate.all<-collocate.all[collocate.all$Freq.x > 49 | collocate.all$Freq.y > 49,]

#first get individual word counts for each corpus
word1<-collocate.all$Freq.x
word2<-collocate.all$Freq.y

#then get total counts for each corpus
all1<-sum(word1)
all2<-sum(word2)

#entropy function
H = function(k) {N = sum(k); return(sum(k/N*log(k/N+(k==0))))}

#store empty results in a table
results <- data.frame(word = collocate.all$collocate.v, 
                      group1=word1,
                      group2=word2,
                      G2 = 0,
                      fisher.OR = 0,
                      fisher.p = 0)
#create loop to go through every word
for (j in 1:nrow(results)){
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
cut<-0.05/nrow(results)

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

#save your results
write.csv(results, file="MyResults.csv", row.names = F)




