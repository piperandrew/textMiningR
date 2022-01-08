######### Emotional Arcs   ##########
######### by Andrew Piper   ####################
######### CC By 4.0 License   ##################

#this script generates a plot of a book's "emotional arc". Emotional arcs help visualize
#the fortunes surrounding a book's events. How much vicissitude is there? Where are the high / low points?
#do these types of moments happen at predictable places or follow familiar shapes?

#you can also use this script to validate the arcs
#it allows you to identify the emotional high and low points so you can read those
#passages to assess how well the algorithm is working on your documents

#we use the emotion lexicon of the Canadian National Research Council
#to measure two kinds of emotions: positive/negative (called "valence")
#and "intensity" (called "arousal"). Intensity measures the activity/calmness of vocabulary

#this tool takes as input a txt file of the MS
#to measure "emotion" it uses the NRC emotion lexicon located here:
#https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm
#it creates a windowed version of the text and generates a score for each window
#it then plots those scores for users to inspect their arcs

setwd("~/Research/Granthika/Granthika Metrics 01")

#ingest lexicon
#we will condtion on the valence and arousal columns
lex<-read.csv("NRC-VAD-Lexicon.txt", sep="\t", stringsAsFactors = F)

#ingest work
work<-scan("00010241_1925_FScottFitzgerald_TheGreatGatsby.txt", what="character", quote="", quiet=T)

#clean the text
#remove numbers
work<-gsub("\\d", "", work)
#make all lowercase
work<-tolower(work)
#remove punctuation
#work<-gsub("\\W", "", work)
work<-unlist(strsplit(work, "[[:punct:]]"))
#remove blanks
work<-work[work != ""]

#window size, default = 250 words
n=250

#calculate number of chunks -- divide length of work by window size and round down
#must subtract the window size itself from the length of the work because you can't take a window beyond the last word :)
chunks<-floor((length(work)-n)/n)

#create empty table to store scores
emotion.df<-NULL

#create loop to slide through MS by "n"-sized chunks
for (i in 0:(chunks-1)){
  
  #subset the work by window size starting at the i-th chunk and moving forward by n 
  sub<-work[((i*n)+1):((i*n)+n)]
  
  #subset the window vector by matching words from the emotion table
  sub<-sub[sub %in% lex$Word]
  
  #for every word in the window subset the lexicon table and keep the valence score for that word
  #do this for every word in the window and return a vector of scores
  #the output is a vector of emotion scores for every word in the window
  
  #valence function
  valence.vector<-unlist(sapply(sub, function (x){
    lex$Valence[lex$Word == x]
  }))
  #arousal function
  arousal.vector<-unlist(sapply(sub, function (x){
    lex$Arousal[lex$Word == x]
  }))
  
  #final emotion score for a given window is the avg of all words' emotion scores for that window
  valence.score<-mean(valence.vector)
  arousal.score<-mean(arousal.vector)
  
  #get word count for plotting purposes
  #the score refers to the word count of the *last* word in the passage
  #thus 750 = a passage that encompasses words 501-750 of the document
  word<-(i*n)+n
  
  #store each score separately in table with a column for the emotion type
  temp.valence<-data.frame(word, valence.score, c("valence"))
  temp.arousal<-data.frame(word, arousal.score, c("arousal"))
  
  #adjust column names for binding together
  colnames(temp.valence)<-c("word", "score", "type")
  colnames(temp.arousal)<-c("word", "score", "type")
  
  #attach tables
  temp.both<-rbind(temp.valence, temp.arousal)
  emotion.df<-rbind(emotion.df, temp.both)
}

#### Create normalized values
#this is valuable to put both emotions on the same plot and to organize the 
#interpretation of the graph around the underlying distribution of scores

#to do this we are going to separate the two emotion types into separate tables and then recombine them
valence.df<-emotion.df[emotion.df$type == "valence",]
arousal.df<-emotion.df[emotion.df$type == "arousal",]

#scale
valence.df$norm<-scale(valence.df$score)
arousal.df$norm<-scale(arousal.df$score)

#recombine
emotion.df<-rbind(valence.df, arousal.df)

#############      PLOT      ###################
library(ggplot2)

######## Plot 1 = Smoothed Arc ########

#choose the emotion dimension to plot
#1. = "fortune" = positive/negative. this is aligned with a sense of "fortune" in the plot
#2. = "arousal" = active/passive. this is aligned with a sense of action or urgency
#3. = both

#set which emotion type

#emotion.dimension<-c("valence")
#emotion.dimension<-c("arousal")
emotion.dimension<-c("valence", "arousal")

#subset the table by the emotion dimension
emotion.sub<-emotion.df[which(emotion.df$type %in% emotion.dimension),]

# set the granularity of smoothing -- how general do you want the arc to be?
#recommended values -- you can play with these values to observe effects of arcs
#very general = .3
#medium = .2
#specific = .1
specificity<-.3

#plot
ggplot(emotion.sub, aes(x=word, y=norm, color = type)) +
  theme_bw() +
  geom_smooth(span = specificity, se=F) +
  geom_hline(yintercept= 0) +
  labs(x="Word Count", y="Emotion Score", title="The emotional arc of your book")

######### Plot 2 - NO SMOOTHING #######
#This allows you to observe all sections

#make sure to run one emotion at a time
emotion.dimension<-c("valence")
#emotion.dimension<-c("arousal")

#subset the table by the emotion dimension
emotion.sub<-emotion.df[which(emotion.df$type %in% emotion.dimension),]

#set specificity of smoothing
specificity<-.3

ggplot(emotion.sub, aes(x=word, y=norm, fill = type)) +
  theme_bw() +
  #geom_line() +
  geom_bar(stat="identity") +
  geom_smooth(span = specificity, se=F) +
  geom_hline(yintercept= 0) +
  labs(x="Word Count", y="Emotion Score", title="The emotional arc of your book")


###########     ASSESS RESULTS    ##########
#in this section you can to identify high and low points in the document, pinpointing the 
#passage(s) and outputting them onscreen so you can read them more closely

#first choose the emotion type to explore
type<-c("valence")
#type<-c("arousal")

#find top 10 high  / low points
top.df<-emotion.df[emotion.df$type == type,]
top.df<-top.df[order(-top.df$norm),]
top.df[1:10,]

bottom.df<-emotion.df[emotion.df$type == type,]
bottom.df<-bottom.df[order(bottom.df$norm),]
bottom.df[1:10,]

#output text for those segments
#first define which segment, i.e. which one of the top 10, in descending order (1:10)
#s=1 looks at the most positive / negative passage
s=1

#extract passage - positive
test<-work[(top.df$word[s]-250):top.df$word[s]]

#extract passage - negative
test<-work[(bottom.df$word[s]-250):bottom.df$word[s]]

#type "test" to see the passage onscreen
test

#get the sentiment score
valence.vector<-unlist(sapply(test, function (x){
  lex$Valence[lex$Word == x]
}))
#type "valence.vector" to see the individual words and their scores
mean(valence.vector)
