###############    Measuring Dialogue using bookNLP  ####################
######### by Andrew Piper ####################
######### CC By 4.0 License ##################

#this script takes as input a directory of bookNLP files
#for each book it measures 
# the frequency of total tokens within dialogue relative to all tokens
# the avg. length of dialogue

library(stringi)
library(stringr)

#load metadata
setwd("/Users/akpiper/Data")
meta<-read.csv("CHICAGO_CORPUS_NOVELS.csv")

#set root working directory
wd.root<-c("/Users/akpiper/Data/CHICAGO_NLP/")

setwd(wd.root)

#get list of folders (i.e. books)
filenames<-list.files()

#subset by .quotes files
file.sub<-filenames[grep(".quotes", filenames)]

#subset by tokens files
file.tokens<-filenames[grep(".tokens", filenames)]

#normalize filenames to match metadata
file.norm<-gsub(".quotes",".txt", file.sub)

#create empty final table
final.df<-NULL

#for every book
for (i in 1:length(file.sub)){
  
  print(i)
  
  #load tokens file
  tokens.df<-read.csv(file.tokens[i], quote="", sep="\t")
  
  #load quotes file
  quotes.df<-read.csv(file.sub[i], quote="", sep="\t")
  
  #frequency of dialogue tokens
  freq.dialogue<-sum(quotes.df$quote_end-quotes.df$quote_start)/nrow(tokens.df)
  
  #avg length of dialogue
  mean.dialogue<-mean(quotes.df$quote_end-quotes.df$quote_start)
  
  #final table
  fileID<-file.norm[i]
  date<-meta$PUBL_DATE[meta$FILENAME == file.norm[i]]
  temp.df<-data.frame(fileID, date, freq.dialogue, mean.dialogue)
  final.df<-rbind(final.df, temp.df)
}

#get decade estimates
final.df$decade<-substr(final.df$date, start = 1, stop = 3)
final.df$decade<-as.numeric(paste(final.df$decade, "0", sep=""))

#summarize results
decade.df<-NULL
for (i in 1:nlevels(factor(final.df$decade))){
  sub<-final.df[final.df$decade == levels(factor(final.df$decade))[i],]
  results.freq.dialogue<-calculate_metrics_by_group(sub$freq.dialogue, sub$decade)
  results.mean.dialogue<-calculate_metrics_by_group(sub$mean.dialogue, sub$decade)
  #decade.mean.dialogue<-mean(sub$mean.dialogue, na.rm=T)
  #decade.freq.dialogue<-mean(sub$freq.dialogue, na.rm=T)
  date<-as.numeric(levels(factor(final.df$decade))[i])
  Variable1<-c("DialogueLength")
  Variable2<-c("FreqDialogueTokens")
  temp.df1<-data.frame(date, results.freq.dialogue, variable=Variable2)
  temp.df2<-data.frame(date, results.mean.dialogue, variable=Variable1)
  decade.df<-rbind(decade.df, temp.df1, temp.df2)
}

#plot
sub.df<-decade.df[decade.df$variable == "FreqDialogueTokens",]
p1<-ggplot(sub.df, aes(x=date, y=Mean*100)) +
  geom_point() +
  #geom_point(shape = 15, size = 3) +
  #geom_line(linetype = "dotted") +
  geom_errorbar(aes(ymin=Mean*100 - SE*100, ymax=Mean*100 + SE*100, width=2)) +
  scale_x_continuous(breaks=seq(from=min(sub.df$date), to=max(sub.df$date), by=20)) + # Adjusting x-axis for every decade
  theme_classic() + 
  #xlab("Decade") +
  #xlab("Decade") +
  xlab("") +
  ylab("Percent of Tokens") +
  ggtitle("Amount of Dialogue") +
  labs(
    #caption="Source: Chicago",
    #subtitle="N=9,089"
  )

sub.df<-decade.df[decade.df$variable == "DialogueLength",]
p2<-ggplot(sub.df, aes(x=date, y=Mean)) +
  geom_point() +
  #geom_point(shape = 15, size = 3) +
  #geom_line(linetype = "dotted") +
  geom_errorbar(aes(ymin=Mean - SE, ymax=Mean + SE, width=2)) +
  theme_classic() + 
  scale_x_continuous(breaks=seq(from=min(sub.df$date), to=max(sub.df$date), by=20)) + # Adjusting x-axis for every decade
  xlab("Decade") +
  ylab("No. Words") +
  ggtitle("Length of Dialogue")+
  labs(
    caption="Source: Chicago (N=9,089)",
    #subtitle="N=9,089"  # Add your subtitle
  )

library(gridExtra)
grid.arrange(p1, p2, ncol=1)



 