#################################################
###### Measuring number of characters ###########
#################################################
#Takes as input the .entities file produced by bookNLP
#https://github.com/booknlp/booknlp

#Outputs: 
#A table of character counts for the top N characters for every book

#get metadata
setwd("~/Data/")
meta<-read.csv("CONLIT_META.csv")

#set root working directory
wd.root<-c("~/Data/CONLIT_NLP/")
setwd(wd.root)

#get list of files
filenames<-list.files()

#subset by .tokens files
file.sub<-filenames[grep(".entities", filenames)]

#normalize filenames
file.sub<-gsub(".entities",".txt", file.sub)

#OPTION: subset by FIC
#file.sub<-file.sub[file.sub %in% meta$ID[meta$Category == "FIC"]]

file.sub<-gsub(".txt",".entities", file.sub)

#check matching
file.sub<-file.sub[file.sub %in% filenames]

#establish character cut off
n=50

count.df<-data.frame(matrix(0, nrow = n, ncol = length(file.sub)))

#for every book
for (i in 1:length(file.sub)){
  
  print(i)
  
  #filename
  fileID<-file.sub[i]
  
  #get entities
  ent.df<-read.csv(file.sub[i], quote="", sep="\t")
    
  #keep only persons
  char.df<-ent.df[ent.df$cat == "PER",]
  
  #table co-reference counts
  char.v<-unname(sort(table(char.df$COREF), decreasing = T))
  
  # Make sure the vector has exactly 50 elements
  if(length(char.v) > 50) {
    # If the vector is longer than 50, truncate it
    char.v <- char.v[1:50]
  } else if(length(char.v) < 50) {
    # If the vector is shorter than 50, extend it with 0s
    char.v <- c(char.v, rep(0, 50 - length(char.v)))
  }
  
  #normalize by word count
  char.v<-char.v/(max(char.df$end_token))
  
  #bind into a data.frame
  count.df[,i]<-char.v
  
}

#get mean for each row
mean.v<-round(apply(count.df, 1, mean), digits = 4)

#get confidence interval
calculate_metrics <- function(data_column) {
  n <- length(data_column)
  mean <- mean(data_column)
  sd <- sd(data_column)
  se <- sd / sqrt(n)
  ci_lower <- mean - qt(0.975, df=n-1) * se
  ci_upper <- mean + qt(0.975, df=n-1) * se
  
  return(c(mean=mean, sd=sd, se=se, ci_lower=ci_lower, ci_upper=ci_upper))
}

#get standard error and CI
error.df<-round(apply(count.df, 1, calculate_metrics), digits=5)
error.df<-t(error.df)
error.df<-as.data.frame(error.df)

#add class
#error.df$class<-"C21"
#error.df$class<-"C20"
#error.df$class<-"C19"

#add index
error.df$index<-seq(1,50)
final.df<-rbind(final.df, error.df)

#plot
library(ggplot2)
ggplot(final.df, aes(x=index, y=mean*100, color=class)) +
  #geom_point(shape = 19, size = 3) + #15 = square, 19 = circle
  geom_point(size=3) +
  geom_line(linetype = "dashed") +
  scale_colour_manual(values=c("black", "grey60")) +
  #scale_colour_manual(values=c("deeppink", "grey40")) +
  #theme_bw() + 
  #xlim(1,50) +
  theme_classic() + 
  xlab("Character Rank") +
  ylab("Frequency (%)") +
  #theme(legend.title = element_blank()) +
  theme(
    legend.background = element_rect(color = "black", fill = "white"),
    legend.box.background = element_rect(color = "black", fill = "white")
  ) +
  labs(color = "Period") +
  theme(legend.position = c(0.8, 0.8)) +
  theme(legend.text = element_text(size=12))
  #theme(legend.position="bottom") +
  #ggtitle("Percentage of ")+
  #labs(
  #  caption="Source:",
  #  title="Your Main Title Here",  # Add your main title
  #  subtitle="Your Subtitle Here"  # Add your subtitle
  #)

