###########################################
#######   Hathi 1M with HathiTools ########
######### by Andrew Piper     #############
######### CC By 4.0 License   #############

#This script is based on the package and vignette developed by Xavier Marquez and Ben Schmidt located here:
#https://xmarquez.github.io/hathiTools/

#Additions and changes are my own.

#It can be incorporated with the Hathi1M data listed here:


library(devtools)
#install_github("xmarquez/hathiTools")
library(hathiTools)
library(tidyverse)
library(slider)
library(ggrepel)

##########################################
##########   EXAMPLES - ALL DATA   #######
##########################################


###########    NGRAMS    ##############
#this gets the yearly frequency of a set of words within a date range and plots them

#list words
myWords<-c("democracy", "monarchy", "revolution")

#get counts
#lims changes the date range, default = 1750-2000
result <- query_bookworm(word = myWords, lims = c(1750, 2000), counttype = c("WordsPerMillion", "TextPercent"))

#plot
result %>%
  group_by(word, counttype) %>%
  mutate(rolling_avg = slider::slide_dbl(value, mean, .before = 10, .after = 10)) %>%
  ggplot(aes(x = date_year, color = word)) +
  geom_line(aes(y = value), alpha = 0.3) +
  geom_line(aes(x = date_year, y = rolling_avg)) +
  facet_wrap(~counttype) +
  labs(x = "Year", y = "", title = "Books published between 1750-2000",
       subtitle="10-year rolling mean", caption="Source: HathiTrust") +
  theme_bw()

#########   VOLUME COUNTS   #############

#get volume counts by language
total_texts <- query_bookworm(counttype = c("TotalTexts"), groups = c("date_year", "language"),
                              lims = c(0,2020))

#extract table of counts by language
lang.df<-data.frame(tapply(total_texts$value, total_texts$language, sum))
lang.df$language<-row.names(lang.df)
lang.df<-lang.df[order(-lang.df$tapply.total_texts.value..total_texts.language..sum.),]
colnames(lang.df)<-c("count", "language")


#plot
#this lumps less common languages
total_texts %>%
  filter(date_year > 1500, date_year < 2001) %>%
  mutate(language = fct_lump_n(language, 10, w = value)) %>%
  group_by(date_year, language) %>%
  summarise(value = sum(value)) %>%
  group_by(language) %>%
  mutate(label = ifelse(date_year == max(date_year), as.character(language), NA_character_)) %>%
  group_by(language) %>%
  mutate(rolling_avg = slider::slide_dbl(value, mean, .before = 10, .after = 10)) %>%
  ggplot() +
  geom_line(aes(x = date_year, y = rolling_avg, color = language), show.legend = FALSE) +
  geom_line(aes(x = date_year, y = value, color = language), show.legend = FALSE, alpha = 0.3) +
  geom_text_repel(aes(x = date_year, y = value, label = label, color = language), show.legend = FALSE) +
  scale_y_log10() +
  theme_bw() +
  labs(title = "Total texts per language in the HathiTrust bookworm", 
       subtitle = "Log scale. Less common languages grouped as 'other'. 10 year rolling average.", 
       x = "Year", y = "")


###########     BUILD DATASETS     ##############

#Build dataset based on a keyword (Slow!)
result2 <- workset_builder("revolution", volumes_only = TRUE)

#Build dataset based on an author
result3 <- workset_builder(name = "Jules Verne")

#Build dataset based on pre-existing list of Hathi IDs
result4<-read.csv("Fiction_metadata.tsv", sep="\t")

#Extract metadata for all selected volumes
tmp <- tempdir() 
meta <- get_workset_meta(result3[1:10,], metadata_dir = tmp)

#Extract Word Counts for a single volume
#choose which volume
i=1
tmp <- tempdir() 
extracted_features <- get_hathi_counts(result3$htid[i], dir = tmp)

#from existing list
i=1
tmp <- tempdir() 
extracted_features <- get_hathi_counts(result4$htid[i], dir = tmp)

#extract features for all volumes and store in single table
tmp <- tempdir() 
extracted_features<-NULL
for (i in 1:nrow(results3)){
  sub<-get_hathi_counts(result3$htid[i], dir = tmp)
  extracted_features<-rbind(extracted_features, sub)
}



