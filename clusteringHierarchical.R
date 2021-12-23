######### Hierarchical Clustering ##########
######### by Andrew Piper ####################
######### CC By 4.0 License ##################

library("tm")
library("proxy")
setwd("~/Data")

#Dataset for this example:
#NovelEnglishGenderSample. https://doi.org/10.6084/m9.figshare.17433266.v1 

#make your DTM
corpus1 <- VCorpus(DirSource("NovelEnglishGenderSample", encoding = "UTF-8"), readerControl=list(language="English"))
corpus1 <- tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
f<-content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus1 <- tm_map(corpus1, f, "[[:punct:]]")
corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace)) 
corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf)))
dtm.scaled<-corpus1.dtm/row_sums(corpus1.dtm)
dtm.tfidf<-weightTfIdf(corpus1.dtm, normalize = TRUE)

### create the following tables #####

#ONLY STOPWORDS
stop<-stopwords("en")
stop<-unlist(strsplit(stop,"[[:punct:]]"))
stop<-unique(stop)
#add extra stopwords
stop<-append(stop, c("said", "one", "will"))
stop<-append(stop, tolower(as.roman(1:1000)))
dtm.stop<-as.matrix(dtm.scaled[ ,which(colnames(dtm.scaled) %in% stop)])

#STOPWORDS + NON-SPARSE WORDS
dtm.sparse.stop<-removeSparseTerms(dtm.scaled, 0.4)

#NO STOPWORDS + NON-SPARSE WORDS
dtm.nostop<-dtm.scaled[ ,which(!colnames(dtm.scaled) %in% stop)]
dtm.sparse<-removeSparseTerms(dtm.nostop, 0.4)

#TFIFDF VERSION NO-STOP, NON-SPARSE
dtm.tfidf<-weightTfIdf(dtm.sparse, normalize = TRUE)

####### Part 1: Build a Similarity Matrix #######
#in this section we will build a similarity matrix between documents given our feature space

#if you wish to see a list of all similarity measures
#we will be using "cosine" similarity as our default
summary(pr_DB)

#generate a similarity matrix using one of the DTMs
#method = "cosine" -- you can change "cosine" to any other method
sim.m<-as.matrix(simil(as.matrix(dtm.sparse), method = "cosine"))

#examine a single document
row.names(sim.m)
sort(sim.m[which(row.names(sim.m) == "Female_1813_Austen,Jane_PrideandPrejudice_Novel.txt"),], decreasing=T)[1:10]

#plot these relationships
target<-sort(sim.m[which(row.names(sim.m) == "Female_1813_Austen,Jane_PrideandPrejudice_Novel.txt"),], decreasing=T)
plot(target, main="Similarity of Novels to Pride and Prejudice", ylab="Cosine Similarity")

###########  Part 2: Clustering   ##########
library("cluster")
library("dendextend")
library("splitstackshape")

####  This takes your DTMs above as input  ######
#dtm.stop
#dtm.sparse.stop
#dtm.sparse
#dtm.tfidf

#make a similarity matrix
#compare sparse and stop
sim.m<-simil(as.matrix(dtm.stop), method = "cosine") #change cosine to correlation or Jaccard or any other measure

#transfer to a distance matrix
dist.m<-pr_simil2dist(sim.m)

#cluster your documents using hierarchical clustering
fit<-hclust(dist.m, method = "ward.D2") #other options for "ward.D2" = "complete", "single", or "centroid" (there are more...)

#predict the optimal number of clusters
dend_k<-find_k(fit, krange = 2:(nrow(dist.m)-1))

###### PLOT #########

#turn into a dendrogram object
dend<-as.dendrogram(fit)

#second color your branches by the predicted number of clusters
dend <- color_branches(dend, k = dend_k$nc)

#create a data frame that has your category labels from your filenames
#this splits on underscores, so in this example it takes the male/female labels from your filenames
#you can use other labels for this purpose from your sample data
label.df<-data.frame(labels(dend))
label.df<-cSplit(label.df, 'labels.dend.', sep="_", type.convert=FALSE)
label.df$filenames<-labels(dend)

#this defines as many colors as you have clusters
colors_to_use <- as.numeric(as.factor(label.df$labels.dend._1))

#this colors your labels according to your categories (i.e. male/female)
labels_colors(dend) <- colors_to_use+1

#set the text to be small
par(cex=0.2)

#plot
plot(dend, horiz=TRUE)

#clear all your graphing parameters
dev.off()

#######  Calculate Purity of Clusters   #########
#calculate how well sorted your clusters are by categories (male/female)
#the measure you will use is called a purity score

#first create a data frame that lists the clusters
cluster.df<-data.frame(dend_k$pamobject$clustering)
cluster.df$filenames<-row.names(cluster.df)

#second merge this table with your previous table above that has the category labels
purity.df<-merge(cluster.df, label.df, by="filenames")

#now run a loop that goes through each cluster and calculates a purity score for that cluster
#purity = the percentage of items that belong to the single largest category for that cluster
final.df<-NULL
for (i in 1:nlevels(as.factor(purity.df$dend_k.pamobject.clustering))){
  #first subset your data frame by each cluster
  sub<-purity.df[purity.df$dend_k.pamobject.clustering == i,]
  #calculate how many items are in a cluster
  items<-nrow(sub)
  #calculate the number of items for each category
  cat1<-length(which(sub$labels.dend._1 == levels(as.factor(sub$labels.dend._1))[1]))
  cat2<-length(which(sub$labels.dend._1 == levels(as.factor(sub$labels.dend._1))[2]))
  #calculate the purity score
  #it takes the group with the most items and divides by the total number of items
  purity.score<-max(cat1, cat2)/(cat1+cat2)
  if (cat1 > cat2) {
    cat.name<-levels(as.factor(sub$labels.dend._1))[1]
  } else {
    cat.name<-levels(as.factor(sub$labels.dend._1))[2]
  }
  temp.df<-data.frame(i, items, purity.score, cat.name)
  final.df<-rbind(final.df, temp.df)
}

#inspect how many texts you have per category to make sure they are balanced
#if you have more texts in one group it is more likely that they will dominate your clusters
table(purity.df$labels.dend._1)
