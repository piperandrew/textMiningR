####################################################
################ Fightin Words #####################
####################################################

#adapted from: 
#https://burtmonroe.github.io/TextAsDataCourse/Tutorials/TADA-FightinWords.nb.html

library(tidyr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(knitr)

######### Load functions ##########
fwgroups <- function(dtm, groups, pair = NULL, weights = rep(1,nrow(dtm)), k.prior = .1) {
  
  weights[is.na(weights)] <- 0
  
  weights <- weights/mean(weights)
  
  zero.doc <- rowSums(dtm)==0 | weights==0
  zero.term <- colSums(dtm[!zero.doc,])==0
  
  dtm.nz <- apply(dtm[!zero.doc,!zero.term],2,"*", weights[!zero.doc])
  
  g.prior <- tcrossprod(rowSums(dtm.nz),colSums(dtm.nz))/sum(dtm.nz)
  
  # 
  
  g.posterior <- as.matrix(dtm.nz + k.prior*g.prior)
  
  groups <- groups[!zero.doc]
  groups <- droplevels(groups)
  
  g.adtm <- as.matrix(aggregate(x=g.posterior,by=list(groups=groups),FUN=sum)[,-1])
  rownames(g.adtm) <- levels(groups)
  
  g.ladtm <- log(g.adtm)
  
  g.delta <- t(scale( t(scale(g.ladtm, center=T, scale=F)), center=T, scale=F))
  
  g.adtm_w <- -sweep(g.adtm,1,rowSums(g.adtm)) # terms not w spoken by k
  g.adtm_k <- -sweep(g.adtm,2,colSums(g.adtm)) # w spoken by groups other than k
  g.adtm_kw <- sum(g.adtm) - g.adtm_w - g.adtm_k - g.adtm # total terms not w or k 
  
  g.se <- sqrt(1/g.adtm + 1/g.adtm_w + 1/g.adtm_k + 1/g.adtm_kw)
  
  g.zeta <- g.delta/g.se
  
  g.counts <- as.matrix(aggregate(x=dtm.nz, by = list(groups=groups), FUN=sum)[,-1])
  
  if (!is.null(pair)) {
    pr.delta <- t(scale( t(scale(g.ladtm[pair,], center = T, scale =F)), center=T, scale=F))
    pr.adtm_w <- -sweep(g.adtm[pair,],1,rowSums(g.adtm[pair,]))
    pr.adtm_k <- -sweep(g.adtm[pair,],2,colSums(g.adtm[pair,])) # w spoken by groups other than k
    pr.adtm_kw <- sum(g.adtm[pair,]) - pr.adtm_w - pr.adtm_k - g.adtm[pair,] # total terms not w or k
    pr.se <- sqrt(1/g.adtm[pair,] + 1/pr.adtm_w + 1/pr.adtm_k + 1/pr.adtm_kw)
    pr.zeta <- pr.delta/pr.se
    
    return(list(zeta=pr.zeta[1,], delta=pr.delta[1,],se=pr.se[1,], counts = colSums(dtm.nz), acounts = colSums(g.adtm)))
  } else {
    return(list(zeta=g.zeta,delta=g.delta,se=g.se,counts=g.counts,acounts=g.adtm))
  }
}
############## FIGHTIN' WORDS PLOTTING FUNCTION
# helper function
makeTransparent<-function(someColor, alpha=100)
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}
fw.ggplot.groups <- function(fw.ch, groups.use = as.factor(rownames(fw.ch$zeta)), max.words = 50, max.countrank = 400, colorpalette=rep("black",length(groups.use)), sizescale=2, 
                             title="", subtitle = "", caption = "") {
  #Optional Title and Caption:
  #Comparison of Terms by Groups
  #Group-specific terms are ordered by Fightin' Words statistic (Monroe, et al. 2008)
  if (is.null(dim(fw.ch$zeta))) {## two-group fw object consists of vectors, not matrices
    zetarankmat <- cbind(rank(-fw.ch$zeta),rank(fw.ch$zeta))
    colnames(zetarankmat) <- groups.use
    countrank <- rank(-(fw.ch$counts))
  } else {
    zetarankmat <- apply(-fw.ch$zeta[groups.use,],1,rank)
    countrank <- rank(-colSums(fw.ch$counts))
  }
  wideplotmat <- as_tibble(cbind(zetarankmat,countrank=countrank))
  wideplotmat$term=names(countrank)
  #rankplot <- gather(wideplotmat, party, zetarank, 1:ncol(zetarankmat))
  rankplot <- gather(wideplotmat, groups.use, zetarank, 1:ncol(zetarankmat))
  rankplot$plotsize <- sizescale*(50/(rankplot$zetarank))^(1/4)
  rankplot <- rankplot[rankplot$zetarank < max.words + 1 & rankplot$countrank<max.countrank+1,]
  rankplot$groups.use <- factor(rankplot$groups.use,levels=groups.use)
  
  p <- ggplot(rankplot, aes((nrow(rankplot)-countrank)^1, -(zetarank^1), colour=groups.use)) + 
    geom_point(show.legend=F,size=sizescale/2) + 
    theme_classic() +
    theme(axis.ticks=element_blank(), axis.text=element_blank() ) +
    ylim(-max.words,40) +
    facet_grid(groups.use ~ .) +
    geom_text_repel(aes(label = term), size = rankplot$plotsize, point.padding=.05,
                    box.padding = unit(0.20, "lines"), show.legend=F) +
    scale_colour_manual(values = alpha(colorpalette, .7)) + 
    #    labs(x="Terms used more frequently overall →", y="Terms used more frequently by group →",  title=title, subtitle=subtitle , caption = caption) 
    labs(x=paste("Terms used more frequently overall -->"), y=paste("Terms used more frequently by group -->"),  title=title, subtitle=subtitle , caption = caption) 
  
}
fw.keys <- function(fw.ch,n.keys=10) {
  n.groups <- nrow(fw.ch$zeta)
  keys <- matrix("",n.keys,n.groups)
  colnames(keys) <- rownames(fw.ch$zeta)
  
  for (g in 1:n.groups) {
    keys[,g] <- names(sort(fw.ch$zeta[g,],dec=T)[1:n.keys])
  }
  keys
}

########## Example ########
#Using Sherlock Holmes (SHS) and Literary Short Stories (SHORT)
corpus1 <- VCorpus(DirSource("Short_Combined", encoding = "UTF-8"), readerControl=list(language="English"))
corpus1 <- tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
f<-content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus1 <- tm_map(corpus1, f, "[[:punct:]]")
corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace)) 
corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(3,Inf)))
#DTM W ONLY STOPWORDS
stop<-stopwords("en")
stop<-unlist(strsplit(stop,"[[:punct:]]"))
stop<-unique(stop)
dtm.stop<-as.matrix(corpus1.dtm[ ,which(colnames(corpus1.dtm) %in% stop)])
#load category data
gr<-read.csv("Short_meta.csv", header=F)
#run function
#takes as input a DTM + vector of group IDs
test<-fwgroups(dtm.stop,groups = factor(gr$V1))
test2 <- fw.keys(test, n.keys=10) #n.keys = number of keywords to test
kable(test2) #View most distinctive words in descending order
#plot
test3<-fw.ggplot.groups(test,sizescale=2,max.words=50,max.countrank=100,colorpalette=c("red","blue"))
test3


