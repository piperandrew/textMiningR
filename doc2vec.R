####### Doc2vec #######
library(doc2vec)
library(tokenizers.bpe)
library(udpipe)
data(belgium_parliament, package = "tokenizers.bpe")
x <- subset(belgium_parliament, language %in% "dutch")
x <- data.frame(doc_id = sprintf("doc_%s", 1:nrow(x)), 
                text   = x$text, 
                stringsAsFactors = FALSE)
x$text   <- tolower(x$text)
x$text   <- gsub("[^[:alpha:]]", " ", x$text)
x$text   <- gsub("[[:space:]]+", " ", x$text)
x$text   <- trimws(x$text)
x$nwords <- txt_count(x$text, pattern = " ")
x        <- subset(x, nwords < 1000 & nchar(text) > 0)

#toy model
model <- paragraph2vec(x = x, type = "PV-DM", dim = 5, iter = 3,  
                       min_count = 5, lr = 0.05, threads = 1)

#better parameters
model <- paragraph2vec(x = x, type = "PV-DBOW", dim = 100, iter = 20, 
                       min_count = 5, lr = 0.05, threads = 4)

#observe embedding table for words or docs
embedding <- as.matrix(model, which = "words")
embedding <- as.matrix(model, which = "docs")
vocab     <- summary(model,   which = "docs")
vocab     <- summary(model,   which = "words")

#get nearest neighbors for words and docs
nn <- predict(model, newdata = c("proximus"), type = "nearest", which = "word2word", top_n = 5)
nn <- predict(model, newdata = c("doc_198"), type = "nearest", which = "doc2doc",   top_n = 5)


######     top2vec     ##########
library(doc2vec)
library(word2vec)
library(uwot)
library(dbscan)
data(be_parliament_2020, package = "doc2vec")
x      <- data.frame(doc_id = be_parliament_2020$doc_id,
                     text   = be_parliament_2020$text_nl,
                     stringsAsFactors = FALSE)
x$text <- txt_clean_word2vec(x$text)
x      <- subset(x, txt_count_words(text) < 1000)

#model doc2vec
d2v    <- paragraph2vec(x, type = "PV-DBOW", dim = 50, 
                        lr = 0.05, iter = 10,
                        window = 15, hs = TRUE, negative = 0,
                        sample = 0.00001, min_count = 5, 
                        threads = 1)

nn <- predict(d2v, newdata = c("proximus"), type = "nearest", which = "word2word", top_n = 5)

#modeltop2vec
#takes doc2vec output as input
model  <- top2vec(d2v, 
                  control.dbscan = list(minPts = 50), 
                  control.umap = list(n_neighbors = 15L, n_components = 3), umap = tumap, 
                  trace = TRUE)
info   <- summary(model, top_n = 10)
info$topwords


