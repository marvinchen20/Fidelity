
library(topicmodels)
library(tm)
library(tidytext)
library(tidyverse)
library(ldatuning)
texts <- c("I love pizza", "I enjoy playing football", "Pizza is delicious", "I like sports", "Football is my favorite sport", "Pizza and pasta are Italian")
corpus <- VCorpus(VectorSource(texts))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)
dtm <- DocumentTermMatrix(corpus)
topic_range <- 2:5
coherence_scores <- FindTopicsNumber(dtm, topics = topic_range, metrics = "Griffiths2004", method = "Gibbs", control = list(seed = 42), mc.cores = 2L)
coherence_scores
