library(tidyverse)
library(tidytext)
library(janeaustenr)
library(stringr)
library(ggplot2)
library(topicmodels)
library(tm)
IMDB.Dataset <- read.csv("D:/BU/Fidelity/Fidelity Project/IMDB Dataset.csv")
IMDB <- tibble(IMDB.Dataset)

IMDB <- IMDB  %>%  mutate(docs = c(1:length(IMDB$review)))

data(stop_words)
my_stop_word <- c("br","Smart","film","Smart","movie","Smart","time","Smart")
stop_words <- rbind(stop_words,my_stop_word)





###cluster
imdb_dtm <- IMDB %>%
  unnest_tokens(word, review) %>%
  anti_join(stop_words)%>%
  count(docs, word) %>%
  cast_dtm(docs, word, n)







###  LDA
ap_lda <- LDA(imdb_dtm, k = 10, control = list(seed = 1234))

ap_topics <- tidy(ap_lda, matrix = "beta")

perplexity(ap_lda)

n_topics <- c(10:20)



ap_lda_compare <- n_topics %>% 
  map(LDA, x = imdb_dtm, control = list(seed = 1109)) 
 
perplexity(ap_lda_compare)

plot(n_topics,ap_lda_compare)



 





ggplot(ap_lda_compare , aes(x = k, y = perplexity)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("5-fold cross-validation of topic modelling with the 'Associated Press' dataset",
          "(ie five different models fit for each candidate number of topics)") +
  labs(x = "Candidate number of topics", y = "Perplexity when fitting the trained model to the hold-out set")

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

perplexity(ap_lda)
###




###(useless part)




book_words <- IMDB %>%
  unnest_tokens(word, review) %>%
  anti_join(stop_words)%>%
  count(docs, word, sort = TRUE)

total_words <- book_words %>% 
  group_by(docs) %>% 
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

freq_by_rank <- book_words %>% 
  group_by(docs) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total) %>%
  ungroup()

book_tf_idf <- book_words %>%
  bind_tf_idf(word, docs, n)

ap_lda <- LDA(book_tf_idf, k = 2, control = list(seed = 1234))

book_tf_idf






ldavis

