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
stop_words <- rbind(stop_words,c("br","Smart"))





###  LDA
imdb_dtm <- IMDB %>%
  unnest_tokens(word, review) %>%
  anti_join(stop_words)%>%
  count(docs, word) %>%
  cast_dtm(docs, word, n)


ap_lda <- LDA(imdb_dtm, k = 10, control = list(seed = 1234))

ap_topics <- tidy(ap_lda, matrix = "beta")

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

