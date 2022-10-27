library(tidyverse)
library(tidytext)
library(janeaustenr)
library(stringr)
library(ggplot2)
IMDB.DataSet <- read.csv("IMDB Dataset.csv")
IMDB <- tibble(IMDB.Dataset)


book_words <- IMDB %>% 
  unnest_tokens(word, review)
data(stop_words)
stop_word <- c("br","SMART")
stop_words <- rbind(stop_word,stop_words)


tidy_books <- book_words %>%
  anti_join(stop_words)

h <- tidy_books %>% count(word, sort = TRUE) 


htidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 6000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

view(stop_words)

