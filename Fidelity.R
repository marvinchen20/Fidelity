library(tidyverse)
library(tidytext)
library(janeaustenr)
library(stringr)
library(ggplot2)
IMDB.DataSet <- read.csv("D:/BU/Fidelity/IMDB Dataset.csv")
IMDB <- tibble(IMDB.DataSet)


book_words <- IMDB %>% 
  unnest_tokens(word, review)

data(stop_words)

tidy_books <- book_words %>%
  anti_join(stop_words)

tidy_books %>% count(word, sort = TRUE) 


tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 6000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)




