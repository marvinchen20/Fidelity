library(tidyverse)
library(tidytext)
library(janeaustenr)
library(stringr)
library(ggplot2)
IMDB.DataSet <- read.csv("D:/BU/Fidelity/IMDB Dataset.csv")
IMDB <- tibble(IMDB.DataSet)


#unnest the review to words
book_words <- IMDB %>% 
  unnest_tokens(word, review,to_lower = F)

#filter all Uppercase words
tidy_test <- book_words %>%filter(upcheck(book_words$word) == T)


#attatch stop_words dataset
data(stop_words)
stop_word <- c("I","SMART")
stop_words <- rbind(stop_word,stop_words)


#anti join the stop words
tidy_books <- tidy_test %>%
  anti_join(stop_words)

test_tib <- tidy_test %>% count(word, sort = TRUE) 
test_tib <- test_tib %>% filter(word != "I")

#grapsh
tidy_test %>%
  count(words, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

#check the if words is uppercase
upcheck <- function(x){
  y <- toupper(x)
  x == y
}

