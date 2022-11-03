library(tidyverse)
library(tidytext)
library(janeaustenr)
library(stringr)
library(ggplot2)
IMDB.DataSet <- read.csv("D:/BU/Fidelity/IMDB Dataset.csv")
IMDB <- tibble(IMDB.DataSet)

#check the if words is uppercase
upcheck <- function(x){
  y <- toupper(x)
  x == y
}

#unnest the review to words
book_words <- IMDB %>% 
  unnest_tokens(word, review,to_lower = F)

#filter all Uppercase words
tidy_test <- book_words %>%filter(upcheck(book_words$word) == T)
tidy_test <- tidy_test %>% 
  unnest_tokens(word,word)


#attatch stop_words dataset
data(stop_words)
stop_word <-select(stop_words,word)
stop_word1 <- c(1:1000)
stop_word1 <- as.data.frame(stop_word1) %>% rename(word = stop_word1)
stop_word2 <- c("tv","dvd","2001")
stop_word2 <- as.data.frame(stop_word2) %>% rename(word = stop_word2)

stop_word <- rbind.data.frame(stop_word,stop_word1,stop_word2)



#anti join the stop words
tidy_books <- tidy_test %>%
  anti_join(stop_word)

test_tib <- tidy_books %>% count(word, sort = TRUE) 


#grapsh
tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 200) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)


##About punctuation
#unnest the review to sentences
book_words <- IMDB %>% 
   unnest_tokens(sentence, review, token = "sentences")


#count frequency of punctuation
devtools::install_github("Amherst-Statistics/katherinemansfieldr")
library(katherinemansfieldr)
char <- extract_punct(book_words$sentence)
puncfreq <- charfreq(char, c('...', '?','!','"'), punctuation = TRUE)

#graph
puncfreq %>%
  ggplot(aes(
character, freq)) +
  geom_col() +
  labs(y = NULL)
