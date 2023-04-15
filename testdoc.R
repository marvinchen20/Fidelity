# Install required packages if not already installed
if (!requireNamespace("BTM", quietly = TRUE)) {
  install.packages("BTM")
}
if (!requireNamespace("tm", quietly = TRUE)) {
  install.packages("tm")
}

# Install required packages if not already installed
if (!requireNamespace("topicmodels", quietly = TRUE)) {
  install.packages("topicmodels")
}
if (!requireNamespace("ldatuning", quietly = TRUE)) {
  install.packages("ldatuning")
}
if (!requireNamespace("text2vec", quietly = TRUE)) {
  install.packages("text2vec")
}

# Load packages
library(text2vec)

# Load packages
library(topicmodels)
library(ldatuning)


# Load packages
library(BTM)
library(tm)
library(proxy)
library(cluster)
library(dbscan)

# Create a simple example dataset
data <- data.frame(
  text = c(
    "Apples are tasty and healthy.",
    "Dogs are great pets and loyal friends.",
    "Bananas are high in potassium.",
    "Cats can be very affectionate and independent.",
    "Strawberries are sweet and delicious.",
    "Elephants are large and intelligent animals.",
    "Oranges are a good source of vitamin C.",
    "Giraffes have long necks and legs.",
    "Blueberries are rich in antioxidants.",
    "Dolphins are known for their playful behavior."
  ),
  stringsAsFactors = FALSE
)

# Preprocess the data using the 'tm' package
corpus <- VCorpus(VectorSource(data$text))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)
dtm <- DocumentTermMatrix(corpus)

# Convert DocumentTermMatrix to a regular matrix
dtm_matrix <- as.matrix(dtm)
dist_matrix <- proxy::dist(as.matrix(dtm), method = "cosine")

result <- dbscan(dtm_matrix,0.1, )

# Convert dtm_matrix to a list of document-word pairs
doc_word_pairs <- lapply(1:nrow(dtm_matrix), function(x) {
  doc_tokens <- which(dtm_matrix[x, ] > 0)
  words <- rep(doc_tokens, dtm_matrix[x, doc_tokens])
  data.frame(doc_id = x, word_id = words)
})

# Combine the document-word pairs into a single data frame
tokens_df <- do.call(rbind, doc_word_pairs)

# Set the number of topics and iterations
k_btm <- 5 # Number of topics
iter <- 1000 # Number of iterations

# Fit the BTM model
btm_model <- BTM(data = tokens_df, k = k_btm, alpha = 0.5, beta = 0.01, iter = iter, trace = TRUE)

btm_coherence(btm_model,dtm_matrix,5)


# Set the number of topics
k_lda <- 2

# Fit the LDA model
lda_model <- LDA(dtm, k = k_lda, control = list(seed = 1234))



coherence_values_lda <- coherence(lda_model, method = "UMass", M = NULL)



btm_coherence(lda_model,dtm_matrix,10)
########
data <- data.frame(
  category1 = c("A", "A", "A", "B", "B", "B", "C", "C", "C"),
  category2 = c("X", "Y", "Y", "X", "X", "Y", "Y", "Y", "X")
)
contingency_table <- table(data$category1, data$category2)
print(contingency_table)
chi_square_test <- chisq.test(contingency_table)
# Chi-square statistic
chi_square_statistic <- chi_square_test$statistic
cat("Chi-square statistic:", chi_square_statistic, "\n")

# Degrees of freedom
degrees_of_freedom <- chi_square_test$parameter
cat("Degrees of freedom:", degrees_of_freedom, "\n")

# P-value
p_value <- chi_square_test$p.value
cat("P-value:", p_value, "\n")





