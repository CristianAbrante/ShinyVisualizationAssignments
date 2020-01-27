library(sentimentr)
library(dplyr)
library(cld3) # For detecting languages
library(tm) # For text mining
library(wordcloud2) # For word cloud


show_wordcloud <- function(dataset, positive) {
  dataset <- dataset %>% select(comments)
  # Filter only english comments (better understanding for sentimentr
  english_reviews_dataset <- dataset %>% filter(detect_language(comments) == 'en')

  ## Add sentiment row
  english_reviews_dataset <- english_reviews_dataset %>% mutate(sentiment = sentiment_by(comments)$ave_sentiment)

  dataset <- ifelse(positive,
                    english_reviews_dataset %>% filter(sentiment > 0.1),
                    english_reviews_dataset %>% filter(sentiment < -0.1))

  ## Read corpus
  docs <- Corpus(VectorSource(unlist(dataset$comments)))

  ## Preprocess corpus
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)

  ## Build term document matrix
  docs_term_matrix <- as.matrix(TermDocumentMatrix(docs))
  sorted_docs_term_matrix <- sort(rowSums(docs_term_matrix), decreasing=TRUE)
  frequent_words <- data.frame(word = names(sorted_docs_term_matrix), freq=sorted_docs_term_matrix)
  View(head(frequent_words, 10))

  ## Generate wordcloud
  set.seed(1234)
  wordcloud2(data=frequent_words, size=1.6, color='random-dark')
}

show_wordcloud(x, TRUE)

