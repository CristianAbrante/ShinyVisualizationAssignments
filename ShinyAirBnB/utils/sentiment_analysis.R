library(sentimentr)
library(dplyr)
library(cld3) # For detecting languages
library(tm) # For text mining
library(wordcloud) # For word cloud
set.seed(1234)

show_positive_wordcloud <- function(dataset, positive) {
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
  #head(frequent_words, 10)

  ## Generate wordcloud
  wordcloud(words = frequent_words$word, freq = frequent_words$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, random.color = T,
          colors=brewer.pal(8, "Dark2"))
}

show_positive_wordcloud(reviews_dataset, TRUE)