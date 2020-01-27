library(sentimentr)
library(dplyr)
library(cld3) # For detecting languages
library(tm) # For text mining
library(wordcloud2) # For word cloud
set.seed(1234)

#reviews_dataset <- read.csv("Data/reviews_detailed.csv", encoding='UTF-8')
#reviews_dataset <- reviews_dataset %>% select(comments)
#reviews_dataset$comments <- as.character(reviews_dataset$comments)

# Filter only english comments (better understanding for sentimentr
#reviews_dataset <- reviews_dataset %>% filter(detect_language(comments) == 'en')

## Perform Sentiment analysis
#reviews_dataset <- reviews_dataset %>% mutate(sentiment = sentiment_by(comments)$ave_sentiment)

# write.csv(reviews_dataset, "Data/reviews_detailed_sample.csv")

get_frequent_words <- function(dataset, positive) {
  if (positive) {
    dataset <- dataset %>% filter(sentiment > 0.1)
  }
  else {
    dataset <- dataset %>% filter(sentiment < -0.1)
  }
  
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
  
  frequent_words
}
