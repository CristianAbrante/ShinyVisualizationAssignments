library(sentimentr)
library(dplyr)
library(cld3) # For detecting languages
library(tm) # For text mining
library(wordcloud) # For word cloud
set.seed(1234)

reviews_dataset <- read.csv("Data/reviews_detailed_sample.csv", encoding = "UTF-8")
reviews_dataset$comments <- as.character(reviews_dataset$comments)

# Filter only english comments (better understanding for sentimentr
english_reviews_dataset <- reviews_dataset %>% filter(detect_language(comments) == 'en')

## Add sentiment row
english_reviews_dataset <- english_reviews_dataset %>% mutate(sentiment = sentiment_by(comments)$ave_sentiment)

## Positive comments
positive_dataset <- english_reviews_dataset %>% filter(sentiment > 0.1)
negative_dataset <- english_reviews_dataset %>% filter(sentiment < -0.1)

positive_comments <- unlist(positive_dataset$comments)
negative_comments <- unlist(negative_dataset$comments)

## Analyze corpus

# Load the data as a corpus
positive_docs <- Corpus(VectorSource(unlist(positive_dataset$comments)))
negative_docs <- Corpus(VectorSource(unlist(negative_dataset$comments)))

## Preprocess corpus
# Convert the text to lower case
positive_docs <- tm_map(positive_docs, content_transformer(tolower))
negative_docs <- tm_map(negative_docs, content_transformer(tolower))
# Remove numbers
positive_docs <- tm_map(positive_docs, removeNumbers)
negative_docs <- tm_map(negative_docs, removeNumbers)
# Remove english common stopwords
positive_docs <- tm_map(positive_docs, removeWords, stopwords("english"))
negative_docs <- tm_map(negative_docs, removeWords, stopwords("english"))
# Remove punctuations
positive_docs <- tm_map(positive_docs, removePunctuation)
negative_docs <- tm_map(negative_docs, removePunctuation)
# Eliminate extra white spaces
positive_docs <- tm_map(positive_docs, stripWhitespace)
negative_docs <- tm_map(negative_docs, stripWhitespace)

## Build term document matrix
positive_docs_term_matrix <- as.matrix(TermDocumentMatrix(positive_docs))
sorted_positive_docs_term_matrix <- sort(rowSums(positive_docs_term_matrix), decreasing=TRUE)
frequent_positive_words <- data.frame(word = names(sorted_positive_docs_term_matrix), freq=sorted_positive_docs_term_matrix)
head(frequent_positive_words, 10)

negative_docs_term_matrix <- as.matrix(TermDocumentMatrix(negative_docs))
sorted_negative_docs_term_matrix <- sort(rowSums(negative_docs_term_matrix), decreasing=TRUE)
frequent_negative_words <- data.frame(word = names(sorted_negative_docs_term_matrix), freq=sorted_negative_docs_term_matrix)
head(frequent_negative_words, 10)

## Generate positive word cloud
wordcloud(words = frequent_positive_words$word, freq = frequent_positive_words$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, random.color = T,
          colors=brewer.pal(8, "Dark2"))

## Generate negative word cloud
wordcloud(words = frequent_negative_words$word, freq = frequent_negative_words$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, random.color = T,
          colors=brewer.pal(8, "Dark2"))