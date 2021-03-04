#============================================
#Modelling Technics of Big Data Group Project
#============================================

library(stringr)
library(stringi)
library(XML)
library(RCurl)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(magrittr)
library(data.table)
library(tidytext)
library(dplyr)
library(sentimentr)
library(ggplot2)
library(syuzhet)
library(pander)

# read data and reduce data table size
amzn <- read.csv("Amazon_data.csv") %>% data.table %>% `[` (1:10000,)

# check data
any(is.na(amzn))
# False

View(amzn)
str(amzn)

# check what are the 5 categories
unique(amzn[, "Category"])
# smartTv, mobile, books, mobile accessories, refrigerator

# extract the comments of each category
smartTV <- amzn[`Category` == "smartTv"][, "Review_text"] 
mobile <- amzn[`Category` == "mobile"][, "Review_text"] 
books <- amzn[`Category` == "books"][, "Review_text"] 
mobile_acc <- amzn[`Category` == "mobile accessories"][, "Review_text"] 
refrigerator <- amzn[`Category` == "refrigerator"][, "Review_text"] 

# define a function to use space as a replacement
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))

# define a function of data cleaning, no of words and frequency calculation, and plot word cloud and bar chart of frequent words
y <- function(documents) {
  
  # store comments of each category as a collection of documents
  documents <- Corpus(VectorSource(documents))
  
  # replace special characters with space 
  documents <- tm_map(documents, toSpace, "/") %>% tm_map(toSpace, "@") %>% tm_map(toSpace, "\\|")
  
  # convert text to lowercase
  documents <- tm_map(documents, content_transformer(tolower)) 
  
  # remove numbers
  documents <- tm_map(documents, removeNumbers) 
  
  # delete English stopwords
  documents <- tm_map(documents, removeWords, stopwords("english"))
  
  # delete punctuation
  documents <- tm_map(documents, removePunctuation)
  
  # remove adiitional empty space
  documents <- tm_map(documents, stripWhitespace)
  
  # most frequent term
  freq <- TermDocumentMatrix(documents)
  word_freq <- as.matrix(freq) 
  
  # most frequent term in the corpus
  v <- sort(rowSums(word_freq), decreasing = T)
  d <- data.frame(word = names(v), freq = v)

  # plot word cloud of the most frequent words
  wordcloud(words = d$word, freq = d$freq, min.freq = 5, max.words = 200, random.order = F, colors = brewer.pal(8, "Dark2"), random.color = T)
  
  # frequent words
  findFreqTerms(freq, lowfreq = 4)
  
  # barplot of word frequency
  barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word, col = "lightblue", main = "Most Frequent Words", ylab = "Word Frequency")
}

# text analysis and graphs of the 5 categories
y(smartTV)
# Top 3 frequently used word by smartTv reviewers were: good, product, quality

y(mobile)
# Top 3 frequently used word by mobile reviewers were: good, phone, battery

y(books)
# Top 3 frequently used word by books reviewers were: book, good, read

y(mobile_acc)
# Top 3 frequently used word by mobile accessories reviewers were: good, phone, camera

y(refrigerator)
# Top 3 frequently used word by refrigerator reviewers were: good, product, quality

# define a function of sentiment analysis
s <- function(text) {
  
  # collapse the text to form a unique text
  plain_text <- paste(text, collapse = "\n")
  
  # compute the sentiment of each sentence
  sentiment(plain_text)
  
  # extract terms with sentiment
  extract_sentiment_terms(plain_text)
  
  # compute the general sentiment of the text
  sentiment_by(plain_text)
}

# sentiment analysis on the 5 categories
sentiment_smartTv <- s(smartTV)
sentiment_smartTv_error <- 1.96 * (sentiment_smartTv$sd/sqrt(sentiment_smartTv$word_count))
sentiment_smartTv$lowerCI = sentiment_smartTv$ave_sentiment-sentiment_smartTv_error
sentiment_smartTv$upperCI = sentiment_smartTv$ave_sentiment+sentiment_smartTv_error
sentiment_smartTv
# element_id word_count     sd ave_sentiment   upperCI  lowerCI
#          1      58574 0.4054     0.2722251 0.2755083 0.268942
# We are 95% confident that the sentiment expressed by smart tv reviewers were positive 
# with average sentiment score between (0.268942, 0.2722251)

sentiment_mobile <- s(mobile)
sentiment_mobile_error <- 1.96 * (sentiment_mobile$sd/sqrt(sentiment_mobile$word_count))
sentiment_mobile$lowerCI = sentiment_mobile$ave_sentiment-sentiment_mobile_error
sentiment_mobile$upperCI = sentiment_mobile$ave_sentiment+sentiment_mobile_error
sentiment_mobile
# element_id word_count        sd ave_sentiment   lowerCI   upperCI
#          1     104358 0.3961957     0.2506635 0.2482597 0.2530674
# We are 95% confident that the sentiment expressed by mobile reviewers were positive 
# with average sentiment score between (0.2482597, 0.2530674)

sentiment_books <- s(books)
sentiment_books_error <- 1.96 * (sentiment_books$sd/sqrt(sentiment_books$word_count))
sentiment_books$lowerCI = sentiment_books$ave_sentiment-sentiment_books_error
sentiment_books$upperCI = sentiment_books$ave_sentiment+sentiment_books_error
sentiment_books
# element_id word_count        sd ave_sentiment   lowerCI   upperCI
#          1       8819 0.3811749     0.4056987 0.3977432 0.4136543
# We are 95% confident that the sentiment expressed by books reviewers were positive 
# with average sentiment score between (0.3977432, 0.4136543)

sentiment_mobileAcc <- s(mobile_acc)
sentiment_mobileAcc_error <- 1.96 * (sentiment_mobileAcc$sd/sqrt(sentiment_mobileAcc$word_count))
sentiment_mobileAcc$lowerCI = sentiment_mobileAcc$ave_sentiment-sentiment_mobileAcc_error
sentiment_mobileAcc$upperCI = sentiment_mobileAcc$ave_sentiment+sentiment_mobileAcc_error
sentiment_mobileAcc
# element_id word_count        sd ave_sentiment   lowerCI   upperCI
#          1      64007 0.4123912     0.2517823 0.2485874 0.2549772
# We are 95% confident that the sentiment expressed by mobileAcc reviewers were positive 
# with average sentiment score between (0.2485874, 0.2549772)

sentiment_refrig <- s(refrigerator)
sentiment_refrig_error <- 1.96 * (sentiment_refrig$sd/sqrt(sentiment_refrig$word_count))
sentiment_refrig$lowerCI = sentiment_refrig$ave_sentiment-sentiment_refrig_error
sentiment_refrig$upperCI = sentiment_refrig$ave_sentiment+sentiment_refrig_error
sentiment_refrig
# element_id word_count        sd ave_sentiment   lowerCI   upperCI
#         1      17380 0.3926353     0.3016402 0.2958028 0.3074776
# We are 95% confident that the sentiment expressed by refrig reviewers were positive 
# with average sentiment score between (0.2958028, 0.3074776)

# Plotting at Aggregated Sentiment
out <- with(amzn, sentiment_by(get_sentences(Review_text), list(Category)))
plot(out)
# few review text with extreme negative sentiment expressed (outliers)


