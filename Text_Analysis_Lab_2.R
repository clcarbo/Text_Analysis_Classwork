#############################
##  Text_Analysis_Lab_2
##  Organizing a corpus into a DTM and exploring term frequencies
##  Author: Christopher Carbonaro
##  Date: 8 February 2019
#############################

##### Organizing our environment and loading packages #####

## Begin by removing any potential residual objects from the environment
rm(list = ls())


## Setting the working directory
setwd("C:/Users/Christopher/Dropbox/College/Junior Year Round 2/Spring Semester/Text Analysis/Text_Analysis_Lab_2")

## Checking to see if any required packages are not installed -- if they are missing, they will be retrieved from CRAN and installed

req_packages <- c("tm", "tidyverse")

for (i in 1:length(req_packages)){
  if (req_packages[i] %in% row.names(installed.packages()) == FALSE){
    install.packages(req_packages[i])
  }
}

lapply(req_packages, require, character.only = TRUE)
## Loading the retrieved data

weather_tweets <- read_csv("C:/Users/Christopher/Dropbox/College/Junior Year Round 2/Spring Semester/Text Analysis/Text_Analysis_Lab_1/weather_tweets.csv")

##### Constructing a corpus and preprocessing it #####
weather_corpus <- VCorpus(VectorSource(weather_tweets$text))

# Checking to see that the corpus was properly constructed
inspect(weather_corpus[1:3])

### Removing twitter/internet specific terms
# Removing URLs from tweets
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
weather_corpus <- tm_map(weather_corpus, content_transformer(removeURL))

# Removing twitter handles
userNames <- function(x) gsub("@\\w+", "", x)
weather_corpus <- tm_map(weather_corpus, content_transformer(userNames))

# Removing special characters
removeSpecial <- function(x) gsub("[^\x20-\x7E]", "", x)
weather_corpus <- tm_map(weather_corpus, content_transformer(removeSpecial))

# Removing unicode encodings
removeUnicode <- function(x) gsub("<U+[^[:space:]]*>", "", x)
weather_corpus <- tm_map(weather_corpus, content_transformer(removeUnicode))

# Redefining the corpus as a plain-text document after using user-constructed functions
weather_corpus <- tm_map(weather_corpus, PlainTextDocument)

### Removing non-internet specific qualities
# Removing punctuation
weather_corpus <- tm_map(weather_corpus, removePunctuation)

# Converting all uppercase characters to lowercase
weather_corpus <- tm_map(weather_corpus, content_transformer(tolower))

# Removing Numbers
weather_corpus <- tm_map(weather_corpus, removeNumbers)

# Removing Stopwords
weather_corpus <- tm_map(weather_corpus, removeWords, stopwords("en"))

# Stemming
weather_corpus <- tm_map(weather_corpus, stemDocument)

# Removing excess whitespace
weather_corpus <- tm_map(weather_corpus, stripWhitespace)




##### Creating and manipulating a DTM #####

## Creating the DTM
weather_dtm <- DocumentTermMatrix(weather_corpus)

## Reassigning a rowname (this is unnecessary for us, given that we never converted our corpus from its original class. I include it here for reference, but I've commented it out so it does not run)
rownames(weather_dtm) <- 1:nrow(weather_dtm)

## Removing Sparse Terms. This is not working, for some reason.
inspect(removeSparseTerms(weather_dtm, .95))
inspect(removeSparseTerms(weather_dtm, .9))
inspect(removeSparseTerms(weather_dtm, .5))
inspect(removeSparseTerms(weather_dtm, .1))

# Summing each term's frequency and sorting them by frequency
freq.terms  <- colSums(as.matrix(weather_dtm))
freq.terms <- sort(freq.terms, decreasing = TRUE)

# Words which appear more than 5 times
findFreqTerms(weather_dtm, 5)

## Transposing vectors into dataframes
top.terms <- data.frame(freq.terms[1:10])
colnames(top.terms) <- c("Occurrence")
top.terms$terms <- factor(row.names(top.terms), levels = row.names(top.terms))

## Creating a bargraph of the most frequent terms
ggplot(top.terms) +
  geom_col(aes(terms, Occurrence), color= "grey", fill = "#ffff1a") +
  labs(x = "", title = "Frequency of 10 Most Common Terms") +
  scale_y_continuous(breaks = seq(0, 5000, 500)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"))

## Finding correlation between words
findAssocs(weather_dtm, "warm", 0.5)
findAssocs(weather_dtm, "polar", 0.5)
findAssocs(weather_dtm, "climat", 0.5)


##### Attempt to puzzle out extra credit #####
# Creating a TfIDF DTM
weather_dtm_tfidf <- weightTfIdf(weather_dtm)

doc_1 <- as.matrix(weather_dtm_tfidf[1,])
doc_2 <- as.matrix(weather_dtm_tfidf[2,])
doc_3 <- as.matrix(weather_dtm_tfidf[3,])
doc_4 <- as.matrix(weather_dtm_tfidf[4,])