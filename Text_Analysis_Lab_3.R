#############################
##  Text_Analysis_Lab_3
##  Dictionary-Based Methods and Sentiment Analysis
##  Author: Christopher Carbonaro
##  Date: 15 February 2019
#############################

##### Configuring Directory and installing packages #####

## Removing residual environmental objects and setting the working directory
rm(list = ls())
setwd("C:/Users/Christopher/Dropbox/College/Junior Year Round 2/Spring Semester/Text Analysis/Text_Analysis_Lab_3")

## Using the 'pacman' package to check the system and see whether the required packages are installed. If they are, they are loaded. If they are not, they are installed, then loaded.
require(pacman)
req_packages <- c("quanteda", "devtools", "stringr", "NLP", "tm", "textstem", "textreg", "quanteda.corpora", "slam", "sentimentr", "ggplot2", "reshape2")
p_load(req_packages, character.only = T)

##### Section 1: Selecting and Manipulating a Corpus #####

## Downloading the data
guardian_docs <- download("data_corpus_guardian")
## Turning the corpus into a dataframe containing the documents and data for them
guardian_docs <- guardian_docs$documents

## Creating two identical corpora for preprocessing. guardian_docs_tm.1 will be preprocessed for dictionary-based methods, and guardian_docs_tm.2 will be preprocessed for sentiment analysis.
guardian_docs_tm.1 <- VCorpus(VectorSource(guardian_docs$texts))
guardian_docs_tm.2 <- VCorpus(VectorSource(guardian_docs$texts))

## Cleaning guardian_docs_tm.1 for dictionary-based methods
# Constructing a function which replaces punctuation with a space
removeAllPunct <- function(x) gsub("[[:punct:]]", " ", x)
guardian_docs_tm.1 <- tm_map(guardian_docs_tm.1, removeAllPunct)
guardian_docs_tm.1 <- tm_map(guardian_docs_tm.1, PlainTextDocument)

# Building an applying a function which removes special characters from the corpus
removeSpecialCharacters <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
guardian_docs_tm.1 <- tm_map(guardian_docs_tm.1, removeSpecialCharacters)
guardian_docs_tm.1 <- tm_map(guardian_docs_tm.1, PlainTextDocument)

# Make capitalized words lowercase
guardian_docs_tm.1 <- tm_map(guardian_docs_tm.1, content_transformer(tolower))

# Remove any numberical characters
guardian_docs_tm.1 <- tm_map(guardian_docs_tm.1, removeNumbers)

# Remove stopwords
guardian_docs_tm.1 <- tm_map(guardian_docs_tm.1, removeWords, stopwords("en"))

# Remove excess whitespace
guardian_docs_tm.1 <- tm_map(guardian_docs_tm.1, stripWhitespace)

## Lemmatizing the corpus
guardian_docs_tm.1_lem <- convert.tm.to.character(guardian_docs_tm.1)
guardian_docs_tm.1_lem <- lemmatize_strings(guardian_docs_tm.1_lem, dictionary = lexicon::hash_lemmas)

# Adding the cleaned document texts back to the dataframe of texts defined at the document level
guardian_docs$docs_cleaned <- guardian_docs_tm.1_lem



## Cleaning guardian_docs_tm.2 for sentiment analysis
# Removing capital letters
guardian_docs_tm.2 <- tm_map(guardian_docs_tm.2, content_transformer(tolower))

# Removing numbers
guardian_docs_tm.2 <- tm_map(guardian_docs_tm.2, removeNumbers)

# Removing excess whitespace
guardian_docs_tm.2 <- tm_map(guardian_docs_tm.2, stripWhitespace)

# Convert corpus to character vector for sentiment analysis
guardian_sentiment <- convert.tm.to.character(guardian_docs_tm.2)




##### Section 2: Dictionary-Based Methods #####

## Making a dictionary of terms
brex_dic <- c("ukip", "brexit", "farage", "referendum")

## Making a dataframe of texts which contain the at least one of the desired terms
guardian_brexit <- guardian_docs[str_detect(guardian_docs$docs_cleaned, "ukip | brexit | farage | referendum"),]

## Converting our cleaned texts into a DTM
DTM_guardian <- as.matrix(DocumentTermMatrix(guardian_docs_tm.1))
row.names(DTM_guardian) <- c(1:nrow(DTM_guardian))

## Making a Document-Term Matrix which only counts the words in our dictionary
DTM_brex <- as.matrix(DocumentTermMatrix(guardian_docs_tm.1, list(dictionary = brex_dic)))
row.names(DTM_brex) <- row.names(guardian_docs_tm.1)

## Making a vector where the values are the total number of words in each document
total_words <- slam::row_sums(DTM_guardian)

## Making a vector where the values are the number of dictionary terms present in each document
dic_words <- slam::row_sums(DTM_brex)

## Normalizing term frequency by turning these sums into proporitons
dic_freq <- dic_words/total_words



##### Section 3: Sentiment Analysis #####
## Parsing Sentiment corpus into sentences
sentences <- get_sentences(guardian_sentiment[1:100])

## Calculating the sentiment polarity for each sentence
sent.sentiment <- sentiment(sentences[[2]])

## Plotting the sentiment polarity for the second document (taken essentially verbatim from pres42)
ggplot(sent.sentiment, 
       aes(x = element_id, y = sentiment,
           fill = ifelse(sent.sentiment$sentiment < 0, "0", "1"))) +
  geom_histogram(stat = "identity") +
  xlab("Sequence (in Sentences)") + ylab("Sentiment Polarity") +
  ylim(-1,1) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"),
        legend.position = "none")

## Extracting sentiment terms from the second document. To save rendering time, this was done on only the second document.
extract_sentiment_terms(guardian_docs$texts[2])

## Sentiment scores aggregated by document. Again, performed on only the second document.
sentiment.byDocument <- sentiment_by(sentences[[2]], by = NULL) 
highlight(sentiment.byDocument)

## load the syuzhet package
p_load(syuzhet)

## Calculate the raw emotion scores for documents. Again, due to memory constraints, I've limited this process to the first 100 documents.
emotions <- get_nrc_sentiment(guardian_sentiment[1:100])
# Normalize count
emotions.count <- emotions/total_words[1:100]

## Creating a boxplot from these scores
emotions <- melt(emotions.count, measure.vars = colnames(emotions.count))

ggplot(emotions[which(emotions$variable!="positive" & emotions$variable!="negative"),], 
       aes(x = as.factor(variable), y = value*100,
           fill = as.factor(variable))) +
  geom_boxplot(aes(fill = as.factor(variable)),
               outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.2) +
  labs(y = "% of document words that are Emotion Words", x = "") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_text(face = "bold"),
        legend.position = "right") +
  guides(fill = guide_legend(title = "Emotion"))

