#####################
### Text_Analysis_Lab_7
### Structured Topic Models
### Author: Christopher Carbonaro
### Instructor: Marshall Taylor
### Date: 2019 03 21
#####################

##### Configuring Working Directory and Loading Data #####

### Removing objects from env.
rm(list = ls())

## Setting WD
setwd("C:/Users/Christopher/Dropbox/College/Junior Year Round 2/Spring Semester/Text Analysis/Text_Analysis_Lab_7")

## Loading packages
require(pacman)

p_load(tm,
       stm,
       tidyverse,
       reshape2,
       tidytext,
       textstem,
       textreg,
       grid,
       LDAvis,
       servr)

devtools :: install_github("mroberts/stmBrowser",dependencies = T)

require(stmBrowser)


### Importing dataset and examining it as an object
un_GDC <- read_rds("un_corpus.rds")
str(un_GDC)
class(un_GDC)

### Making a Corpus and Preprocessing it
un_gd_corp <- VCorpus(VectorSource(un_GDC$texts))

# Defining a function to remove any characters which aren't alpha-numeric
remove_Special_Characters <- function(data){gsub("[^[:alnum:]]",
                                                 " ",
                                                 data)
}

### Preprocessing by removing punctuation, removing non-alphanumeric characters, making all words lowercase, removing stopwords, removing numbers, lemmatizing words, and then converting the corpus to a DTM

un_gd_processed <- un_gd_corp %>%
  tm_map(removePunctuation) %>%
  tm_map(content_transformer(remove_Special_Characters)) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, words = stopwords("en")) %>%
  tm_map(content_transformer(removeNumbers)) %>%
  tm_map(stripWhitespace) %>%
  convert.tm.to.character() %>%
  lemmatize_strings(dictionary = lexicon::hash_lemmas) %>%
  VectorSource() %>%
  VCorpus() %>%
  DocumentTermMatrix() %>%
  removeSparseTerms(.98)
  
un_gd_stmprep <- readCorpus(un_gd_processed, type = "slam")
un_gd_stmdeets <- prepDocuments(documents = un_gd_stmprep$documents,
                                vocab = un_gd_stmprep$vocab,
                                meta = select(un_GDC, -texts),
                                upper.thresh = 170)
docs <- un_gd_stmdeets$documents
vocab <- un_gd_stmdeets$vocab
meta <- un_gd_stmdeets$meta

topic_num <- searchK(docs,
                     vocab,
                     K = seq(10, 50, 10),
                     data = meta,
                     proportion = 0.5,
                     heldout.seed = 100,
                     seed = 100,
                     init.type = "LDA",
                     verbose = T)

UnGdModel1 <- stm(docs, vocab)
