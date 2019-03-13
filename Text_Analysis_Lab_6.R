#############################
##  Text_Analysis_Lab_6
##  Document Classification and Machine Learning
##  Author: Christopher Carbonaro
##  Date: 8 March 2019
#############################

##### Directory Configuration and Package loading #####
rm(list = ls())

setwd("C:/Users/Christopher/Dropbox/College/Junior Year Round 2/Spring Semester/Text Analysis/Text_Analysis_Lab_6")

require(pacman)

p_load(tidyverse, 
       tm, 
       e1071, 
       gmodels, 
       textstem, 
       textreg, 
       caret, 
       esquisse, 
       text2vec, 
       factoextra,
       reshape2,
       tidytext,
       topicmodels,
       quanteda.corpora,
       LDAvis,
       servr)

# Reading in our collection of articles and giving the corpus a variable name

aca_arts_corpus <- readRDS("article_corpus.rds")

# Defining a function to remove any characters which aren't alpha-numeric
remove_Special_Characters <- function(data){gsub("[^[:alnum:]]",
                                                 " ",
                                                 data)
}

# Preprocessing by removing punctuation, removing non-alphanumeric characters,
# making all words lowercase, removing stopwords, removing numbers, lemmatizing words, and then converting the corpus to a DTM

aca_arts_DTM <- tm_map(aca_arts_corpus, removePunctuation) %>%
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
  removeSparseTerms(0.95)

## Generating LDA models. I liked the way you manipulated R's workflow to quantify how long the LDA generation takes, so I've incorporated the same idea.

## I decided to define a function with one argument which determines how many topics are generated. I can then run this function and generate several topic models while only needing to specify the number of topics.

make_LDA <- function(i) {
  start <- Sys.time()
  assign(str_c("aca_LDA_", 
               i, 
               sep = ""),
         LDA(x = aca_arts_DTM,
             k = i,
             control = list(seed = 100)),
         envir = .GlobalEnv)
  end <- Sys.time()
  model_generation_time <- end - start
  print(model_generation_time)
}


#Generating topic models with 5, 8, 11, 14, and 17 topics. Each model is named "aca_LDA_x" where x is the number of models.

for(x in seq(5, 17, 3)) {
  make_LDA(x)
}
