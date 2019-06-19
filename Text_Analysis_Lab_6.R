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


# Generating topic models with 5, 8, 11, 14, and 17 topics. Each model is named "aca_LDA_x" where x is the number of models.

topic_nums <- seq(5, 17, 3)

for(x in topic_nums) {
  make_LDA(x)
}

##### Visualizing and Playing with the Models #####

## Making a function which graphs the top n terms for each topic in a model. The code for this uses the "tidytext" package, which allows us to use Hadley Wickham's tidy workflow approach. We mutate the topic model using the LDA tidier which returns a dataframe of three columns - term, topic, and our beta values. We then group them by the topic variable, select the top "n" terms, arrange them by their beta values, and graph them using ggplot.

graph_n_terms <- function(model, n){
  tidy(model) %>%
  group_by(topic) %>%
  top_n(n, beta) %>%
  arrange(beta) %>%
  ggplot() +
  geom_col(aes(term, beta, fill = factor(topic))) +
  coord_flip() +
  facet_wrap(~topic, scales = "free")
}

for(i in topic_nums){
  graph_n_terms(model = get(str_c("aca_LDA_", i)), n = 15)
}

## The LDA tidier function doesn't seem to work when reloading an .RData file, so an alternative graphing function is provided below. This function works around the tidy function by assigning the beta values to the top_terms variable (which returns a matrix containing the beta values. The columns represent the terms and the rows represent the topics). We assign names to our columns, exponentiate our values, transpose the matrix, and specify which topics are which. We then select our top terms as before and graph them with ggplot. 

graph_n_terms_alt <- function(model, n){
  top_terms <- model@beta
  colnames(top_terms) <- model@terms
  top_terms <- exp(top_terms) %>%
    t()
  colnames(top_terms) <- str_c("topic", 
                               1:ncol(top_terms))
  
  top_terms <- melt(top_terms) %>%
    group_by(Var2) %>%
    top_n(n, 
          value) %>%
    ungroup() %>%
    arrange(Var2,
            -value) %>%
    mutate(order = row_number())
  
  ggplot(top_terms, 
         aes(rev(order), 
             value, 
             fill = Var2)) +
    geom_bar(stat = "identity",
             color = "black") +
    facet_wrap(~Var2, scales = "free") +
    coord_flip() +
    xlab("") +
    ylab("Term") +
    guides(fill = F) +
    scale_x_continuous(labels = top_terms$Var1,
                       breaks = rev(top_terms$order),
                       expand = c(0, 0))
}

for (i in topic_nums) {
  print(graph_n_terms_alt(model = get(str_c("aca_LDA_", i)), 15))
}

##### Visualizing Term Relevance #####

## Creating a JSON file





