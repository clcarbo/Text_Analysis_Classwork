#############################
##  Text_Analysis_Lab_5
##  Document Classification and Machine Learning
##  Author: Christopher Carbonaro
##  Date: 1 March 2019
#############################

##### Directory Configuration and Package loading #####
rm(list = ls())

setwd("C:/Users/Christopher/Dropbox/College/Junior Year Round 2/Spring Semester/Text Analysis/Text_Analysis_Lab_5")

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
       factoextra)

##### Section 1: Classification #####

### Loading and Cleaning our corpus

disaster_raw <- readRDS(file = "disaster_classify.rds") 
disaster_corpus <- VCorpus(VectorSource(disaster_raw$text))

## Defining special corpus cleaning functions

# At some point I will make a function which generates custom cleaning functions and cuts down on this repeated text. I just haven't sat down and diagrammed out which argments need to be mapped and which functions need be rested inside each other. A goal for a future project, I suppose.

removeURL <- content_transformer(function(x){
  gsub(pattern = "http[^[:space:]]*", 
       replacement = " ", 
       x = x)
  }
)

removeUserNames <- content_transformer(function(x){
  gsub(pattern = "@\\w+",
       replacement = " ",
       x = x)
  }
)

removeSpecialChar <- content_transformer(function(x){
  gsub(pattern = "[^\x20-\x7E]",
       replacement = " ",
       x = x)
  }
)

removeRogueTags <- content_transformer(function(x){
  gsub(pattern = "# ",
       replacement = " ",
       x = x)
  }
)

attachHash <- content_transformer(function(x){
  gsub(pattern = "# ",
       replacement = "#",
       x = x)
  }
)

removeMostPunctuation <- content_transformer(function(x){
  x <- gsub("#", "\002", x)
  x <- gsub("[[:punct:]]+", "", x)
  x <- gsub("\002", "#", x, fixed = TRUE)
  }
)
  

## To avoid continually reassigning our corpus name to a new, modified corpus, I have taken advantage of the pipe tool from the 'dplyr' package (contained within the tidyverse). See https://r4ds.had.co.nz/pipes.html for a brief overview of how the pipe works.

disaster_corpus_cleaned <- tm_map(disaster_corpus, removeURL) %>%
  tm_map(removeUserNames) %>%
  tm_map(removeSpecialChar) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeMostPunctuation) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(kind = "en")) %>%
  tm_map(removeRogueTags) %>%
  tm_map(stripWhitespace) %>%
  convert.tm.to.character() %>%
  lemmatize_strings(dictionary = lexicon::hash_lemmas) %>%
  VectorSource() %>%
  VCorpus() %>%
  tm_map(attachHash)

## Checking to see what the content of our first three cleaned documents is
for (i in 1:3){
  inspect(disaster_corpus_cleaned[[i]])
}

## Everything looks the way it should. Creating a Document-Term Matrix
disaster_DTM <- DocumentTermMatrix(disaster_corpus_cleaned) %>%
  removeSparseTerms(0.999)

inspect(disaster_DTM[1:10, 1:10])

### Partitioning our data and creating a classifier
## Splitting our data into test and training data
disaster_raw_train <- disaster_raw[1:3000,]
disaster_raw_test <- disaster_raw[3001:nrow(disaster_raw),]

disaster_corpus_cleaned_train <- disaster_corpus_cleaned[1:3000]
disaster_corpus_cleaned_test <- disaster_corpus_cleaned[3001:length(disaster_corpus_cleaned)]

disaster_DTM_train <- disaster_DTM[1:3000,]
disaster_DTM_test <- disaster_DTM[(3001):nrow(disaster_DTM),]


# Checking proportions between test and training data
prop.table(table(disaster_raw_test$relevant))
prop.table(table(disaster_raw_train$relevant))

## Removing terms which do not appear in the training data
at_least_once <- findFreqTerms(disaster_DTM_train, 1)
test_matrix <- as.matrix(DocumentTermMatrix(disaster_corpus_cleaned_test, list(dictionary = at_least_once)))
train_matrix <- as.matrix(DocumentTermMatrix(disaster_corpus_cleaned_train, list(dictionary = at_least_once)))

## Binarizing
binarize <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, 
              levels = c(0, 1), 
              labels = c("No", "Yes"))
}

test_matrix <- apply(test_matrix, 
                     MARGIN = 2, 
                     binarize)
train_matrix <- apply(train_matrix, 
                      MARGIN = 2, 
                      binarize)

## Creating our classifier
disaster_nb_relevant <- naiveBayes(train_matrix, 
                                   disaster_raw_train$relevant,
                                   laplace = 1)

disaster_nb_relevant$tables$riot
disaster_nb_relevant$tables$intrusion

## Predicting relevancy
disaster_relevant_predict <- predict(disaster_nb_relevant, 
                                     test_matrix)

## Assessing our model's accuracy
confusionMatrix(disaster_relevant_predict, 
                disaster_raw_test$relevant)


##### Section 2: Clustering #####

## Loading and cleaning our corpus/constructing a DTM
health_raw <- readRDS("health_cluster.rds")
health_corpus <- VCorpus(VectorSource(health_raw$content))

health_corpus_cleaned <- tm_map(health_corpus, removeURL) %>%
  tm_map(removeUserNames) %>%
  tm_map(removeSpecialChar) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeMostPunctuation) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords(kind = "en")) %>%
  tm_map(removeRogueTags) %>%
  tm_map(stripWhitespace) %>%
  convert.tm.to.character() %>%
  lemmatize_strings(dictionary = lexicon::hash_lemmas) %>%
  VectorSource() %>%
  VCorpus() %>%
  tm_map(attachHash)

health_DTM <- DocumentTermMatrix(health_corpus_cleaned) %>%
  removeSparseTerms(0.999)

for (i in 1:3) {
  inspect(health_corpus_cleaned[[i]])
}


## Building a matrix which records the differences in cosines
health_DTM <- as.matrix(health_DTM)
health_cosine <- sim2(health_DTM,
                      method = "cosine",
                      norm = "l2")

health_cosine_dis <- 1 - health_cosine

## Making a function which calculates silhouette distances by number of clusters
ward.D2 = function(diss, k){hcut(diss,
                                 k, 
                                 hc_method = "ward.D2")}

## Visualizing these distances
fviz_nbclust(health_DTM, 
             FUNcluster = ward.D2, 
             method = "silhouette", 
             diss = health_cosine_dis, 
             k.max = 20)

## Dividing the documents into 10 clusters
health_clusters <- hcut(as.dist(health_cosine_dis), k = 10, method = "ward.D2", isdiss = T)

## Finding the most frequent terms by cluster
p_words <- colSums(health_DTM) / sum(health_DTM)
cluster_words <- lapply(unique(health_clusters$cluster), 
                        function(x){
  rows <- health_DTM[health_clusters$cluster == x,]
  rows <- rows[, colSums(rows) > 0]
  colSums(rows)/sum(rows) - p_words[colnames(rows)]
})

## Consolidating these terms into a dataframe
cluster_summary <- data.frame(cluster = unique(health_clusters$cluster),
  size = as.numeric(table(health_clusters$cluster)),
  top_words = sapply(cluster_words,
                     function(d){
                       paste(names(d)[order(d,
                                            decreasing = TRUE)][1:5],
                             collapse = ", ")
                              }),
                    stringsAsFactors = FALSE)

cluster_summary

save.image("C:/Users/Christopher/Dropbox/College/Junior Year Round 2/Spring Semester/Text Analysis/Text_Analysis_Lab_5/Text_Analysis_Lab_5_Environment.RData")