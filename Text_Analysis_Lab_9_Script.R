#####################
### Text_Analysis_Lab_9
### Text Networks Pt. I
### Author: Christopher Carbonaro
### Instructor: Marshall Taylor
### Date: 2019 04 05
#####################

##### Configuring Working Directory and Loading Data #####

### Removing objects from env.
rm(list = ls())
require(pacman)
p_load(igraph,
       tm,
       tidyverse,
       devtools,
       htmlwidgets,
       textreg)
devtools::install_github("cbail/textnets")
require(textnets)

## Setting WD
setwd("/Volumes/GoogleDrive/My Drive/Text_Analysis/Text_Analysis_Lab_9")

## Importing Data and examining its structure
res_tweets <- readRDS("nyr_data.rds")
class(res_tweets)
str(res_tweets)
head(res_tweets$text2)

## Prepping text with the textnets package. The first object is creating a network from the relationships between the documents and the second object is creating a network from the relationships between words.
res_tweets_net_doc <- PrepText(res_tweets,
                               textvar = "text2", 
                               groupvar = "resolution_topics",
                               node_type = "groups",
                               tokenizer = "tweets",
                               remove_stop_words = T,
                               remove_numbers = T)

res_tweets_net_word <- PrepText(res_tweets,
                               textvar = "text2", 
                               groupvar = "resolution_topics",
                               node_type = "words",
                               tokenizer = "tweets",
                               remove_stop_words = T,
                               remove_numbers = T)

## Creating and graphing the networks. I do not suggest running line 53; the igraph is extremly large and is both computationally intensive to produce and unlikely to be informative. It has been commented out, but could be run by removing the hashtags before the command.
docNetwork <- CreateTextnet(res_tweets_net_doc)
VisTextNet(docNetwork)
wordNetwork <- CreateTextnet(res_tweets_net_word)
## VisTextNet(wordNetwork)
