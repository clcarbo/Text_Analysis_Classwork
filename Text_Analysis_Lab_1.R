#############################
##  Text_Analysis_Lab_1
##  Script for Scraping Tweets using 'rtweet'
##  Author: Christopher Carbonaro
##  Date: 1 February 2019
#############################


##### Organizing our environment and loading packages #####

## Begin by removing any potential residual objects from the environment
rm(list = ls())


## Setting the working directory
setwd("C:/Users/Christopher/Dropbox/College/Junior Year Round 2/Spring Semester/Text Analysis/Text_Analysis_Lab_1")

## Check to see if any of the necessary packages need installation.
req_packages <- c("rtweet", "tidyverse", "maps")

for(i in 1:3){
  if (req_packages[i] %in% row.names(installed.packages()) == FALSE){
    install.packages(req_packages[i])
  }
}

# Load all required packages
lapply(req_packages, require, character.only = TRUE)

##### Setting up rtweet #####

create_token(
  app = "basic_twitter_webscraper",
  consumer_key = "26g8Ts9rFGGVUbbLAJYaWe32K",
  consumer_secret = "vF2tHOgoobsPUs6Kz5trCfDUt4OvSKtfbHK4E2gniWHFGCJLPl",
  access_token = "1034881142850244608-m2lusmU2uattwZoVeAAYNNKeXyI31J",
  access_secret = "8xMaqtiiASMSH9ecEVDCta7MWvnE8RfHb3EL4fzmYlfhH")

## Collecting tweets which contain either the term 'polar vortex,' 'climate change,' or 'global warming.' Results are coerced into a dataframe/tibble. Total number of observations is limited to 10,000, retweets are excluded.
weather_tweets <- search_tweets(q = "'polar vortex' OR 'climate change' OR 'global warming'", n = 10000, include_rts = FALSE, lang = "en")



##### Examining the Data #####

## To peruse the collected tweets, use View(). Using the 'head()' function on the dataset provides a quick summary of the entire dataframe, but this is moderately unwiedly due to the number of variables.

head(weather_tweets, n = 10)

## To quickly preview the actual text of the tweets, use head() on the vector in the dataframe which contains the text.

head(weather_tweets$text, n = 10)

## Plotting these tweets by time. ts_plot is an extention of ggplot2's plotting system.
ts_plot(weather_tweets, color = "blue", size = 1.5) + labs(title = "Number of tweets mentioning 'global warming,'\n 'polar vortex,' or 'climate change'", x = "Date", y = "Number of Tweets")



##### Using the 'maps' package #####

## Collecting tweets with geocode information
weather_tweets_w_location <- search_tweets(q = "'polar vortex' OR 'climate change' OR 'global warming'", n = 10000, include_rts = FALSE, geocode = lookup_coords("usa"), lang = "en")


## Creating a vector with the latitude and longitude information included. All of this is done automatically by 'rtweet.'
pos <- lat_lng(weather_tweets_w_location)

## Setting up parameters for graphing the map
par(mar = c(0, 0, 0, 0))
maps::map("state", lwd = .25)
# plot latitude and longitude points onto the map called by the prior function
with(pos, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))


##### Preserving the data #####

## Saving the tweets as a CSV file (useful for writing report with rmarkdown)
write_as_csv(weather_tweets, file_name = "weather_tweets.csv", na = "NA")

write_as_csv(pos, file_name = "pos.csv", na = "NA")
