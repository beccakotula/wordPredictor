suppressPackageStartupMessages(c(library(ngram), library(quanteda), library(tm), library(data.table), library(stringr), library(stringi)))

#setwd("/Users/rebeccakotula/Desktop/Coursera/Data Science Capstone/Shiny/wordPredictor/")

uni <- read.csv("Unigrams")
uni <- uni[1,]
sortBigrams <- read.csv("Bigrams")
sortTrigrams <- read.csv("Trigrams")
sortQuadgrams <- read.csv("Quadgrams")

unigrams <- as.data.table(uni)
bigrams <- as.data.table(sortBigrams)
trigrams <- as.data.table(sortTrigrams)
quadgrams <- as.data.table(sortQuadgrams)

rm(uni)
rm(sortBigrams)
rm(sortTrigrams)
rm(sortQuadgrams)
