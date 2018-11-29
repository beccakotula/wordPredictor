suppressPackageStartupMessages(c(library(ngram), library(quanteda), library(tm), library(data.table), library(stringr), library(stringi)))
predictionSorter <- function(sentence){
    n <- wordcount(sentence)
    sentence <- tolower(sentence)
    sentence <- str_replace_all(sentence, "[[:punct:]]", "")
    
    if (n >=3){
        sentence <- strsplit(sentence, " ")
        sentence <- sentence[[1]][(n-2):n]
        sentence <- paste0(sentence, collapse=" ")
        quadPredict(sentence)
    }
    else if (n==2){
        triPredict(sentence)
    }
    else{
        biPredict(sentence)
    }
}


quadPredict <- function(trigram) {
    ans <- quadgrams[like(word, paste0(trigram, " "))]
    if(dim(ans)[1] !=0){
        ans <- ans[1][[1]]
        ans <- as.character(ans)
        ans <- strsplit(ans, " ")[[1]][4]
        print(ans)
    }
    
    else{
        bigram <- strsplit(trigram, " ")
        bigram <- bigram[[1]][2:3]
        bigram <- paste0(bigram, collapse=" ")
        triPredict(bigram)
    }
}

triPredict <- function(bigram) {
    ans <- trigrams[like(word, paste0(bigram, " "))]
    if(dim(ans)[1] !=0){
        ans <- ans[1][[1]]
        ans <- as.character(ans)
        ans <- strsplit(ans, " ")[[1]][3]
        print(ans)
    }
    
    else{
        unigram <- strsplit(bigram, " ")
        unigram <- unigram[[1]][2]
        unigram <- paste0(unigram, collapse=" ")
        biPredict(unigram)
        
    }
}


biPredict <- function(unigram) {
    ans <- bigrams[like(word, paste0(unigram, " "))]
    if(dim(ans)[1] !=0){
        ans <- ans[1][[1]]
        ans <- as.character(ans)
        ans <- strsplit(ans, " ")[[1]][2]
        print(ans)
    }
    
    else {
        ans <- unigrams
        ans <- ans[1][[1]]
        ans <- as.character(ans)
        print(ans)
    }
}