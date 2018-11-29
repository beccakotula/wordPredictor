suppressPackageStartupMessages(c(library(ngram), library(quanteda), library(tm), library(data.table), library(stringr), library(stringi)))

con <- file("/Users/rebeccakotula/Desktop/Coursera/Data Science Capstone/final/en_US/en_US.twitter.txt", "r") 
twitter <- readLines(con)
close(con)

con <- file("/Users/rebeccakotula/Desktop/Coursera/Data Science Capstone/final/en_US/en_US.news.txt", "r") 
newsText <- readLines(con)
close(con)

twitter <- unique(twitter)

set.seed(8)
newsSub <- newsText[(rbinom(length(newsText)*.5, length(newsText), .5))]
set.seed(8)
twitterSub <- twitter[(rbinom(length(twitter)*.5, length(twitter), .5))]

write.csv(newsSub, file = "~/Desktop/Coursera/Data Science Capstone/Sample2/newsSample.csv", row.names = FALSE)
write.csv(twitterSub, file = "~/Desktop/Coursera/Data Science Capstone/Sample2/twitterSample.csv", row.names = FALSE)

corpus <-VCorpus(DirSource("/Users/rebeccakotula/Desktop/Coursera/Data Science Capstone/Sample2",encoding="UTF-8"))

rm(twitter)
rm(newsText)
rm(twitterSub)
rm(newsSub)

badWords <- read.csv("/Users/rebeccakotula/Desktop/Coursera/Data Science Capstone/BadWords.csv")

clean <- tm_map(corpus, removeNumbers)
clean <- tm_map(clean, removePunctuation)
clean <- tm_map(clean, tolower)
clean <- tm_map(clean, stripWhitespace)
clean <- tm_map(clean, removeWords, badWords)
final <- tm_map(clean, PlainTextDocument)

rm(badWords)
rm(clean)
rm(corpus)

# 1-grams
unigramTokenizer <- function(x) {
    unlist(lapply(ngrams(words(x), 1), paste, collapse = " "), use.names = FALSE)
}
tdm1 <- TermDocumentMatrix(final, control=list(tokenize = unigramTokenizer))

# 2-grams
bigramTokenizer <- function(x) {
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}
tdm2 <- TermDocumentMatrix(final, control=list(tokenize = bigramTokenizer))

# 3-grams
trigramTokenizer <- function(x) {
    unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
}
tdm3 <- TermDocumentMatrix(final, control=list(tokenize = trigramTokenizer))

# 4-grams
quadgramTokenizer <- function(x) {
    unlist(lapply(ngrams(words(x), 4), paste, collapse = " "), use.names = FALSE)
}
tdm4 <- TermDocumentMatrix(final, control=list(tokenize = quadgramTokenizer))

rm(final)

# Dictionary Creation 
unigramf <- findFreqTerms(tdm1,lowfreq =100)
unigramfreq <- rowSums(as.matrix(tdm1[unigramf,]))
unigramfreq <- data.frame(word=names(unigramfreq),frequency=unigramfreq)
sortUnigrams <- unigramfreq[order(unigramfreq$frequency, decreasing=TRUE),]
uni <- sortUnigrams[1,]

Bigramf <- findFreqTerms(tdm2,lowfreq =10)
Bigramfreq <- rowSums(as.matrix(tdm2[Bigramf,]))
Bigramfreq <- data.frame(word=names(Bigramfreq),frequency=Bigramfreq)
sortBigrams <- Bigramfreq[order(Bigramfreq$frequency, decreasing=TRUE),]

Trigramf <- findFreqTerms(tdm3,lowfreq =10)
Trigramfreq <- rowSums(as.matrix(tdm3[Trigramf,]))
Trigramfreq <- data.frame(word=names(Trigramfreq),frequency=Trigramfreq)
sortTrigrams <- Trigramfreq[order(Trigramfreq$frequency, decreasing=TRUE),]

Quadgramf <- findFreqTerms(tdm4,lowfreq =10)
Quadgramfreq <- rowSums(as.matrix(tdm4[Quadgramf,]))
Quadgramfreq <- data.frame(word=names(Quadgramfreq),frequency=Quadgramfreq)
sortQuadgrams <- Quadgramfreq[order(Quadgramfreq$frequency, decreasing=TRUE),]

rm(tdm1)
rm(tdm2)
rm(tdm3)
rm(tdm4)
rm(Quadgramf)
rm(Quadgramfreq)
rm(Bigramf)
rm(Bigramfreq)
rm(Trigramf)
rm(Trigramfreq)
rm(unigramf)
rm(unigramfreq)
rm(sortUnigrams)
rm(bigramTokenizer)
rm(unigramTokenizer)
rm(trigramTokenizer)
rm(quadgramTokenizer)

write.csv(sortUnigrams, file = "/Users/rebeccakotula/Desktop/Coursera/Data Science Capstone/Shiny/wordPredictor/Unigrams", row.names = FALSE)
write.csv(sortBigrams, file = "/Users/rebeccakotula/Desktop/Coursera/Data Science Capstone/Shiny/wordPredictor/Bigrams", row.names = FALSE)
write.csv(sortTrigrams, file = "/Users/rebeccakotula/Desktop/Coursera/Data Science Capstone/Shiny/wordPredictor/Trigrams", row.names = FALSE)
write.csv(sortQuadgrams, file = "/Users/rebeccakotula/Desktop/Coursera/Data Science Capstone/Shiny/wordPredictor/Quadgrams", row.names = FALSE)

uni <- read.csv("/Users/rebeccakotula/Desktop/Coursera/Data Science Capstone/Shiny/wordPredictor/Unigrams")
uni <- uni[1,]
sortBigrams <- read.csv("/Users/rebeccakotula/Desktop/Coursera/Data Science Capstone/Shiny/wordPredictor/Bigrams")
sortTrigrams <- read.csv("/Users/rebeccakotula/Desktop/Coursera/Data Science Capstone/Shiny/wordPredictor/Trigrams")
sortQuadgrams <- read.csv("/Users/rebeccakotula/Desktop/Coursera/Data Science Capstone/Shiny/wordPredictor/Quadgrams")

unigrams <- as.data.table(uni)
bigrams <- as.data.table(sortBigrams)
trigrams <- as.data.table(sortTrigrams)
quadgrams <- as.data.table(sortQuadgrams)

rm(uni)
rm(sortBigrams)
rm(sortTrigrams)
rm(sortQuadgrams)

#Prediction
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
    ans
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
        ans
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
    }
    
    else {
        ans <- unigrams
        ans <- ans[1][[1]]
        ans <- as.character(ans)
    
    }
    return(ans)
}

