suppressPackageStartupMessages(c(library(ngram), library(quanteda), library(tm), library(data.table), library(stringr), library(stringi), library(shinythemes)))

source("prep.R")
source("predict.R")

# Define server logic required to predict the next word
shinyServer(function(input, output, session) {
    
    wordPrediction <- eventReactive(input$text, {
        text <- input$text
        wordPrediction <- predictionSorter(input$text)
    })
    observeEvent(input$text, {
        updateActionButton(session, "nextInput", label=predictionSorter(input$text))
    })
    
    output$inputValue <- renderText({input$text}, quoted=FALSE)
    output$prediction <- renderPrint(wordPrediction())
    
    observeEvent(input$nextInput, {
        updateTextAreaInput(session, "text", value = paste(input$text, predictionSorter(input$text)))
    })
})
