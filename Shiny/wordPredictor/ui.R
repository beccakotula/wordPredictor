suppressPackageStartupMessages(c(library(ngram), library(quanteda), library(tm), library(data.table), library(stringr), library(stringi), library(shinythemes), library(shinyWidgets)))

# Define UI for application that predicts the next word given a string of words.
shinyUI(fluidPage(theme = shinytheme("journal"),
                  setBackgroundColor(color=c("gold", "coral"), gradient="radial", direction="right"),
                  
                  # Sidebar with a text input and submit button
                  sidebarLayout(
                      sidebarPanel( 
                          h1("Text Predictor"),
                          h4("This application will predict the next word given text input by the user. You may click the predicted word to append it to your sentence and the app will automatically update its prediction."), 
                          br(), 
                          h5("Created by Rebecca Kotula for the Johns Hopkins Coursera Data Science Capstone."), 
                          h6("November 2018")
                      ),
                      
                      # Show the predicted words
                      mainPanel(
                          textAreaInput('text',
                                        label=h3("Enter Text:"), placeholder = "Type here...", resize = "both"), 
                          h3('Your Sentence:'),
                          h4(textOutput('inputValue')),
                          br(),
                          h3('Predicted Word:'),
                          h5("(click to add to your sentence)"),
                          tags$head(
                              tags$style(HTML('#nextInput{background-color:#191970'))
                          ),
                          actionButton('nextInput', label='')
                      )
                  )))