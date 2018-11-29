Word Prediction Shiny App      
========================================================
author: Rebecca Kotula
date: November 2018 
autosize: true
font-family: "Garamond"

Need help writing a speech? Can't think of what to say to your crush? Tired of your mom's boring old word predictor? 

Then this [Word Prediction Shiny App]() is for you!

App Information
========================================================

This app has an exciting user interface, quick response time, and the perfect amount of interactivity. 

Using text from blogs, newspapers, and twitter (all in English) provided by SwiftKey, this prediction model predicts the user's next word given text input. The app includes an interactive feature so that the user can simply click on the predicted word to add it to their sentence.

This app was created for the [Johns Hopkins Coursera Data Science Capstone.](https://www.coursera.org/learn/data-science-project/home/info)


Model
========================================================
The app works by using an n-gram backoff model. A basic idea of the steps the model takes are: 

1. Enter word or phrase of length n. If n > 3, keeps last 3 words of phrase. 
2. App searches dictionary of (n+1)grams for matches of the first n words.
    + If match, returns the most frequent (n+1)th word. 
    + If not, shortens the n-gram (removes first word). Then process repeats with new phrase and continues until match is found.
3. If no match is found, model returns most frequent unigram(single word).

What's so special about this app?
========================================================
Response time was sped up by:
- pre-cleaning data(including removing retweets(duplicate tweets) & filtering out n-grams with frequency < 10)
- using 50% of news and twitter data
- writing new, clean, tokenized data files to be used by app
- converting data frames to data tables to enable faster lookup

If the app can't find a match, it returns the highest frequency unigram so the user always has a new word to add to their sentence. 

Shiny App User Interface
========================================================
Here is an example of the app's user interface.

![plot of chunk unnamed-chunk-1](wordPredictor-figure/unnamed-chunk-1-1.png)

[Shiny App]() / [Github Repo]()
