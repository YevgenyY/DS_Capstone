Coursera Data Science Capstone Project
========================================================
author: Yevgeny V. Yorkhov
date: 2016/04/17
width: 1440
height: 900


The challenge
========================================================

- Build the approach to word prediction with a suitable quality
- Use SwiftKey corpus of texts that is large enough
- Develop the algorithm using this approach which works withing the time and memory constraints


### The data product - Shiny App

- The next word predictive model is deployed as a shiny application wich can be accessible [here]()

### Using the App

- After opening the App please wait for a while (may be minute or two) because Shiny App is waking up
- Type a phrase or a sentence in the input text box
- Press "Submit" button to see the predicted word
- Based on your input, the App provides 10 top predictions for the next word.

The approach of bigrams generation
========================================================

The example:
"The guy in front of me just bought a pound of bacon, a bouquet, and ***a case of beer***". After text cleaning we get:  
***"guy" "front" "just"  "bought"  "pound" "bacon" "bouquet" "case"  "beer"***


We could have the following phrases where the words ***case*** and ***beer*** are connected to each other and can be combined into a ***bigram***:

- a ***case*** of ***beer***, that is equal: ***W<sub>i-1</sub>,W<sub>i</sub>***
- a ***case*** of dark ***beer*** , that is equal: ***W<sub>i-2</sub>,W<sub>i</sub>***
- a ***case*** of Irish stout ***beer***, that is equal: ***W<sub>i-3</sub>,W<sub>i</sub>***
- a ***case*** of dark Irish stout ***beer***, that is equal: ***W<sub>i-4</sub>,W<sub>i</sub>***

As well as generating bigrams we could generate tri/quad/penta-grams, but we can violate the time and memory constraints in that case.

Building the predictive model
========================================================

### Offline preparation
1. Read Swiftkey dataset and clean it (punctuation, profanity checking, lowercase, etc...)
2. Tokenize the dataset ( [quanteda] (https://cran.r-project.org/web/packages/quanteda/index.html) package is extremely useful)
3. Build ngram frequency tables
  - 1 / 2-ngram frequency tables, that are built on texts with removed [stopwords](http://www.inside-r.org/packages/cran/tm/docs/stopwords)
  - generate bigram tables as it is mentioned at the previous slide
  - 1 / 2 / 3 / 4 / 5-ngram tables, that are built on cleaned texts, but with *stopwords* included.


Two-phases word prediction algorithm
========================================================

1. Phase #1
  - Read user input and clean it (punctuation, profanity checking, lowercase, etc...)
  - Generate set of bigrams {W<sub>i-1</sub>,W<sub>i</sub>},{W<sub>i-2</sub>,W<sub>i</sub>},{W<sub>i-3</sub>,W<sub>i</sub>}, {W<sub>i-4</sub>,W<sub>i</sub>} using ngram frequency tables
  - Use [Katz's model](https://en.wikipedia.org/wiki/Katz's_back-off_model) to caclculate the probability of the next words
  - Build a word list sorted by P(W<sub>i-n</sub>,W<sub>i</sub>)

2. Phase #2
  - Extract 10% of words from sorted by P(W<sub>i-n</sub>,W<sub>i</sub>) list of words from Katz's model
  - Use ngram frequency tables with *stopwords* included for linear interpolation of next word prediction:

  \( \\ P(W_{i-4}...W_i) = \lambda_1 C(W_{i-1},W_i) + \lambda_2 C(W_{i-2},W_i) + 
  \lambda_3 C(W_{i-3},W_i) + \\ \\ \lambda_4 C(W_{i-4},W_i) + \lambda_5 P_{smoothing} \\ \)



