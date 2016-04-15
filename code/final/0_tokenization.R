library(quanteda)
setwd("~/Coursera/DS_Capstone/")
source("code/final/0_stats_helper.R")


getCorpus <- function(filename, sampleSize, profanityWords) {
  conn <- file(filename,open="r")
  
  lines <- iconv(readLines(conn), to = "utf-8")
  if (sampleSize != 0) {
    # Sample data with uniform distribution
    rowNums <- round(runif(sampleSize, min=1, max=length(lines)),0)
    raw <- c(lines[1])
    for(i in rowNums) {
      raw <- c(raw, lines[i])
    }
    rm(lines)
  } else
    raw <- lines
  close(conn)
  
  # remove punctuation, numbers and tolower the content
  raw <- gsub("[^[:alnum:][:space:]']", ' ', raw)
  raw <- gsub('[[:digit:]]+', ' ', raw)
  raw <- gsub('[[:punct:]]+', '', raw)
  raw <- tolower(raw)
  
  txt<-corpus(raw, verbose=TRUE, toLower=TRUE, removeNumbers=TRUE, removePunct=TRUE,
              removeSeparators=TRUE, removeTwitter=FALSE, stem=FALSE, keptFeatures=NULL, 
              thesaurus=NULL, dictionary=NULL, groups=NULL)
  
  return(txt)
}

blog.en="data/final/en_US/en_US.blogs.txt"
news.en="data/final/en_US/en_US.news.txt"
twit.en="data/final/en_US/en_US.twitter.txt"

# Calculate lines to read for the corpus
lines.blog <- round(getBasicStats(blog.en)[2] * 0.25, 0)
lines.news <- round(getBasicStats(news.en)[2] * 0.25, 0)
lines.twit <- round(getBasicStats(twit.en)[2] * 0.25, 0)

# Read lines for the corpus
txt.blog <- getCorpus(blog.en, lines.blog, stop_words)
txt.news <- getCorpus(news.en, lines.news, stop_words)
txt.twit <- getCorpus(twit.en, lines.twit, stop_words)
txt <- c(txt.blog, txt.news, txt.twit)

# Extract tokens
tokens <- tokenize(txt, simplify=FALSE)
tokens <- removeFeatures(tokens, c(stopwords("english"), "will", "ass", "ill", "id", alphabet.en, profanityWords.en))
unigrams <- ngrams(tokens, n=1, skip=0, concatenator = " ")
bigrams <- ngrams(tokens, n=2, skip=0, concatenator = " ")
trigrams <- ngrams(tokens, n=3, skip=0, concatenator = " ")
quadgrams <- ngrams(tokens, n=4, skip=0, concatenator = " ")
pentagrams <- ngrams(tokens, n=5, skip=0, concatenator = " ")

dfm_one <- dfm(tokens, groups = NULL) 
dfm_two <- dfm(bigrams, groups = NULL)
dfm_tri <- dfm(trigrams, groups = NULL)
dfm_quad <- dfm(quadgrams, groups = NULL)
dfm_penta <- dfm(pentagrams, groups = NULL)

# Calculate frequency dictionaries
f1 <- sort(colSums(dfm_one), decreasing = TRUE)
f2 <- sort(colSums(dfm_two), decreasing = TRUE)
f3 <- sort(colSums(dfm_tri), decreasing = TRUE)
f4  <- sort(colSums(dfm_quad),  decreasing = TRUE)
f5 <- sort(colSums(dfm_penta), decreasing = TRUE)

N <- sum(ntoken(txt)) # corpus size
V <- length(f1)  # vocabulary size

# Save all
save(f1, f2, f3, f4, f5, N, V, file="data/f12345.Rda")

# Make tokens with stopwords
txt <- txt.en
tokens <- tokenize(txt, simplify=FALSE)
unigrams <- ngrams(tokens, n=1, skip=0, concatenator = " ")
bigrams <- ngrams(tokens, n=2, skip=0, concatenator = " ")
trigrams <- ngrams(tokens, n=3, skip=0, concatenator = " ")
quadgrams <- ngrams(tokens, n=4, skip=0, concatenator = " ")
pentagrams <- ngrams(tokens, n=5, skip=0, concatenator = " ")

dfm_one <- dfm(tokens, groups = NULL) 
dfm_two <- dfm(bigrams, groups = NULL)
dfm_tri <- dfm(trigrams, groups = NULL)
dfm_quad <- dfm(quadgrams, groups = NULL)
dfm_penta <- dfm(pentagrams, groups = NULL)

# Calculate frequency dictionaries
f1r <- sort(colSums(dfm_one), decreasing = TRUE)
f2r <- sort(colSums(dfm_two), decreasing = TRUE)
f3r <- sort(colSums(dfm_tri), decreasing = TRUE)
f4r <- sort(colSums(dfm_quad), decreasing = TRUE)
f5r <- sort(colSums(dfm_penta), decreasing = TRUE)

N <- sum(ntoken(txt)) # corpus size
V <- length(f1r)  # vocabulary size
save(f1r, f2r, f3r, f4r, f5r, N, V, file="data/f12345raw.Rda")
