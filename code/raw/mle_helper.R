library(quanteda)

setwd("~/Coursera/DS_Capstone/")
load(file="data/freq_tokens_dfm.Rda")

q1 <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
alphabet.en <- c(stopwords("english"), 
                 "a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","q","p","r","s","t","u","v","w","x","y","z")

# return word #n-1
wnm1 <- function(x) { return(strsplit(x, " ")[[1]][1]) }

mle_bigram <- function(x1,x2) {
  p <- freq_two[paste(x1,x2,sep=" ")] / freq_one[paste(x1)]
  
  return(p)
}

ngram_mask <- function(x, ngram) {
  pattern <- paste("^",ngram, sep = '')
  return(subset(x, grepl(paste(pattern), names(x))))
}

head( ngram_mask(freq_two, "case") )

raw <- q1
raw <- gsub("[^[:alnum:][:space:]']", ' ', raw)
raw <- gsub('[[:digit:]]+', ' ', raw)
raw <- gsub('[[:punct:]]+', '', raw)
raw <- tolower(raw)

tokens <- tokenize(raw, simplify=FALSE)
tokens <- removeFeatures(tokens, c(stopwords("english"), "will", "ass", alphabet.en, profanityWords.en))

