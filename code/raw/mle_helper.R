library(quanteda)

setwd("~/Coursera/DS_Capstone/")
load(file="data/freq_tokens_dfm.Rda")

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

head( ngram_mask(freq_two, "years") )

