library(quanteda)

load(file="data/freq_tokens_dfm.Rda")

mle_bigram <- function(x1,x2) {
  p <- freq_two[paste(x1,x2,sep=" ")] / freq_one[paste(x1)]
  
  return(p)
}

