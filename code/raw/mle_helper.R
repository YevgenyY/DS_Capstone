library(quanteda)

setwd("~/Coursera/DS_Capstone/")
source("code/raw/stats_helper.R")
load(file="data/freq.Rda")

q1 <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"

head( ngram_mask(freq_two, "case") )

penta <- ngrami(q1,5)
quad  <- ngrami(q1,4)
tri <- ngrami(q1,3)
two <- ngrami(q1,2)
one <- ngrami(q1,1)

# normalizing
normalize_freq <- function(freq) {
  x<-freq
  for (i in 1:length(x)) {
    w1 <- fwdw(names(x[i]), 1)
    total <- length(ngram_mask(x, w1)) 
    x[i] <- x[i] / total
  }
  return(x)  
}

f2n <- normalize_freq(f2)




