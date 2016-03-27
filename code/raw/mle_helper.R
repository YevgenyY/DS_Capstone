library(quanteda)

setwd("~/Coursera/DS_Capstone/")
source("code/raw/stats_helper.R")
#load(file="data/freq.Rda")
#load(file="data/freq_tokens_dfm.Rda")

q1 <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
q2 <- "You're the reason why I smile everyday. Can you follow me please? It would mean the"
q3 <- "Hey sunshine, can you follow me and make me the"
q4 <- "Very early observations on the Bills game: Offense still struggling but the"
q5 <- "Go on a romantic date at the"
q6 <- "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my"
q7 <- "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some"
q8 <- "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little"
q9 <- "Be grateful for the good times and keep the faith during the"
q10 <- "If this isn't the cutest thing you've ever seen, then you must be"

prepare_bi <- function(x) {
  t <- toki(x)
  len <- length(t[[1]])
  mtx <- data.frame(matrix(0, nrow = len, ncol = len))
  names(mtx) <- paste(t[[1]]); rownames(mtx) <- paste(t[[1]])
  for (i in 1:len) {
    for (j in 1:len) {
      w1 <- t[[1]][i]
      w2 <- t[[1]][j]
      mtx[i,j] <- log(f2[paste(w1, w2, sep=" ")] / f1[w1])
    }
  }
  mtx[is.na(mtx)] <- 0
}

head( ngram_mask(f2, "case") )


q<-q1
penta <- rvsw(q,5)
quad  <- rvsw(q,4)
tri <- rvsw(q,3)
two <- rvsw(q,2)
one <- rvsw(q,1)

ngram_mask(f5, paste(quad))
ngram_mask(f4, paste(tri))
ngram_mask(f3, paste(two))
ngram_mask(f2, paste(one))
