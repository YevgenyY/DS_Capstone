library(quanteda)
library(parallel)
library(foreach)

setwd("~/Coursera/DS_Capstone/")
source("code/raw/stats_helper.R")
#load(file="data/freq.Rda")
load(file="data/f12345.Rda")
#load(file="data/pfs.Rda")

q1 <- "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"
q2 <- "You're the reason why I smile everyday. Can you follow me please? It would mean the"
q3 <- "Hey sunshine, can you follow me and make me the"
q4 <- "Very early observations on the Bills game: Offense still struggling but the"
q5 <- "Go on a romantic date at the"
q6 <- "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my"
q7 <- "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some"
q8 <- "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little"
q9 <- "Be grateful for the good times and keep the faith during the"
q10 <- "If this isn't the cutest thing you've ever seen, then you must be"

# Compute P for 1-grams 
pf1 <- f1/N 
pf1 <- data.frame(cbind(f1,pf1))
names(pf1) <- c("f","p")
save(pf1, file="data/mle.pf1.Rda")

# Compute P for 2-grams 
tmp <- names(f2)
no_cores <- detectCores()-1
cl <- makeCluster(no_cores)
t <- parSapply(cl, tmp, function(x) {return(strsplit(x, split=" ")[[1]][1]) })
rm(tmp)
pf2 <- data.frame(cbind(f2,f2/f1[t]))
names(pf2) <- c("f","p")
save(pf2, file="data/mle.pf2.Rda")

# Compute P for 3-grams
tmp <- names(f3)
t <- parSapply(cl, tmp, function(x) { 
  t <- unlist(strsplit(x, split=" "))
  z <- t[1:length(t)-1]
  n <- paste(z, collapse = " ")
  return(n)
})
tmp <- f2[t]; rm(t)
tmp <- f3/tmp
pf3 <- data.frame(cbind(f3,tmp)); rm(tmp)
names(pf3) <- c("f","p")
save(pf3, file="data/mle.pf3.Rda")

# Compute P for 4-grams
tmp <- names(f4)
t <- parSapply(cl, tmp, function(x) { 
  t <- unlist(strsplit(x, split=" "))
  z <- t[1:length(t)-1]
  n <- paste(z, collapse = " ")
  return(n)
})
tmp <- f3[t]
tmp <- f4/tmp
pf4 <- data.frame(cbind(f4, tmp)); rm(tmp)
names(pf4) <- c("f","p")
save(pf4, file="data/mle.pf4.Rda")

# Compute P for 5-grams
tmp <- names(f5)
t <- parSapply(cl, tmp, function(x) { 
  t <- unlist(strsplit(x, split=" "))
  z <- t[1:length(t)-1]
  n <- paste(z, collapse = " ")
  return(n)
})
tmp <- f4[t]
tmp <- f5/tmp
pf5 <- data.frame(cbind(f5, tmp)); rm(tmp)
names(pf5) <- c("f","p")
save(pf5, file="data/mle.pf5.Rda")


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
