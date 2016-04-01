library(quanteda)
library(parallel)
library(foreach)

setwd("~/Coursera/DS_Capstone/")
source("code/raw/stats_helper.R")
#load(file="data/freq.Rda")
load(file="data/f12345.Rda")

# use parallel computation
tmp <- names(f2)
no_cores <- detectCores()-1
cl <- makeCluster(no_cores)

# Compute P for 1-gram (add one smoothing)
f1c <- (f1+1)*N/(N + V) # calculate new count - c* add-one smoothing
pf1 <- f1c/N            # normalize
pf1 <- data.frame(cbind(f1c,pf1))
names(pf1) <- c("f","p")
save(pf1, file="data/add_one.pf1.Rda")

# Compute P for 2-gram (add one smoothing)
f2c <- (f2+1)
tmp <- names(f2)
t <- parSapply(cl, tmp, function(x) { 
  t <- unlist(strsplit(x, split=" "))
  z <- t[1:length(t)-1]
  n <- paste(z, collapse = " ")
  return(n)
})
tmp <- f1[t]; rm(t)
tmp <- f2c/(tmp + V) # add-one smoothing
pf2 <- data.frame(cbind(f3,tmp)); rm(tmp)
names(pf2) <- c("f","p")
save(pf2, file="data/add_one.pf2.Rda")

# Compute P for 3-gram (add one smoothing)
f3c <- (f3+1)
tmp <- names(f3)
t <- parSapply(cl, tmp, function(x) { 
  t <- unlist(strsplit(x, split=" "))
  z <- t[1:length(t)-1]
  n <- paste(z, collapse = " ")
  return(n)
})
tmp <- f2[t]; rm(t)
tmp <- f3c/(tmp + V) # add-one smoothing
pf3 <- data.frame(cbind(f3,tmp)); rm(tmp)
names(pf3) <- c("f","p")
save(pf3, file="data/add_one.pf3.Rda")

# Compute P for 4-gram (add one smoothing)
f4c <- (f4+1)
tmp <- names(f4)
t <- parSapply(cl, tmp, function(x) { 
  t <- unlist(strsplit(x, split=" "))
  z <- t[1:length(t)-1]
  n <- paste(z, collapse = " ")
  return(n)
})
tmp <- f3[t]; rm(t)
tmp <- f4c/(tmp + V) # add-one smoothing
pf4 <- data.frame(cbind(f4,tmp)); rm(tmp)
names(pf4) <- c("f","p")
save(pf4, file="data/add_one.pf4.Rda")

# Compute P for 5-gram (add one smoothing)
f5c <- (f5+1)
tmp <- names(f5)
t <- parSapply(cl, tmp, function(x) { 
  t <- unlist(strsplit(x, split=" "))
  z <- t[1:length(t)-1]
  n <- paste(z, collapse = " ")
  return(n)
})
tmp <- f4[t]; rm(t)
tmp <- f5c/(tmp + V) # add-one smoothing
pf5 <- data.frame(cbind(f5,tmp)); rm(tmp)
names(pf5) <- c("f","p")
save(pf5, file="data/add_one.pf5.Rda")

# Compute P for 3-grams Katz back-off model
# https://en.wikipedia.org/wiki/Katz's_back-off_model
K <- 3
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

clusterExport(cl, "pf2"); clusterExport(cl, "pf3"); clusterExport(cl, "K")
#clusterExport(cl, "except_last_word"); 
t1 <- parSapply(cl, names(f3), function(x) {
  #t1 <- sapply(names(f3), function(x) {
  t <- unlist(strsplit(x, split=" "))
  z <- t[1:length(t)-1]
  n <- paste(z, collapse = " ")
  if(x <= K) {
    return(pf2[n])
  } else
    return(pf3[x])
})
