library(quanteda)
library(parallel)
library(foreach)

###########################################
# Knesser-Ney smoothing NOT COMPLETED
###########################################

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

