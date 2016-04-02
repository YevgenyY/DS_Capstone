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

########### Good-Turing smoothing ##########
# Find Nc for 1-grams
clusterExport(cl, "f1")
f1Nc <- parSapply(cl, f1, function(x) {
  t <- f1 == x
  return(sum(t))
})
# find discount for 1-grams
clusterExport(cl, "f1Nc")
f1Dc <- parSapply(cl, names(f1), function(x) {
  Nc <- f1Nc[x]   # "said" has c=76778, but Nc=1
  cp1<- f1[x] + 1 # c + 1
  #i <- match(cp1, f1)
  for(i in 1:length(f1)) {
    if(cp1 <= f1[i])
      break
  }
  
  if (i == length(f1))
    Ncp1 <- 0
  else {
    j <- names(f1[i])
    Ncp1 <- f1Nc[j]
  }
  return(Ncp1/Nc)
})
pf1 <- (f1+1)*f1Dc
save(pf1, file="data/good_turing.pf1.Rda")



