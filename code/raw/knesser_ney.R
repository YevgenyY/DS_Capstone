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

######## Knesser-Ney smoothing ##############

# find Pcont
f2len <- length(f2)
clusterExport(cl, "f2")
clusterExport(cl, "f2len")
tmp <- names(f1)
Pc1 <- parSapply(cl, tmp, function(x) { 
  # get last word
  #s <- unlist(strsplit(x, split=" "))
  #w <- s[length(s)]
  w <- x
  # find number of all bigrams with last word 'w' 
  pattern <- paste(w, '$', sep = '')
  Cc <- length(subset(f2, grepl(paste(pattern), names(f2))))
  
  return(Cc/f2len)
})

save(Pc1, file="data/kn.pc1.Rda")


