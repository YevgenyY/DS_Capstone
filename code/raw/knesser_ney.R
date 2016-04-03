library(quanteda)
library(parallel)
#library(foreach)

setwd("~/Coursera/DS_Capstone/")
source("code/raw/stats_helper.R")
#load(file="data/freq.Rda")
load(file="data/f12345small.Rda")

# use parallel computation
#tmp <- names(f2)
no_cores <- detectCores()-1
cl <- makeCluster(no_cores)

######## Knesser-Ney smoothing ##############

# find Pcont
f2_N <- length(f2)
f2_first <- sapply(strsplit(names(f2), ' '), function(x) x[1])
f2_second <- sapply(strsplit(names(f2), ' '), function(x) x[2])

clusterExport(cl, "f2_N")
clusterExport(cl, "f2_second")

#Cc1 <- parSapply(cl, names(f1), function(w) {
Cc1 <- sapply(names(f1), function(w) { 
  # get last word
  # s <- unlist(strsplit(x, split=" "))
  # w <- s[1:length(s)-1]
  
  # find number of all bigrams with last word 'w' 
  #ptm <- proc.time()
  #pattern <- paste(w, '$', sep = '')
  #Cc <- length(subset(f2, grepl(paste(pattern), namesf2)))
  #proc.time()-ptm
  Cc <- sum(f2_second==w)
  return(Cc)
})

ptm <- proc.time()
sum(f2_second=="just")
proc.time() - ptm

save(Pc1, file="data/kn.pc1.Rda")


