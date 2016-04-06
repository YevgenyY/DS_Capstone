library(quanteda)
library(parallel)
#library(foreach)

setwd("~/Coursera/DS_Capstone/")
source("code/raw/stats_helper.R")
#load(file="data/freq.Rda")
load(file="data/f12345.Rda")

# use parallel computation
#tmp <- names(f2)
no_cores <- detectCores()-1
cl <- makeCluster(no_cores)

######## Knesser-Ney smoothing ##############

################## find Pcont for 2-gram ################## 
f2_N <- sum(f2)
#f2_first <- sapply(strsplit(names(f2), ' '), function(x) x[1])
f2_last <- sapply(strsplit(names(f2), ' '), function(x) x[2])

clusterExport(cl, "f2")
clusterExport(cl, "f2_last")

Cc2 <- parSapply(cl, names(f1), function(w) {
  # find number of all bigrams with last word 'w' 
  #ptm <- proc.time()
  #pattern <- paste(w, '$', sep = '')
  #Cc <- length(subset(f2, grepl(paste(pattern), namesf2)))
  #proc.time()-ptm
  Cc <- sum(f2[f2_last==w])
  return(Cc)
})

Pc2 <- Cc2 / f2_N
save(Cc2, Pc2, file="data/kn.Pc2.sum.Rda")


################## find Pcont for 3-gram ################## 
f3_N <- sum(f3)
#f3_first <- sapply(strsplit(names(f3), ' '), function(x) x[1])
f3_last <- sapply(strsplit(names(f3), ' '), function(x) x[3])

clusterExport(cl, "f3")
clusterExport(cl, "f3_last")

Cc3 <- parSapply(cl, names(f1), function(w) {
  ptm <- proc.time()
  Cc <- sum(f3[f3_last==w])
  proc.time()-ptm
  
  return(Cc)
})

Pc3 <- Cc3/f3_N
save(Cc3, Pc3, file="data/kn.Pc3.sum.Rda")

# find Pcont for 4-grams
f4_N <- sum(f4)
#f4_first <- sapply(strsplit(names(f4), ' '), function(x) x[1])
f4_last <- sapply(strsplit(names(f4), ' '), function(x) x[4])
clusterExport(cl, "f4")
clusterExport(cl, "f4_last")

Cc4 <- parSapply(cl, names(f1), function(w) {
  Cc <- sum(f4[f4_last==w])
  return(Cc)
})

Pc4 <- Cc4 / f4_N;
save(Cc4, Pc4, file = "data/kn.Pc4.sum.Rda")

# find Pcont for 5-grams
f5_N <- sum(f5)
#f5_first <- sapply(strsplit(names(f5), ' '), function(x) x[1])
f5_last <- sapply(strsplit(names(f5), ' '), function(x) x[5])
clusterExport(cl, "f5")
clusterExport(cl, "f5_last")

Cc5 <- parSapply(cl, names(f1), function(w) {
  Cc <- sum(f5[f5_last==w])
  return(Cc)
})

Pc5 <- Cc5 / f5_N;
save(Cc5, Pc5, file = "data/kn.Pc5.sum.Rda")

# find Clambda for 2-grams
f2_N <- length(f2)
f2_first <- sapply(strsplit(names(f2), ' '), function(x) x[1])
#f2_last <- sapply(strsplit(names(f5), ' '), function(x) x[2])
clusterExport(cl, "f2_first")

Cc2 <- parSapply(cl, names(f1), function(w) {
  Cc <- sum(f2[f2_first==w])
  return(Cc)
})

save(Cc2, file = "data/kn.CLambda.Rda")
load("data/kn.CLambda.Rda")




