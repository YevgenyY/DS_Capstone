library(quanteda)
library(parallel)

setwd("~/Coursera/DS_Capstone/")
source("code/raw/stats_helper.R")
load(file="data/f12345.Rda")

###### Prepare new bigrams for prediction
# f21 - (wi-1,wi) = f2
# f22 - (wi-2,wi), i.e. "cant wait see" -> "cant see"
# f23 - (wi-3,wi), i.e. "happy st patrick day" -> "happy day"
# f24 - (wi-4,wi), i.e. "services llc amazon eu associates" -> "services associates"
# 
# test implementation
#t <- sapply(strsplit(names(f4), ' '), function(x) paste(x[1],x[4],collapse = ' '))
#f23 <- f4; names(f23)<-t

# calculate f22
first <- sapply(strsplit(names(f3), ' '), function(x) return(x[1]))
last <- sapply(strsplit(names(f3), ' '), function(x) return(x[3]))
f22 <- data.frame(cbind(f3,first,last))
names(f22) <- c("c", "first","last");  rownames(f22) <- NULL # save our memory
f22$c <- as.numeric(as.character(f22$c))

# calculate f23
first <- sapply(strsplit(names(f4), ' '), function(x) return(x[1]))
last <- sapply(strsplit(names(f4), ' '), function(x) return(x[4]))
f23 <- data.frame(cbind(f4,first,last))
names(f23) <- c("c", "first","last");  rownames(f23) <- NULL # save our memory
f23$c <- as.numeric(as.character(f23$c))

# calculate f24
first <- sapply(strsplit(names(f5), ' '), function(x) return(x[1]))
last <- sapply(strsplit(names(f5), ' '), function(x) return(x[5]))
f24 <- data.frame(cbind(f5,first,last))
names(f24) <- c("c", "first","last");  rownames(f24) <- NULL # save our memory
f24$c <- as.numeric(as.character(f24$c))

#t <- sapply(strsplit(names(f5), ' '), function(x) paste(x[1],x[5],collapse = ' '))
#f24 <- f5; names(f24)<-t
#which(names(f24) %in% "case beer") # check the answer

save(f22,f23,f24, file="data/f22_23_24.Rda")

my.predict <- function(q) {
  t <- tokis(q)
  len = length(t)
  Wi <- t[len]
  Wi1 <- t[len-1]
  Wi2 <- t[len-2]
  Wi3 <- t[len-3]
  Wi4 <- t[len-4]
  
  paste(Wi4,Wi3,Wi2,Wi1,Wi, collapse = " ")
  sWi2 <- f22[f22$first==Wi,]
  sWi3 <- f23[f23$first==Wi,]
  sWi4 <- f24[f24$first==Wi,]
  tWi <- intersect(s2$last,s3$last)
  tWi <- intersect(tWi,sWi4$last)

  sW12 <- f22[f22$first==Wi1,]
  sW13 <- f23[f23$first==Wi1,]
  sW14 <- f24[f24$first==Wi1,]
  tWi1 <- intersect(sW12$last,sW13$last)
  tWi1 <- intersect(tWi1,sW14$last)
  
  
}

 ptm <- proc.time()
Cc <- f22[f22$first=="nice",]
proc.time()-ptm

