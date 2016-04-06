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

t <- sapply(strsplit(names(f3), ' '), function(x) paste(x[1],x[3],collapse = ' '))
f22 <- f3; names(f22)<-t

t <- sapply(strsplit(names(f4), ' '), function(x) paste(x[1],x[4],collapse = ' '))
f23 <- f4; names(f23)<-t

t <- sapply(strsplit(names(f5), ' '), function(x) paste(x[1],x[5],collapse = ' '))
f24 <- f5; names(f24)<-t
which(names(f24) %in% "case beer") # check the answer

save(f22,f23,f24, file="data/f22_23_24.Rda")

predict <- function(q) {
  t <- tokis(q)
  len = length(t)
  Wi <- t[len]
  Wi1 <- t[len-1]
  Wi2 <- t[len-2]
  Wi3 <- t[len-3]
  Wi4 <- t[len-4]
  
  paste(Wi4,Wi3,Wi2,Wi2,Wi, collapse = " ")
}

