library(quanteda)
library(parallel)

setwd("~/Coursera/DS_Capstone/")
source("code/raw/stats_helper.R")
load(file="data/f12345.Rda")
load(file="data/f21_22_23_24.Rda")

###### Prepare new bigrams for prediction
# f21 - (wi-1,wi) = f2
# f22 - (wi-2,wi), i.e. "cant wait see" -> "cant see"
# f23 - (wi-3,wi), i.e. "happy st patrick day" -> "happy day"
# f24 - (wi-4,wi), i.e. "services llc amazon eu associates" -> "services associates"
# 
# test implementation
#t <- sapply(strsplit(names(f4), ' '), function(x) paste(x[1],x[4],collapse = ' '))
#f23 <- f4; names(f23)<-t


#t <- sapply(strsplit(names(f5), ' '), function(x) paste(x[1],x[5],collapse = ' '))
#f24 <- f5; names(f24)<-t
#which(names(f24) %in% "case beer") # check the answer

#save(f21,f22,f23,f24, file="data/f21_22_23_24.Rda")
load("data/f21_22_23_24.Rda")

fg <- function(x, y) {  y[y[,"last"]==x,] }  
# Try intersect Wi as Wi-1,Wi-2, Wi-3, Wi-4  
paste(Wi4,Wi3,Wi2,Wi1,Wi, collapse = " ")
x <- Wi

full_intersect <- function(x) {
  ai1 <- f21[f21$first==x,]
  ai2 <- f22[f22$first==x,]
  ai3 <- f23[f23$first==x,]
  ai4 <- f24[f24$first==x,]
  
  is12 <- intersect(ai1$last,ai2$last); length(is12)
  is13 <- intersect(ai1$last,ai3$last); length(is13)
  is14 <- intersect(ai1$last,ai4$last); length(is14)
  
  is23 <- intersect(ai2$last,ai3$last); length(is23)
  is24 <- intersect(ai2$last,ai4$last); length(is24)
  
  is34 <- intersect(ai3$last,ai4$last); length(is34)
  
  # full intersect
  a <- intersect(is12,is13); a <- intersect(a,is14)
  a <- intersect(a, is23); a <- intersect(a, is24)
  full_is <- intersect(a, is34) 
  
  return(full_is)
}

full_join <- function(x) {
  ai1 <- as.character(f21[f21$first==x,"last"])
  ai2 <- as.character(f22[f22$first==x,"last"])
  ai3 <- as.character(f23[f23$first==x,"last"])
  ai4 <- as.character(f24[f24$first==x,"last"])
  
  jn <- unique(c(ai1,ai2,ai3,ai4)); print(length(jn))
  
  return(jn)
}

try.join.intersect <- function(x,y) {
  sentence <- tokis(x)

  # joint intersection
  jis <- c()
  for (i in 0:4) {
    w <- sentence[length(sentence)-i]
    if (i==0) {
      jis <- full_join(w)
    } else {
      t <- full_join(w)
      jis <- intersect(jis, t)
    }
  }
  for(i in 1:length(y)) {
    out <- paste(y[i], sum(grepl(y[i], jis)), collapse = ":")
    print(out)
  }
  
  out <- paste("Join-intersect length", length(jis), collapse = " ")
  print(out)
  
  return(jis)
}

try.intersect <- function(x,y) {
  
  sentence <- tokis(x)
  wi <- sentence[length(sentence)]
  wi1 <- sentence[length(sentence)-1]
  wi2 <- sentence[length(sentence)-2]
  wi3 <- sentence[length(sentence)-3]
  wi4 <- sentence[length(sentence)-4]
  paste(wi4,wi3,wi2,wi1,wi, collapse = " ")
  
  # joint intersection
  jis <- c()
  
  for (i in 0:4) {
    jis <- c(jis,full_intersect(sentence[length(sentence)-i]))
  }
  
  for(i in 1:length(y)) {
    out <- paste(y[i], sum(grepl(y[i], jis)), collapse = ":")
    print(out)
  }
  
  
  out <- paste("Full intersect area length:", length(jis), collapse = " ")
  print(out)
  
  return(jis)
}


































ptm <- proc.time()
Cc <- f22[f22$first=="nice",]
proc.time()-ptm

