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

full_intersect <- function(x) {
  coeff <- c(1, 0.1, 0.02, 0.03)
  ai1 <- f21[f21$first==x,]; ai1$c <- ai1$c * coeff[1]
  ai2 <- f22[f22$first==x,]; ai2$c <- ai2$c * coeff[2]
  ai3 <- f23[f23$first==x,]; ai3$c <- ai3$c * coeff[3]
  ai4 <- f24[f24$first==x,]; ai4$c <- ai4$c * coeff[4]
  
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
  
  all <- rbind(ai1,ai2,ai3,ai4)
  if(length(all$c) > 0) {
    t <- aggregate(c ~ first+last, data=all,FUN=sum); 
    rm(all)
  
    out <- t # t[t$last %in% full_is,]
    out$c <- out$c / f1[x]
    out <- out[order(-out$c),]
  } else
    out <- NULL
  
  return( out )
}

full_join <- function(x) {
  ai1 <- as.character(f21[f21$first==x,]$last); print(length(ai1))
  ai2 <- as.character(f22[f22$first==x,]$last); print(length(ai2))
  ai3 <- as.character(f23[f23$first==x,]$last); print(length(ai3))
  ai4 <- as.character(f24[f24$first==x,]$last); print(length(ai4))
  
  full_join <- unique(c(ai1,ai2,ai3,ai4))
  
  print(length(full_join))
  
  return(full_join)
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

try.intersect.join <- function(x,y) {
  
  sentence <- tokis(x)
  #sentence <-sentence [! sentence %in% c("id","ill") ]
  wi <- sentence[length(sentence)]
  wi1 <- sentence[length(sentence)-1]
  wi2 <- sentence[length(sentence)-2]
  wi3 <- sentence[length(sentence)-3]
  wi4 <- sentence[length(sentence)-4]
  paste(wi4,wi3,wi2,wi1,wi, collapse = " ")
  
  # make intersection and then join the results
  jis <- data.frame()
  
  for (i in 0:4) {
    jis <- rbind(jis,full_intersect(sentence[length(sentence)-i]))
  }
  
  # summarize the counts
  jis <- aggregate(c ~ first+last, data=jis,FUN=sum);
  jis <- jis[order(-jis$c),]
  rownames(jis) <- NULL
  res <- c(rep(0,length(y))); 
  names(res) <- y;
  
  # print the result
  for(i in 1:length(y)) {
    t <- fg(y[i], jis)
    if(length(t$c > 0)) {
      print(t)
      out <- paste(y[i], sum(t$c), collapse = ":")
      print(out)
      e <- c(sum(t$c)); names(e) <- t$last[1]
      res[i] <- e
    }
  }
  
  print(res)
  
  # Add f2 coeff, i.e "case beer"
  coef <- 10
  bias <- 0.001
  first <- sentence[length(sentence)]
  for (i in 1:length(res)) {
      last <- names(res)[i]
      bigram <- paste(first, last, collapse = " ")
    
      if (!is.na(f2[bigram])) {
        if(res[i] == 0) { res[i] <- bias }
        res[i] <- res[i] * coef * f2[bigram]
        out <- paste("Rewarding \"", bigram, "\"multiplying by", coef, "=", res[i], collapse = " ")
        print(out)
      }
  }
  
  win <- names(res)[match(max(res),res)]
  out <- paste("The winner: ", win, res[win])
  print(out)
  
  out <- paste("Full intersect area length:", length(jis$last), collapse = " ")
  print(out)
  jis <- aggregate(c ~ last, data=jis,FUN=sum); 
  jis <- jis[order(-jis$c),]; rownames(jis)<-NULL
  out <- paste("Winner index in intersect area is:", rownames(fg(win, jis)))
  print(out)
  
  return(jis) 
}


































ptm <- proc.time()
Cc <- f22[f22$first=="nice",]
proc.time()-ptm

