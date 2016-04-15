setwd("~/Coursera/DS_Capstone/")
source("code/final/0_stats_helper.R")
load(file="data/f12345.Rda")
load(file="data/f21_22_23_24.Rda")

# fast grep
fg <- function(x, y) {  y[y[,"last"]==x,] }  

full_join <- function(x) {
  coeff <- c(1, 0.1, 0.05, 0.001)
  ai1 <- f21[f21$first==x,]; ai1$c <- ai1$c * coeff[1]
  ai2 <- f22[f22$first==x,]; ai2$c <- ai2$c * coeff[2]
  ai3 <- f23[f23$first==x,]; ai3$c <- ai3$c * coeff[3]
  ai4 <- f24[f24$first==x,]; ai4$c <- ai4$c * coeff[4]
  all <- rbind(ai1,ai2,ai3,ai4)
  
  if(length(all$c) > 0) {
    t <- aggregate(c ~ first+last, data=all,FUN=sum); 
    rm(all)
    
    t <- t[order(-t$c),]; rownames(t) <- NULL
    
    out <- t   # t[t$last %in% full_is,]
    out$c <- out$c / f1[x]
    out <- out[order(-out$c),]
  } else
    out <- NULL
  
  return( out )
}

try.predict <- function(x,y) {
  
  sentence <- tokis(x)
  # join generated bigrams
  jis <- data.frame()
  j <- length(sentence) - 1
  for (i in 0:j) {
    jis <- rbind(jis,full_join(sentence[j-i]))
  }
  
  # summarize the counts
  jis <- aggregate(c ~ first+last, data=jis,FUN=sum);
  jis <- jis[order(-jis$c),]
  rownames(jis) <- NULL
  res <- c(rep(0,length(y))); 
  names(res) <- y;
  
  # print the result
  for(i in 1:length(y)) {
    out <- paste("Processing word", y[i], collapse = " ")
    print(out)
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
  print("Post processing, stage 1, rewarding bigrams from the answer set.")
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
  
  print("Post processing, stage 2, checking ")
  len <- round(length(jis$c)*0.1,0)
  t <- jis[1:len,]
  l <- apply(t,1, function(x) return(paste(x[1],x[2],collapse=" ")))
  c <- sapply(l, function(x) return( as.numeric( f2[x])))
  
  
  return(jis) 
}


