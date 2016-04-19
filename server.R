library(shiny)

load(file="data/f12.Rda")
load(file="data/f12345raw.Rda")
load(file="data/f21_22_23_24.Rda")

# fast grep
fg <- function(x, y) {  y[y[,"last"]==x,] }  

# Generate and join bigrams
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

check_ngramm <- function(fNr, words, question, ngram_len) {
  ngram <- get_ngramm(question, ngram_len) 
  ngrams_ar <- sapply(words, function(x) { return ( paste(ngram, x, collapse = " ") ) })
  t <- fNr[names(fNr) %in% ngrams_ar]
  t<-t[order(-t)]
  
  return(t)
}


try.predict <- function(x) {
  
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

  out <- paste("Array length:", length(jis$last), collapse = " ")
  print(out)
  jis <- aggregate(c ~ last, data=jis,FUN=sum); 
  jis <- jis[order(-jis$c),]; rownames(jis)<-NULL
  out <- paste("Winner index in the array is:", rownames(fg(win, jis)))
  print(out)
  
  print("Post processing, stage 2, checking penta / quad / tri gramms")
  coef <- c(10, 5, 1)
  len <- round(length(jis$c)*0.1,0)
  t <- jis[1:len,]
  words <- as.character( t$last )
  
  # Collect ngrams
  out5 <- check_ngramm(f5r, words, x, 4) * coef[1] 
  out4 <- check_ngramm(f4r, words, x, 3) * coef[2]
  out3 <- check_ngramm(f3r, words, x, 2) * coef[3]
  
  # Normalize ngrams
  ngr <- get_ngramm(x, 4); out5 <- out5 / f4r[ngr]
  ngr <- get_ngramm(x, 3); out4 <- out4 / f3r[ngr]
  ngr <- get_ngramm(x, 2); out3 <- out3 / f2r[ngr]
  
  out <- c(out5, out4, out3)
  
  t <- sapply(names(out), get_last_word)
  names(out) <- t
  
  df <- data.frame(out,names(out))
  names(df ) <- c("c","last")
  out <- aggregate(c ~ last, data=df,FUN=sum)
  out <- out[order(-out$c),]
  rownames(out) <- NULL
  out$last <- as.character(out$last)
  
  out$c <- out$c/sum(out$c)
  head(out)
  head(jis)
  
  # Calculate P
  out$last <- as.character(out$last)
  for( i in 1:length(out$c) ) {
    out[i, "c"] <- out[i, "c"] + fg(out$last[i], jis)$c
  }
  
  t <- out[order(-out$c),]; rownames(t) <- NULL
  head(t,10)  
  
  return(t$last[1]) 
}

shinyServer(
  function(input, output) {
    output$out_sentence <- renderText({input$input_text})
    output$out_word_predicted <- renderText({paste("input text is:", input$input_text)})
  }
)

