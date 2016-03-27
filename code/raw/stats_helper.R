library(quanteda)

setwd("~/Coursera/DS_Capstone/")

alphabet.en <- c(stopwords("english"), 
                 "a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","q","p","r","s","t","u","v","w","x","y","z")
profanityWords.en <- names(read.csv(url("http://www.bannedwordlist.com/lists/swearWords.csv")))

# return word #n-1
wnm1 <- function(x) { return(strsplit(x, " ")[[1]][1]) }

mle_bigram <- function(x1,x2) {
  p <- freq_two[paste(x1,x2,sep=" ")] / freq_one[paste(x1)]
  
  return(p)
}

ngram_mask <- function(x, ngram) {
  pattern <- paste("^",ngram, sep = '')
  return(subset(x, grepl(paste(pattern), names(x))))
}

# TOKenize Input
toki <- function(raw) {
  raw <- gsub("[^[:alnum:][:space:]']", ' ', raw)
  raw <- gsub('[[:digit:]]+', ' ', raw)
  raw <- gsub('[[:punct:]]+', '', raw)
  raw <- tolower(raw)
  
  tokens <- tokenize(raw, simplify=FALSE)
  tokens <- removeFeatures(tokens, c(stopwords("english"), "will", "ass", alphabet.en, profanityWords.en))
  
  return(tokens)  
}

# Get last "len" words from "x"
rvsw <- function(x, len) {
  t<-toki(x)
  
  size <- length(t[[1]])
  if (size < len)
    return(NULL)
  res <- c()
  j <- size - len + 1
  for (i in j:size) {
    res <- c(res,t[[1]][i])
  }
  return(paste(res, collapse = " "))
}
fwdw <- function(x, len) {
  t<-toki(x)
  
  size <- length(t[[1]])
  if (size < len)
    return(NULL)
  res <- c()
  j <- len
  for (i in 1:j) {
    res <- c(res,t[[1]][i])
  }
  return(paste(res, collapse = " "))
}

# normalizing
# long algo
normalize_freq <- function(freq) {
  x<-freq
  for (i in 1:length(x)) {
    w1 <- fwdw(names(x[i]), 1)
    total <- sum(ngram_mask(x, w1)) 
    x[i] <- x[i] / total
  }
  return(x)  
}
# make an assumption that c(w1,*) =~ c(w1)
normalize_2gram <- function() {
  x<-f2
  for (i in 1:length(x)) {
    w1 <- fwdw(names(x[i]), 1)
    total <- f1[paste(w1)]
    x[i] <- x[i] / total
    
    if(i%%1000 == 0) print(i)
    flush.console()
  }
  return(x)  
}
normalize_3gram <- function() {
  x<-f3
  for (i in 1:length(x)) {
    w1 <- fwdw(names(x[i]), 2)
    total <- f2[paste(w1)]
    x[i] <- x[i] / total
    
    if(i%%1000 == 0) print(i)
    flush.console()
  }
  return(x)  
}



