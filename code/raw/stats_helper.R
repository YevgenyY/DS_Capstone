library(quanteda)

setwd("~/Coursera/DS_Capstone/")

profanityWords.en <- names(read.csv(url("http://www.bannedwordlist.com/lists/swearWords.csv")))
stop_words<- c(stopwords("english"), 
                 "a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","q","p","r","s","t","u","v","w","x","y","z", "id","ill", profanityWords.en)
rm(profanityWords.en)

# return word #n-1
wnm1 <- function(x) { return(strsplit(x, " ")[[1]][1]) }

ngram_mask <- function(x, ngram) {
  pattern <- paste("^",ngram, sep = '')
  return(subset(x, grepl(paste(pattern), names(x))))
}

ngram_mask_last <- function(x, ngram) {
  pattern <- paste(ngram, '$', sep = '')
  return(subset(x, grepl(paste(pattern), names(x))))
}

# TOKenize Input
toki <- function(raw) {
  raw <- gsub("[^[:alnum:][:space:]']", ' ', raw)
  raw <- gsub('[[:digit:]]+', ' ', raw)
  raw <- gsub('[[:punct:]]+', '', raw)
  raw <- tolower(raw)
  
  tokens <- tokenize(raw, simplify=FALSE)
  tokens <- removeFeatures(tokens, c(stopwords("english"), "will", "ass", stop_words))
  
  return(tokens)  
}

tokis <- function(raw) {
  raw <- gsub("[^[:alnum:][:space:]']", ' ', raw)
  raw <- gsub('[[:digit:]]+', ' ', raw)
  raw <- gsub('[[:punct:]]+', '', raw)
  raw <- gsub('[\n]+', '', raw)
  raw <- tolower(raw)
  
  '%nin%' <- Negate('%in%')
  out <- lapply(raw, function(x) {
    t <- unlist(strsplit(x, " "))
    t[t %nin% stop_words]
  })
  out <- unlist(out) 
  return(out[lapply(out,nchar)>0])
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

except_last_word <- function(x) { 
  stopifnot(is.character (x))
  stopifnot(length(x) == 1)
  t <- unlist(strsplit(x, split=" "))
  z <- t[1:length(t)-1]
  return(paste(z, collapse = " "))
}

prepare_bi <- function(x) {
  t <- toki(x)
  len <- length(t[[1]])
  mtx <- data.frame(matrix(0, nrow = len, ncol = len))
  names(mtx) <- paste(t[[1]]); rownames(mtx) <- paste(t[[1]])
  for (i in 1:len) {
    for (j in 1:len) {
      w1 <- t[[1]][i]
      w2 <- t[[1]][j]
      mtx[i,j] <- log(f2[paste(w1, w2, sep=" ")] / f1[w1])
    }
  }
  mtx[is.na(mtx)] <- 0
}



