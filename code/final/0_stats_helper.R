library(quanteda)
options(warn=-1)

setwd("~/Coursera/DS_Capstone/")

profanityWords.en <- names(read.csv(url("http://www.bannedwordlist.com/lists/swearWords.csv")))
stop_words<- c(stopwords("english"), 
                 "a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","q","p","r","s","t","u","v","w","x","y","z", "id","ill", profanityWords.en)
rm(profanityWords.en)

# x = filename
getBasicStats <- function(x) {
  bytesMB <- 1024 * 1024
  conn <- file(x,open="r")
  raw <- readLines(conn)
  close(conn)
  
  res <- file.info(x)$size / bytesMB
  res <- c(res, length(raw))
  res <- c(res, sum(sapply(gregexpr("\\W+", raw), length)))
  
  names(res) <- c("file size", "number of lines", "number of words")
  
  return(res)
}

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


except_last_word <- function(x) { 
  stopifnot(is.character (x))
  stopifnot(length(x) == 1)
  t <- unlist(strsplit(x, split=" "))
  z <- t[1:length(t)-1]
  return(paste(z, collapse = " "))
}

