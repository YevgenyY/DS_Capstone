library(quanteda)
options(java.parameters = "-Xmx24g")

#options(warn=-1)
getCorpus <- function(filename, sampleSize, profanityWords) {
  conn <- file(filename,open="r")
  
  lines <- iconv(readLines(conn), to = "utf-8")
  if (sampleSize != 0) {
    # Sample data with uniform distribution
    rowNums <- round(runif(sampleSize, min=1, max=length(lines)),0)
    raw <- c(lines[1])
    for(i in rowNums) {
      raw <- c(raw, lines[i])
    }
    rm(lines)
  } else 
    raw <- lines
  
  close(conn)
  
  # remove punctuation, numbers and tolower the content
  raw <- gsub("[^[:alnum:][:space:]']", ' ', raw)
  raw <- gsub('[[:digit:]]+', ' ', raw)
  raw <- gsub('[[:punct:]]+', '', raw)
  raw <- tolower(raw)
  
  # make a corpus
  
  
  
  # Clean the corpus
  txt.corpus <- tm_map(txt.corpus, removeWords, stopwords("english"))
  txt.corpus <- tm_map(txt.corpus, removeWords, stopwords("russian"))
  
  txt.corpus <- tm_map(txt.corpus, removeWords, profanityWords)
  txt.corpus <- tm_map(txt.corpus, stripWhitespace)
  
  return(txt.corpus)
}

setwd("~/Coursera/DS_Capstone/")
blog.en="data/final/en_US/en_US.blogs.txt"
news.en="data/final/en_US/en_US.news.txt"
twitter.en="data/final/en_US/en_US.twitter.txt"
blog.ru="data/final/ru_RU/ru_RU.blogs.txt"
news.ru="data/final/ru_RU/ru_RU.news.txt"
twitter.ru="data/final/ru_RU/ru_RU.twitter.txt"

profanityWords.en <- names(read.csv(url("http://www.bannedwordlist.com/lists/swearWords.csv")))
profanityWords.ru <- names(read.csv("data/profanity_russian.txt"))
