library(tm)
library(RWeka)
options(java.parameters = "-Xmx24g")

options(warn=-1)
profanityWords <- read.csv(url("http://www.bannedwordlist.com/lists/swearWords.csv"))

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

library(tm)
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
  txt <- VectorSource(raw)
  rm(raw)
  txt.corpus <- Corpus(txt)
  rm(txt)
  
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

txt.en.blog <- getCorpus(blog.en, 10000, profanityWords.en)
txt.en.news <- getCorpus(news.en, 10000, profanityWords.en)
txt.en.twit <- getCorpus(twitter.en, 10000, profanityWords.en)
txt.ru.blog <- getCorpus(blog.ru, 10000, profanityWords.ru)
txt.ru.news <- getCorpus(news.ru, 10000, profanityWords.ru)
txt.ru.twit <- getCorpus(twitter.ru, 10000, profanityWords.ru)

txt.en <- c(txt.en.blog, txt.en.news, txt.en.twit)
txt.ru <- c(txt.ru.blog, txt.ru.news, txt.ru.twit)

# free memory
rm(txt.en.blog, txt.en.news, txt.en.twit, txt.ru.blog, txt.ru.news, txt.ru.twit)

library(slam)
getFrequency <- function(x) {
  tdm <- TermDocumentMatrix(x)
  tdm.999 <- removeSparseTerms(tdm, sparse = 0.999)
  rm(tdm)
  
  freq <- sort(row_sums(tdm.999), decreasing = TRUE)
  return(freq)
}

calcCoverage <- function(x) {
  df <- data.frame(matrix(nrow=length(x),ncol=2))
  names(df) <- c("index", "value")
  df$index <- c(1:length(x))
  df$value <- cumsum(x)*100/sum(x)
  
  return(df)
}

freq.en <- getFrequency(txt.en)
freq.ru <- getFrequency(txt.ru)
par(mfrow=c(1,2))
hist(freq.en, breaks=1000, main="English words frequency\ndistribution", xlab="Words index", border="blue")
hist(freq.ru, breaks=1000, main="Russian words frequency\ndistribution", xlab="Words index", border="red")

txt.ru <- tm_map(txt.ru, removeWords, "это")
txt.en <- tm_map(txt.en, removeWords,  c("don", "didn", "can", "doesn", "isn", "wasn", "t", "dont", "u", "s", "c", "a", "p", "m"))

freq.ru <- getFrequency(txt.ru)

ggplot(test_data, aes(date)) + 
  geom_line(aes(y = var0, colour = "var0")) + 
  geom_line(aes(y = var1, colour = "var1"))







# Make wordcloud
wordcloud(names(freq.en.twit), freqBlog, scale=c(5,0.5), max.words=30, 
          random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, 
          colors=brewer.pal(8, "Set1"))

wordcloud(names(freq.ru.twit), freq.en.blog, scale=c(5,0.1), max.words=300, random.order=FALSE, 
          rot.per=0.5, use.r.layout=FALSE, colors=brewer.pal(8,"Dark2"))


# TODO: check this http://hack-r.com/n-gram-wordclouds-in-r/
