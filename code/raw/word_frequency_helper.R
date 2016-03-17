library(tm)
library(RWeka)
options(java.parameters = "-Xmx24g")

options(warn=-1)
profanityWords <- read.csv(url("http://www.bannedwordlist.com/lists/swearWords.csv"))

getBasicStat <- function(x) {
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

getTDM <- function(x,y,z, removeWords) {

  # Load file line by line
  sampleSize <- y
  conn <- file(x,open="r")
  
  if (sampleSize != 0) {
    lines <- readLines(conn, n=sampleSize)
    # Get random strings from dataset
    rowNums <- round(runif(sampleSize, min=1, max=length(lines)),0)
    raw <- c(lines[1])
    for(i in rowNums) {
      raw <- c(raw, lines[i])
    }
    rm(lines)
  } else
    raw <- readLines(conn)
  
  close(conn)

  # remove punctuation, numbers and tolower the content
  raw <- gsub("[^[:alnum:][:space:]']", ' ', raw)
  raw <- gsub('[[:digit:]]+', ' ', raw)
  raw <- tolower(raw)
  
  # make a corpus
  txt <- VectorSource(raw)
  rm(raw)
  txt.corpus <- Corpus(txt)
  rm(txt)
  
  # Clean the corpus
  txt.corpus <- tm_map(txt.corpus, removeWords, stopwords("english"))
  txt.corpus <- tm_map(txt.corpus, removeWords, stopwords("russian"))
  txt.corpus <- tm_map(txt.corpus, removeWords, removeWords)
  txt.corpus <- tm_map(txt.corpus, stripWhitespace)
  
  delim <- " "
  ngram_size <- z
  tdm <- NGramTokenizer(txt.corpus, Weka_control(min=ngram_size, max=ngram_size, delimiters = delim))
  #tdm <- TermDocumentMatrix(txt.corpus)

  # Remove sparse terms (which occur very infrequently)
  # tdm.999 <- removeSparseTerms(x=tdm, sparse = 0.999)
  return(tdm)
} 

tdm.en.blog <- getTDM(blog.en, 100000,1, removeWords)
tmp <- apply(tdm.en.blog,1,sum)
freqen.en.blog <- sort(tmp, decreasing = TRUE)

tdm.en.news <- getTDM(news.en, 100000)
tmp <- apply(tdm.en.news,1,sum)
freq.en.news <- sort(tmp, decreasing = TRUE)

tdm.en.twit <- getTDM(twitter.en, 100000)
tmp <- apply(tdm.en.twit,1,sum)
freq.en.twit <- sort(tmp, decreasing = TRUE)

tdm.ru.blog <- getTDM(blog.ru, 100000)
tmp <- apply(tdm.ru.blog,1,sum)
freq.ru.blog <- sort(tmp, decreasing = TRUE)

tdm.ru.news <- getTDM(news.ru, 100000)
tmp <- apply(tdm.ru.news,1,sum)
freq.ru.news <- sort(tmp, decreasing = TRUE)

tdm.ru.twit <- getTDM(twitter.ru, 100000)
tmp <- apply(tdm.ru.twit,1,sum)
freq.ru.twit <- sort(tmp, decreasing = TRUE)

# Make wordcloud
wordcloud(names(freq.en.twit), freqBlog, scale=c(5,0.5), max.words=30, 
          random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, 
          colors=brewer.pal(8, "Set1"))

wordcloud(names(freq.ru.twit), freq.en.blog, scale=c(5,0.1), max.words=300, random.order=FALSE, 
          rot.per=0.5, use.r.layout=FALSE, colors=brewer.pal(8,"Dark2"))


# TODO: check this http://hack-r.com/n-gram-wordclouds-in-r/
