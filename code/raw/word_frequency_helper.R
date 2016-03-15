library(tm)

getTDM <- function(x,y) {
  # Load file line by line
  size <- y
  conn <- file(x,open="r")
  raw <- readLines(conn, n=size)
  close(conn)
  
  tmp <- raw[1:size]
  rm(raw)
  tmp <- tmp[grep("[a-zA-Z0-9 ]+", tmp)]
  
  # make a corpus
  txt <- VectorSource(tmp)
  rm(tmp)
  txt.corpus <- Corpus(txt)
  
  # Clean the corpus
  txt.corpus <- tm_map(txt.corpus, content_transformer(tolower))
  txt.corpus <- tm_map(txt.corpus, removePunctuation)
  txt.corpus <- tm_map(txt.corpus, removeNumbers)
  txt.corpus <- tm_map(txt.corpus, removeWords, stopwords("english"))
  txt.corpus <- tm_map(txt.corpus, stripWhitespace)
  tdm <- TermDocumentMatrix(txt.corpus)

  return(tdm)
} 

tdm.blog <- getTDM(blog.ru, 2000)
tmp <- apply(tdm.blog,1,sum)
freqBlog <- sort(tmp, decreasing = TRUE)

tdm.news <- getTDM(news.ru, 2000)
tmp <- apply(tdm.news,1,sum)
freqNews <- sort(tmp, decreasing = TRUE)

tdm.twit <- getTDM(twitter.ru, 2000)
tmp <- apply(tdm.twit,1,sum)
freqTwit <- sort(tmp, decreasing = TRUE)

# TODO: check this http://hack-r.com/n-gram-wordclouds-in-r/
