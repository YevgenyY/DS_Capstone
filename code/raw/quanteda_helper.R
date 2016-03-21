library(quanteda)

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
  
  txt<-corpus(raw, verbose=TRUE, toLower=TRUE, removeNumbers=TRUE, removePunct=TRUE,
              removeSeparators=TRUE, removeTwitter=FALSE, stem=FALSE, keptFeatures=NULL, 
              thesaurus=NULL, dictionary=NULL, groups=NULL)
  
  # make a corpus
  #txt <- VectorSource(raw)
  #rm(raw)
  #txt.corpus <- Corpus(txt)
  #rm(txt)
  
  # Clean the corpus
  #txt.corpus <- tm_map(txt.corpus, removeWords, stopwords("english"))
  #txt.corpus <- tm_map(txt.corpus, removeWords, stopwords("russian"))
  
  #txt.corpus <- tm_map(txt.corpus, removeWords, profanityWords)
  #txt.corpus <- tm_map(txt.corpus, stripWhitespace)
  
  return(txt)
}

setwd("~/Coursera/DS_Capstone/")
blog.en="data/final/en_US/en_US.blogs.txt"
news.en="data/final/en_US/en_US.news.txt"
twitter.en="data/final/en_US/en_US.twitter.txt"
blog.ru="data/final/ru_RU/ru_RU.blogs.txt"
news.ru="data/final/ru_RU/ru_RU.news.txt"
twitter.ru="data/final/ru_RU/ru_RU.twitter.txt"

profanityWords.en <- names(read.csv(url("http://www.bannedwordlist.com/lists/swearWords.csv")))
alphabet <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","q","p","r","s","t","u","v","w","x","y","z")

txt <- getCorpus(blog.ru, 100000, profanityWords.en)
tokens <- tokenize(txt, simplify=FALSE)
tokens <- removeFeatures(tokens, c(stopwords("russian"), "will", "mr", alphabet))
bigrams <- ngrams(tokens, n=2, skip=0, concatenator = " ")
trigrams <- ngrams(tokens, n=3, skip=0, concatenator = " ")

mydfm <- dfm(tokens, groups = NULL)
freq <- sort(colSums(mydfm), decreasing = TRUE)

# wordcloud it
library(wordcloud)

coverage50<-500
wordcloud(names(freq), freq, scale=c(5,0.5), max.words=coverage50, 
          random.order=FALSE, rot.per=0.5, use.r.layout=FALSE, 
          colors=brewer.pal(8,"Dark2"))





