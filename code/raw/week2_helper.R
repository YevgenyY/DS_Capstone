library(tm)
setwd("~/Coursera/DS_Capstone")

dirEN <- "data/final/en_US/small"
blog.en="data/final/en_US/en_US.blogs.txt"
news.en="data/final/en_US/en_US.news.txt"
twitter.en="data/final/en_US/en_US.twitter.txt"
blog.ru="data/final/ru_RU/ru_RU.blogs.txt"
news.ru="data/final/ru_RU/ru_RU.news.txt"
twitter.ru="data/final/ru_RU/ru_RU.twitter.txt"

my.inspect <- function(x) {
  out <- c(x[[1]][[1]])
  for (i in 2:length(x)) {
    out <- c(out, x[[i]][[1]])
  }
  out
}

# Load file line by line
size <- 10
conn <- file(blog.en,open="r")
raw <- readLines(conn, n=size)
close(conn)

tmp <- raw[1:size]
tmp <- tmp[grep("[a-zA-Z0-9 ]+", tmp)]
tmp <- iconv(tmp,to="latin1")

txt <- VectorSource(tmp)
txt.corpus <- Corpus(txt)
my.inspect(txt.corpus)

# Load file using tm
#blog <- system.file("data/final/en_US/en_US.blogs.txt", package = "tm")
#txt.corpus <- Corpus(DirSource(dirEN), readerControl = list(reader=readPlain, language="la", load=TRUE))

txt.corpus <- tm_map(txt.corpus, content_transformer(tolower))
txt.corpus <- tm_map(txt.corpus, removePunctuation)
txt.corpus <- tm_map(txt.corpus, removeNumbers)
txt.corpus <- tm_map(txt.corpus, removeWords, stopwords("english"))

# Stem documents
#library(SnowballC)
#txt.corpus <- tm_map(txt.corpus, stemDocument)
#detach(package:SnowballC)
#inspect(txt.corpus)

txt.corpus <- tm_map(txt.corpus, stripWhitespace)
my.inspect(txt.corpus)

# Analyze the text
tdm <- TermDocumentMatrix(txt.corpus)
dtm <- DocumentTermMatrix(txt.corpus)
inspect(tdm)

# Clear tdm
#rowTotals <- apply(tdm , 1, sum) #Find the sum of words in each Document
#tdm   <- tdm[rowTotals > 0, ]           #remove all docs without words

# Find associated words
findAssocs(x=tdm, term="afford", corlimit=0.6)

# remove sparse terms (which occur very infrequently)
dtm.common.60 <- removeSparseTerms(x=dtm, sparse = 0.6)
dtm.common.20 <- removeSparseTerms(x=dtm, sparse = 0.2)

findFreqTerms(x=tdm, lowfreq = 50, highfreq = Inf)

######### N-Gramm #########################
# library("RWeka")
library(SnowballC)
# BigramTokenizer ####
# BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
# BigramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2))}
BigramTokenizer <- function(x, n=2) return(rownames(as.data.frame(unclass(textcnt(x,method="string",n=n)))))
tokenize_ngrams <- function(x, n=3) return(rownames(as.data.frame(unclass(textcnt(x,method="string",n=n)))))

tdm.2gram <- TermDocumentMatrix(txt.corpus, control = list(tokenize = tokenize_ngrams))
tdm.2gram <- TermDocumentMatrix(txt.corpus, control = list(tokenize = BigramTokenizer))

