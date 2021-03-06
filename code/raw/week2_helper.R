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
size <- 100000
conn <- file(blog.en,open="r")
#lines <- readLines(conn, n=size)
lines <- readLines(conn, encoding="UTF-8")
close(conn)

# Get random strings from dataset
rowNums <- round(runif(size, min=1, max=length(lines)),0)
raw <- c(lines[1])
for(i in rowNums) {
  raw <- c(raw, lines[i])
}

# remove punctuation, numbers and tolower the content
raw <- gsub("[^[:alnum:][:space:]']", ' ', raw)
raw <- gsub('[[:digit:]]+', ' ', raw)
raw <- tolower(raw)


raw <- iconv(raw, to='ASCII//TRANSLIT')
txt <- VectorSource(raw)
txt.corpus <- Corpus(txt, readerControl = list(language = "lat"))
rm(txt)

# Load file using tm
#blog <- system.file("data/final/en_US/en_US.blogs.txt", package = "tm")
#txt.corpus <- Corpus(DirSource(dirEN), readerControl = list(reader=readPlain, language="la", load=TRUE))

#txt.corpus <- tm_map(txt.corpus, content_transformer(tolower))
#txt.corpus <- tm_map(txt.corpus, removePunctuation)
#txt.corpus <- tm_map(txt.corpus, removeNumbers)
txt.corpus <- tm_map(txt.corpus, removeWords, stopwords("english"))

# Stem documents
#library(SnowballC)
#txt.corpus <- tm_map(txt.corpus, stemDocument)
#detach(package:SnowballC)
#inspect(txt.corpus)

txt.corpus <- tm_map(txt.corpus, stripWhitespace)

# Analyze the text
tdm <- TermDocumentMatrix(txt.corpus)
termsSorted <- sort(apply(tdm, 1, sum), decreasing=TRUE)
#dtm <- DocumentTermMatrix(txt.corpus)
#inspect(tdm)

# Remove sparse terms (which occur very infrequently)
tdm.999 <- removeSparseTerms(x=tdm, sparse = 0.999)
# Find associated words
findAssocs(x=tdm.999, term="abuse", corlimit=0.05)
termsSorted <- sort(apply(tdm.999,1,sum), decreasing=TRUE)


findFreqTerms(x=tdm.common.999, lowfreq = 5000, highfreq = Inf)

######### N-Gramm #########################
library("RWeka")
options(java.parameters = "- Xmx8192m")
delim <- " "
BiGram <- NGramTokenizer(raw, Weka_control(min=2,max=2, delimiters = delim))
TriGram <- NGramTokenizer(txt.corpus, Weka_control(min=3,max=3, delimiters = delim))

