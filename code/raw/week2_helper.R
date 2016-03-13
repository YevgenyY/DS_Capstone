library(tm)

dirEN <- "data/final/en_US/small"
blog.en="data/final/en_US/en_US.blogs.txt"
news.en="data/final/en_US/en_US.news.txt"
twitter.en="data/final/en_US/en_US.twitter.txt"
blog.ru="data/final/ru_RU/ru_RU.blogs.txt"
news.ru="data/final/ru_RU/ru_RU.news.txt"
twitter.ru="data/final/ru_RU/ru_RU.twitter.txt"

# Load file line by line
conn <- file(blog.en,open="r")
raw <- readLines(conn)
close(conn)

tmp <- raw[1:1000]
tmp <- tmp[grep("[a-zA-Z0-9 ]+", tmp)]

txt <- VectorSource(tmp)
txt.corpus <- Corpus(txt, readerControl = txt$DefaultReader, Encoding="latin1")
inspect(txt.corpus)

# Load file using tm
#blog <- system.file("data/final/en_US/en_US.blogs.txt", package = "tm")
#txt.corpus <- Corpus(DirSource(dirEN), readerControl = list(reader=readPlain, language="la", load=TRUE))

txt.corpus <- tm_map(txt.corpus, content_transformer(tolower))
txt.corpus <- tm_map(txt.corpus, removePunctuation)
txt.corpus <- tm_map(txt.corpus, removeNumbers)
txt.corpus <- tm_map(txt.corpus, removeWords, stopwords("english"))

# Stem documents
# library(SnowballC)
# txt.corpus <- tm_map(txt.corpus, stemDocument)
# detach(package:SnowballC)
# inspect(txt.corpus)

txt.corpus <- tm_map(txt.corpus, stripWhitespace)
inspect(txt.corpus)

# Analyze the text
tdm <- TermDocumentMatrix(txt.corpus)
inspect(tdm)

# Clear tdm
#rowTotals <- apply(tdm , 1, sum) #Find the sum of words in each Document
#tdm   <- tdm[rowTotals > 0, ]           #remove all docs without words

# Find associated words
findAssocs(x=tdm, term="afford", corlimit=0.6)

# remove sparse terms (which occur very infrequently)
tdm.common.60 <- removeSparseTerms(tdm, 0.6)
tdm.common.20 <- removeSparseTerms(x=tdm, sparse = 0.2)

findFreqTerms(x=tdm, lowfreq = 100, highfreq = Inf)



