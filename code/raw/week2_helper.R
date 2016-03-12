library(tm)

file.blog="data/final/en_US/en_US.blogs.txt"
file.news="data/final/en_US/en_US.news.txt"
file.twitter="data/final/en_US/en_US.twitter.txt"

conn <- file(file.blog,open="r")
raw <- readLines(conn)
close(conn)

txt <- VectorSource(raw[1:100])
txt.corpus <- Corpus(txt)
inspect(txt.corpus)

txt.corpus <- tm_map(txt.corpus, content_transformer(tolower))
txt.corpus <- tm_map(txt.corpus, removePunctuation)
txt.corpus <- tm_map(txt.corpus, removeNumbers)
txt.corpus <- tm_map(txt.corpus, removeWords, stopwords("english"))

# Stem documents
library(SnowballC)
txt.corpus <- tm_map(txt.corpus, stemDocument)
detach(package:SnowballC)
inspect(txt.corpus)

txt.corpus <- tm_map(txt.corpus, stripWhitespace)
inspect(txt.corpus)

# Analyze the text
tdm <- TermDocumentMatrix(txt.corpus)
inspect(tdm)

# Find all words/terms which were used 8 or more times (in all docs)
findFreqTerms(x=tdm, lowfreq = 25, highfreq = Inf)



