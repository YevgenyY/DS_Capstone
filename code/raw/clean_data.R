file1="data/final/en_US/en_US.blogs.txt"
file2="data/final/en_US/en_US.news.txt"
file3="data/final/en_US/en_US.twitter.txt"

conn <- file(file1,open="r")
lines <- readLines(conn)
close(conn)

raw <- iconv(lines, to='ASCII//TRANSLIT')

for(i in 1:length(lines)) {
  txt <- VectorSource(lines[i])
  txt.corpus <- Corpus(txt, readerControl = list(language = "lat"))  
  
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
  
  # Analyze the text
  tdm <- TermDocumentMatrix(txt.corpus)
}







