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
profanityWords.ru <- names(read.csv("data/profanity_russian.txt"))

alphabet.en <- c(stopwords("english"), 
                 "a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","q","p","r","s","t","u","v","w","x","y","z")

alphabet.ru <- c(stopwords("russian"),
                 "а","б","в","г","д","е","ё","ж","з","и","к","л","м","н",
                 "о","п","р","с","т","у","ф","х","ц","ч","ш","щ","ъ","ы","ь","э","ю","я")

txt.blog <- getCorpus(blog.en, 100000, profanityWords.en)
txt.news <- getCorpus(news.en, 100000, profanityWords.en)
txt.twit <- getCorpus(twitter.en, 100000, profanityWords.en)
txt.en <- c(txt.blog, txt.news, txt.twit)

# Save russian and english corpuses for future using
save(txt.en, file="data/txt.en.Rda")
save(txt.ru, file="data/txt.ru.Rda")

txt <- txt.en

tokens <- tokenize(txt, simplify=FALSE)
tokens <- removeFeatures(tokens, c(stopwords("english"), "will", "ass", alphabet.en, profanityWords.en))
unigrams <- ngrams(tokens, n=1, skip=0, concatenator = " ")
bigrams <- ngrams(tokens, n=2, skip=0, concatenator = " ")
trigrams <- ngrams(tokens, n=3, skip=0, concatenator = " ")
quadgrams <- ngrams(tokens, n=4, skip=0, concatenator = " ")
pentagrams <- ngrams(tokens, n=5, skip=0, concatenator = " ")

dfm_one <- dfm(tokens, groups = NULL)
dfm_two <- dfm(bigrams, groups = NULL)
dfm_tri <- dfm(trigrams, groups = NULL)
dfm_quad <- dfm(quadgrams, groups = NULL)
dfm_penta <- dfm(pentagrams, groups = NULL)
save(tokens, unigrams, bigrams, trigrams, quadgrams, pentagrams,
     dfm_one,dfm_two,dfm_tri,dfm_quad,dfm_penta, file="data/tokens_dfm.Rda")

freq_one <- sort(colSums(dfm_one), decreasing = TRUE)
freq_two <- sort(colSums(dfm_two), decreasing = TRUE)
freq_tri <- sort(colSums(dfm_tri), decreasing = TRUE)
freq_quad  <- sort(colSums(dfm_quad),  decreasing = TRUE)
freq_penta <- sort(colSums(dfm_penta), decreasing = TRUE)

save(freq_one, freq_two, freq_tri, freq_quad, freq_penta,
     tokens, unigrams, bigrams, trigrams, quadgrams, pentagrams,
     dfm_one,dfm_two,dfm_tri,dfm_quad,dfm_penta, file="data/freq_tokens_dfm.Rda")

# wordcloud it
library(wordcloud)

coverage50<-500
wordcloud(names(freq), freq, scale=c(5,0.5), max.words=coverage50, 
          random.order=FALSE, rot.per=0.5, use.r.layout=FALSE, 
          colors=brewer.pal(8,"Dark2"))





