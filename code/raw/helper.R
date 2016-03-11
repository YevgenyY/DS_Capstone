file1="data/final/en_US/en_US.blogs.txt"
file2="data/final/en_US/en_US.news.txt"
file3="data/final/en_US/en_US.twitter.txt"

maxLen <- 0

checkLinesLength <- function(x) {
  conn <- file(x,open="r")
  lines <- readLines(conn)
  for (i in 1:length(lines)){
    curLen <- nchar(lines[i])
    
    if(maxLen < curLen)
      maxLen <- curLen
    
  }
  close(conn)
  
  maxLen
}

# Q4
conn <- file(file3,open="r")
lines <- readLines(conn)
close(conn)

loveDF <- subset(lines, grepl('love',lines))
hateDF <- subset(lines, grepl('hate',lines))

length(loveDF)/length(hateDF)


#Q6
nSent <- 0
conn <- file(file3,open="r")
lines <- readLines(conn)
close(conn)

matchedDF <- subset(lines, grepl('A computer once beat me at chess, but it was no match for me at kickboxing',lines))
matchedDF
