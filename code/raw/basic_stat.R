# x = filename
getBasicStats <- function(x) {
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
