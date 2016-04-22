setwd("~/Coursera/DS_Capstone/")
source("code/final/0_stats_helper.R")
#load(file="data/f12345.Rda")

###### Prepare new bigrams for prediction
# f21 - (wi-1,wi) = f2
# f22 - (wi-2,wi), i.e. "cant wait see" -> "cant see"
# f23 - (wi-3,wi), i.e. "happy st patrick day" -> "happy day"
# f24 - (wi-4,wi), i.e. "services llc amazon eu associates" -> "services associates"
# 
# test implementation
#t <- sapply(strsplit(names(f4), ' '), function(x) paste(x[1],x[4],collapse = ' '))
#f23 <- f4; names(f23)<-t

# calculate f21
first <- sapply(strsplit(names(f2), ' '), function(x) return(x[1]))
last <- sapply(strsplit(names(f2), ' '), function(x) return(x[2]))
f21 <- data.frame(cbind(f2,first,last))
names(f21) <- c("c", "first","last");  rownames(f21) <- NULL # save our memory
f21$c <- as.numeric(as.character(f21$c))

# calculate f22
first <- sapply(strsplit(names(f3), ' '), function(x) return(x[1]))
last <- sapply(strsplit(names(f3), ' '), function(x) return(x[3]))
f22 <- data.frame(cbind(f3,first,last))
names(f22) <- c("c", "first","last");  rownames(f22) <- NULL # save our memory
f22$c <- as.numeric(as.character(f22$c))

# calculate f23
first <- sapply(strsplit(names(f4), ' '), function(x) return(x[1]))
last <- sapply(strsplit(names(f4), ' '), function(x) return(x[4]))
f23 <- data.frame(cbind(f4,first,last))
names(f23) <- c("c", "first","last");  rownames(f23) <- NULL # save our memory
f23$c <- as.numeric(as.character(f23$c))

# calculate f24
first <- sapply(strsplit(names(f5), ' '), function(x) return(x[1]))
last <- sapply(strsplit(names(f5), ' '), function(x) return(x[5]))
f24 <- data.frame(cbind(f5,first,last))
names(f24) <- c("c", "first","last");  rownames(f24) <- NULL # save memory
f24$c <- as.numeric(as.character(f24$c))

t <- aggregate(c ~ first+last, data=f21,FUN=sum); f21 <- t[order(-t$c),]
t <- aggregate(c ~ first+last, data=f22,FUN=sum); f22 <- t[order(-t$c),]
t <- aggregate(c ~ first+last, data=f23,FUN=sum); f23 <- t[order(-t$c),]
t <- aggregate(c ~ first+last, data=f24,FUN=sum); f24 <- t[order(-t$c),]

save(f21,f22,f23,f24, file="data/f21_22_23_24.Rda")
save(f1,f2,f1r,f2r,f3r,f4r,f5r,f21,f22,f23,f24,file="data/shiny_data.Rda")

getsize <- function(x) {
  object.size(x)/1024/1024
}

all_size <- getsize(f1) + getsize(f2) + getsize(f1r) + getsize(f2r) + getsize(f3r) + getsize(f21) + getsize(f22) + getsize(f23)
all_size



