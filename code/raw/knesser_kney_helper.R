library(quanteda)
library(parallel)

setwd("~/Coursera/DS_Capstone/")
source("code/raw/stats_helper.R")
load(file="data/f12345.Rda")

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
names(f24) <- c("c", "first","last");  rownames(f24) <- NULL # save our memory
f24$c <- as.numeric(as.character(f24$c))

#t <- sapply(strsplit(names(f5), ' '), function(x) paste(x[1],x[5],collapse = ' '))
#f24 <- f5; names(f24)<-t
#which(names(f24) %in% "case beer") # check the answer
t <- aggregate(c ~ first+last, data=f21,FUN=sum); f21 <- t[order(-t$c),]
t <- aggregate(c ~ first+last, data=f22,FUN=sum); f22 <- t[order(-t$c),]
t <- aggregate(c ~ first+last, data=f23,FUN=sum); f23 <- t[order(-t$c),]
t <- aggregate(c ~ first+last, data=f24,FUN=sum); f24 <- t[order(-t$c),]

save(f21,f22,f23,f24, file="data/f21_22_23_24.Rda")
#load("data/f21_22_23_24.Rda")

q1 <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
my.predict <- function(q) {
  t <- tokis(q)
  len = length(t)
  Wi <- t[len]
  Wi1 <- t[len-1]
  Wi2 <- t[len-2]
  Wi3 <- t[len-3]
  Wi4 <- t[len-4]

    
  paste(Wi4,Wi3,Wi2,Wi1,Wi, collapse = " ")
  ai <- f22[f22$first==Wi,]
  ai1 <- f22[f22$first==Wi1,]
  ai2 <- f22[f22$first==Wi2,]
  ai3 <- f22[f22$first==Wi3,]
  ai4 <- f22[f22$first==Wi4,]
  
  i1 <- intersect(ai$last,ai1$last)
  i2 <- intersect(ai$last,ai2$last)
  i3 <- intersect(ai$last,ai3$last)
  i4 <- intersect(ai$last,ai4$last)
  
  i11 <- intersect(i1,i2)
  i12 <- intersect(i1,i3)
  i13 <- intersect(i1,i4)
  
  intersect(i1,i2)
  
  #r3 <- f23[f23$first==Wi,]
  #r4 <- f24[f24$first==Wi,]
  #rw1 <- intersect(r2$last,r3$last)
  #rw1 <- intersect(rw1,r4$last)
  
  b2 <- f22[f22$first==Wi1,]
  #r3 <- f23[f23$first==Wi1,]
  #r4 <- f24[f24$first==Wi1,]
  #rw2 <- intersect(r2$last,r3$last); rw2 <- intersect(rw2,r4$last)
  
  a2<-r2[r2$last %in% rw1,]
  a3<-r3[r3$last %in% rw1,]
  a4<-r4[r4$last %in% rw1,]
  a2 <- rbind(a,b,c)
  tmp<-aggregate(c ~ last, data=a2,FUN=sum)
    
  b2 <- f22[f22$first==Wi2,]
  #b3 <- f23[f23$first==Wi2,]
  #b4 <- f24[f24$first==Wi2,]
  #rw3 <- intersect(r2$last,r3$last)
  #rw3 <- intersect(rw3,r4$last)
  
}

q1<-"The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
# Calculate M1 of Knesser-Ney smoothing
W<-f21[f21$first=="case",]
rownames(W)<-NULL
N <- sum(W$c)
x <- W$c/N
M1 <- cbind(W,x)

# find W
W<-f22[f22$first=="case",]
W<-aggregate(c ~ last,data=W,FUN=sum)
W[W$last=="beer",]
W<-W[order(-W$c),]
length(W$last)

# find normalization value
a <- aggregate(c ~ last,data=f22,FUN=sum)
for (i in 1:length(W$last)) {
  # x[i] <- sum(f22[f22$last==W$last[i],]$c)
  x[i] <- W$c[i]/sum(a[a$last==W$last[i],]$c)
}
M2 <- cbind(W,x)


































ptm <- proc.time()
Cc <- f22[f22$first=="nice",]
proc.time()-ptm

