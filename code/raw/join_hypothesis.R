# Try join hypothesis

setwd("~/Coursera/DS_Capstone/")
source("code/raw/stats_helper.R")
source("code/raw/questions.R")
load(file="data/f12345.Rda")
load(file="data/f21_22_23_24.Rda")

full_join <- function(x) {
  ai1 <- as.character(f21[f21$first==x,]$last); print(length(ai1))
  ai2 <- as.character(f22[f22$first==x,]$last); print(length(ai2))
  ai3 <- as.character(f23[f23$first==x,]$last); print(length(ai3))
  ai4 <- as.character(f24[f24$first==x,]$last); print(length(ai4))
  
  full_join <- unique(c(ai1,ai2,ai3,ai4))
  
  print(length(full_join))
  
  return(full_join)
}