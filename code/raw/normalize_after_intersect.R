

jis <- try.intersect.join(q1,a1)

ai1 <- f21[f21$first==x,]
ai2 <- f22[f22$first==x,]
ai3 <- f23[f23$first==x,]
ai4 <- f24[f24$first==x,]

nmz <- c()
#for(i in 1:length(jis)) {
#  y <- jis[i]
#  nmz <- c(nmz,ai1[ai1$last==y,]$c / f1[y])
#}

nmz <- unlist(sapply(jis, FUN = function(x) {return (ai1[ai1$last==x,]$c / f1[x])}))
nmz <- nmz[sort(-nmz)]

for(i in 1:length(y)) {
  out <- paste(y[i], grep(y[i], names(nmz)), collapse = ":")
  print(out)
}

