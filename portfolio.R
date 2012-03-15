#portfolio analysis

#Sharpo Ratio
SharpeRatio <- function(rp, rf){
  return((mean(rp) - mean(rf))/sd(rp))
}

#Information Ratio
InfoRatio <- function(rp, rb){
  return(mean(rp) - mean(rb))/sd(rp-rb))
}

#Jensen Alpha 
JensenAplha <- function(a, b, rp, rf, rm){
  return(rp - (a + b(rm - rf)))
}
