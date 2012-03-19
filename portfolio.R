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

#geometri average return
GeoAvgRetrun <- function(r){
  return((prod(1+r))^(1/length(r)) - 1)
}

#arithmetric average return
ArithAvgRetrun <- function(r){
 return(sum(r)/length(r))
}

#average log return
LogAvgRetrun <- function(r){
  return(sum(log(r + 1))/length(r))
}


#Portfolio Variance
PortfolioVariance <- function(v, w, s, r){
  #sum(w^2 * v^2) + w * s
}

#Portfolio Expect Return
PortfolioExpRtn <- function(r, w){
  return(sum(w * r))
}


