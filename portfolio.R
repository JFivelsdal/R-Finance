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
PortfolioVariance <- function(s, w, p){
  #p -->> correlation matrix
  #s -->> variance vector
  #w -->> weighted vector 
  # Attention: length(s) should be equals to length(w)
  n <- length(s)
  
  sec_term <- 0.0
  comb.matrix <- combn(seq(n),2, fun = NULL, simplify =T)
  for (v in seq(n/2) ) {
    a <- comb.matrix[1,v]
    b <- comb.matrix[2,v]
    sec_term <- w[a]*w[b]*s[a]*s[b]*p[a,b] + sec_term
  }
  
  return(sum(w^2 * v^2) + sec_term)
}

#Portfolio Expect Return
PortfolioExpRtn <- function(r, w){
  return(sum(w * r))
}


