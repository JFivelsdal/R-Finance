# Parametric Approaches

ParametricVAR <- function(d, a=0.05, method=qnorm){
  m <- mean(d)
  std <- sd(d)
  return (-m + method(a) * std)
}

LognormalVAR <- function(p, r, a=0.05, method=qnorm){
  return (p * (1 - exp(mean(r) - sd(r) * method(a))))
}

# Non-Parametric Approaches

# Age- weighted Historic Approaches

AgeWeight <- function(l, n){
  rlt <- rep(NA, n)
  for (i in seq(1,n)){
    rlt[i] <- l ^ (i - 1) * (1 - l) / (1 - l ^ n)
  }
  return(rlt)
}

# Volatility- Weighted Historical Approaches

VolatilityWeight <- function(T, t, r){
  return((T / t) * r)
}

#Back Testing with Unconditional Method

LR <- function(p, n, T){
  return (-2 * log(((1 - p)^(T-n)) * p^n) + 2 * log((1 - n/T)^(T-n) * (n/T)^n ))
}

POTVAR <- function(u, b, s, n, N, a=0.05){
  return(u + (b/s) * ((n/N * (1 - a))^(-s) - 1))
}

POTES <- function(var, b, u, s){
  return((var + b - s * u)/(1 - s))
}

