PaymentPerMonth <- function(r, t, os){
  # r monthly interest.rate, t loan.term in months, os outstanding.balance
  outstanding.principal <- principal.payment <- interest.payment <- rep(NA, t)
  month.payment <- os * (r * (1 + r)^t) / ((1 + r)^t - 1)
  for ( i in seq(t)) {
    interest.payment[i] <- os * r
    principal.payment[i] <- month.payment - interest.payment[i]
    outstanding.principal[i] <- os <- os - principal.payment[i]
  }
  return (data.frame(principal.payment, interest.payment, outstanding.principal))
}

SingleMonthlyMortalityRate <- function(cpr=.002, psa= 1){
  # CPR ,
  return (1 - (1-cpr*psa)^(1/12))
}
