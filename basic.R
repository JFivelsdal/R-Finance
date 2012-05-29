#cal_pv <- function ( discount_rate, t , amount ){
#	amount * discount_factor
	#amount * exp( -discount_rate * t)
#}


DiscountFactor <- function(discount_rate, t, continuous = T){
  if (continuous){
    return(exp(-discount_rate*t))
  }else{
    return((1 + discount_rate) ^ -t)
  }
}


CAPM <- function(a, b, rf, rm){
  return(a + b(rm - rf))
}

VasicekModel <- function(a, b, n, delta_t, sigma, r = 0.05, dm = 0, dd = 1){
  # b: "long term mean level". All future trajectories of  will evolve around a mean level b in the long run;
  # a: "speed of reversion".  characterizes the velocity at which such trajectories will regroup around  in time;
  # delta: "instantaneous volatility", measures instant by instant the amplitude of randomness entering the system. Higher  implies more randomness
  rlt <- r
  while ( n > 1 ){
    dr <- a * (b - r) * delta_t + sigma * rnorm(dd, dm)
    rlt <- append(rlt, r + dr)
    r <- dr + r
    n <- n - 1
  }
  return(rlt)
}
