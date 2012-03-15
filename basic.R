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




FindRoot <- function(expr, interval, b = 0.1 , eps = 0.0001){
  for (x in seq(from=interval[1], to=interval[2], by=b)){
    print(x)
    print (eval(expr))
	if (eval(expr) <= abs(eps)){
      return (x)	
	}
  }
  return(F)
}
