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
