# Bond related calculation




GetYieldToMaturity <- function (trade_price, time_to_maturity, current_coupon, eps = 0.00001 ){
  payment_t <- seq(time_to_maturity %% 0.5, time_to_maturity, by = 0.5)
  payment_list <- rep(current_coupon, length(payment_t) )
  payment_list[length(payment_list)] <- payment_list[length(payment_list)] + 100
  
  ror <- 0.05
  while (abs(trade_price - sum (payment_list * (1 + ror)^(-payment_t)) ) >= delta ){
    ror <- ror - (trade_price - sum(payment_list * (1 + ror)^(-payment_t)))/(sum(payment_list * payment_t * (1 + ror)^(-payment_t-1)))
  }
  return(ror)
}
