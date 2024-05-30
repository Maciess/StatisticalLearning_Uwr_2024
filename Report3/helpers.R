compute_js_estimator <- function(u_mle, sigma_square)
{
  # m is dimension of desired distribution
  m <- length(u_mle)
  c_js <- 1 - ((m - 2) * sigma_square)  /  sum(u_mle^2)
  return(c_js * u_mle)
}


compute_js_estimator_common_mu <- function(u_mle, sigma_square)
{
  m <- length(u_mle)
  d_js <- (m-3)/(m-1) * sigma_square / var(u_mle)
  return((1 - d_js )* u_mle + d_js * mean(u_mle))
}

get_FDP_and_TPP <- function(vars, beta_true)
{
  FP=sum(vars & (beta_true == 0))
  FN=sum(!vars & (beta_true > 0))
  TP=sum(vars & (beta_true > 0))
  
  TrueCount=sum(beta_true > 0)
  Reject=sum(vars != 0)
 
  FDP <- FP/(max(Reject, 1))
  TPP <- TP/ sum(beta_true > 0)
  returnValue(c(FDP = FDP, TPP=TPP))
}
