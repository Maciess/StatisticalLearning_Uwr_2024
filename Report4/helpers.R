generate_random_orthogonal <- function(n, p)
{
  require(mvtnorm)
  cov_matrix <- matrix(0, nrow = p, ncol = p)
  diag(cov_matrix) <- 1/sqrt(n)
  X = rmvnorm(n = n, mean = rep(0, p), sigma = cov_matrix)
}

get_FDP_and_TPP <- function(beta_hat, beta_true)
{
  FP=sum(beta_hat & (beta_true == 0))
  FN=sum(!beta_hat & (beta_true > 0))
  TP=sum(beta_hat & (beta_true > 0))
  
  TrueCount=sum(beta_true > 0)
  Reject=sum(beta_hat != 0)
  
  FDP <- FP/(max(Reject, 1))
  TPP <- TP/ sum(beta_true > 0)
  returnValue(c(FDP = FDP, TPP=TPP))
}
