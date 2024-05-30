generate_indicator_matrix <- function(i, j, rows = 2, cols = 6) {
  indicator_matrix <- matrix(0, nrow = rows, ncol = cols) 
  indicator_matrix[1, i] <- 1  
  indicator_matrix[2, j] <- 1  
  return(indicator_matrix)
}


get_bonferroni_confidence_intervals <- function(mu, sample_covariance, n, confidence_level = .05)
{
  
  bonferroni_alpha <- confidence_level / (2 * 1)
  margin <- qt(1-bonferroni_alpha, n-1)  * sqrt( diag(sample_covariance) / n)
  upper_bound <- mu + margin
  lower_bound <- mu - margin
  returnValue(list(lower = lower_bound, upper = upper_bound))
}
