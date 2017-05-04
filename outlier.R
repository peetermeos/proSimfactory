# Outlier detector code
library(MASS) #for generalised matrix inverses 

OutlierDetector <- function(x, alpha=0.05){
  # Assume that x is data frame

  # Find numeric columns
  num.vec <- sapply(x, class) %in% c("integer", "numeric", "double")
  
  # Calculate means and covariances
  m <- colMeans(x[, num.vec])
  s <- cov(x[, num.vec])
  
  # Calculate Mahalanobis distance
  # To avoid sigularities, invert the covariance matrix ourselves
  d2 <- mahalanobis(x[, num.vec], m, ginv(s), inverted = TRUE)  
  
  # Calculate chi square critical values
  chi.crit <- qchisq(1-alpha, df=length(num.vec))
   
  return(d2 > chi.crit)
}