# Likelihood ratio test function

one_sample_cov_test <- function(input, alpha, correction_method = "none") {
  library(MASS)
  Empirical_Sigma <- cov(input)
  L <-
    sum(diag(Empirical_Sigma)) - log(abs(det(Empirical_Sigma))) - ncol(input) #value of log likelihood
  likelihood_statistics <- nrow(input) * L # value of LR statistic
  if (correction_method == "none") {
    threshold <-
      qchisq(1 - alpha, df = ncol(input) * (ncol(input) + 1) / 2) # compute reject boundary of region
    if (likelihood_statistics > threshold) {
      return(1) # Reject
    }
    return(0) # Accept
  }
  if (correction_method == "RMT") {
    d <- ncol(input)
    yn <- d / nrow(input)
    CLRn <-
      (L - d * (1 - (1 - 1 / yn) * log(1 - yn)) - log(1 - yn) / 2) / (sqrt(-2 *
                                                                             log(1 - yn) - 2 * yn))# value of CLR statistics
    UpperThreshold <- qnorm(1 - alpha / 2)
    LowerThreshold <- qnorm(alpha / 2) # compute two-sided threshold
    if (CLRn > UpperThreshold || CLRn < LowerThreshold) {
      return(1) #Reject
    }
    return(0) # Accept
  }
  
}



