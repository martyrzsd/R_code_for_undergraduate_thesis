# Packages included

library(MASS)

# Likelihood ratio test function

LR <- function(input, alpha) {
  Empirical_Sigma <- cov(input)
  L <-
    sum(diag(Empirical_Sigma)) - log(abs(det(Empirical_Sigma))) - ncol(input) #value of log likelihood
  likelihood_statistics <- nrow(input) * L # value of LR statistics
  threshold <-
    qchisq(1 - alpha, df = ncol(input) * (ncol(input) + 1) / 2) # compute reject boundary of region
  if (likelihood_statistics > threshold) {
    return(1) # Reject
  }
  return(0) # Accept
}

CLR <- function(input, alpha) {
  d <- ncol(input)
  yn <- d / nrow(input)
  Empirical_Sigma <- cov(input)
  L <-
    sum(diag(Empirical_Sigma)) - log(abs(det(Empirical_Sigma))) - ncol(input) #value of log likelihood
  CLRn <-
    (L - d * (1 - (1 - 1 / yn) * log(1 - yn)) - log(1 - yn) / 2) / (sqrt(-2 *
                                                                           log(1 - yn) - 2 * yn))# value of CLR statistics
  UpperThreshold <- qnorm(1 - alpha / 2, mean = 0, sd = 1)
  LowerThreshold <-
    qnorm(alpha / 2, mean = 0, sd = 1) # compute two-sided threshold
  if (CLRn > UpperThreshold || CLRn < LowerThreshold) {
    return(1) #Reject
  }
  return(0) # Accept
}

# Benchmark Function

BenchMarkCov <-
  function(testTimes = 100,
           dimensionVector = c(2, 10, 50, 100, 200, 500),
           RSD = 0.05) {
    # one can change test times and dimension of each experiments using the parameters above
    LRresults <- c()
    CLRresults <- c()
    AlterLRresults <- c()
    AlterCLRresults <- c()
    for (p in dimensionVector) {
      # p: dimension
      n <- ceiling(p / RSD) # compute the rounded sample size
      for (k in 1:testTimes) {
        # k: number of experiments
        Sigma <- diag(x = 1, nrow = p)
        Test_data <-
          mvrnorm(n, rep(0, times = p), Sigma) # Generating data
        LRresults[length(LRresults) + 1] <- LR(Test_data, 0.05) / testTimes
        CLRresults[length(CLRresults) + 1] <- CLR(Test_data, 0.05) / testTimes
        AlterSigma <- diag(c(3,rep(1,times = p-1)),nrow = p) # Alternative hypothesis
        AlterTest_data <-
          mvrnorm(n, rep(0, times = p), Sigma) # Generating data under alternative
        AlterLRresults[length(LRresults) + 1] <- LR(Test_data, 0.05) / testTimes
        AlterCLRresults[length(CLRresults) + 1] <- CLR(Test_data, 0.05) / testTimes
      }
    }
    LRresults <- matrix(LRresults, ncol = testTimes, byrow = TRUE)
    CLRresults <- matrix(CLRresults, ncol = testTimes, byrow = TRUE)
    AlterLRresults <- matrix(LRresults, ncol = testTimes, byrow = TRUE)
    AlterCLRresults <- matrix(CLRresults, ncol = testTimes, byrow = TRUE)
    return(
      list(
        "LR Dimension Effects" = apply(LRresults, 1, sum),
        "LR power" = 1-apply(AlterLRresults, 1, sum),
        "LR Raw Results" = LRresults,
        "CLR Dimension Effects" = apply(CLRresults, 1, sum),
        "CLR power" = 1-apply(AlterCLRresults, 1, sum),
        "CLR Raw Results" = CLRresults
      )
    )
  }

# Benchmark and visualization

Finalresults <- BenchMarkCov(testTimes = 100)


plot(x=c(2, 10, 50, 100, 200, 500),y=Finalresults$`LR Dimension Effects`,"o", xlab = "Dimension",ylab= "Power",pch=2)
lines(x=c(2, 10, 50, 100, 200, 500),y=Finalresults$`CLR Dimension Effects`,"o")
legend("topleft",legend=c("LR","CLR"),pch = c(2,1))



