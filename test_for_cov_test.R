# call function
source("one_sample_covariance_testing.R")
# Benchmark Function

BenchMarkCov <-
  function(testTimes = 100,
           dimensionVector = c(2, 10, 50, 100, 200, 500),
           RSD = 0.05) {
    library(MASS)
    # one can change test times and dimension of each experiments using the parameters above
    LRresults <- c()
    CLRresults <- c()
    AlterLRresults <- c()
    AlterCLRresults <- c()
    for (p in dimensionVector) {
      # p: dimension
      n <- ceiling(p / RSD) # compute the rounded sample size
      
      # Run Test
      for (k in 1:testTimes) {
        # k: number of experiments
        Sigma <- diag(x = 1, nrow = p)
        Test_data <-
          mvrnorm(n, rep(0, times = p), Sigma) # Generating data
        LRresults[length(LRresults) + 1] <-
          one_sample_cov_test(Test_data, 0.05, "none") / testTimes
        CLRresults[length(CLRresults) + 1] <-
          one_sample_cov_test(Test_data, 0.05, "RMT") / testTimes
        AlterSigma <-
          diag(c(3, rep(1, times = p - 1)), nrow = p) # Alternative hypothesis
        AlterTest_data <-
          mvrnorm(n, rep(0, times = p), Sigma) # Generating data under alternative
        AlterLRresults[length(LRresults) + 1] <-
          one_sample_cov_test(Test_data, 0.05, "none") / testTimes
        AlterCLRresults[length(CLRresults) + 1] <-
          one_sample_cov_test(Test_data, 0.05, "RMT") / testTimes
      }
    }
    LRresults <- matrix(LRresults, ncol = testTimes, byrow = TRUE)
    CLRresults <- matrix(CLRresults, ncol = testTimes, byrow = TRUE)
    AlterLRresults <-
      matrix(LRresults, ncol = testTimes, byrow = TRUE)
    AlterCLRresults <-
      matrix(CLRresults, ncol = testTimes, byrow = TRUE)
    return(
      list(
        "LR Dimension Effects" = apply(LRresults, 1, sum),
        "LR power" = 1 - apply(AlterLRresults, 1, sum),
        "LR Raw Results" = LRresults,
        "CLR Dimension Effects" = apply(CLRresults, 1, sum),
        "CLR power" = 1 - apply(AlterCLRresults, 1, sum),
        "CLR Raw Results" = CLRresults
      )
    )
  }



# Benchmark and visualization

Finalresults <- BenchMarkCov(testTimes = 100)


plot(
  x = c(2, 10, 50, 100, 200, 500),
  y = Finalresults$`LR Dimension Effects`,
  "o",
  xlab = "Dimension",
  ylab = "Power",
  pch = 2
)
lines(x = c(2, 10, 50, 100, 200, 500),
      y = Finalresults$`CLR Dimension Effects`,
      "o")
legend("topleft",
       legend = c("LR", "CLR"),
       pch = c(2, 1))
