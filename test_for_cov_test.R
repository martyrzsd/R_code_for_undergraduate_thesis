# call function
source("one_sample_covariance_testing.R")
# Benchmark Function
{
BenchMarkCov <-
  function(testTimes = 100,
           dimensionVector = c(2, 10, 50, 100, 200, 500),
           RSD = 0.05, Sigma=diag(x = 1, nrow = dimensionVector[1])) {
    library(MASS)
    # one can change test times and dimension of each experiments using the parameters above
    # initialize repeated experiments
    LRresults <- c()
    CLRresults <- c()
    AlterLRresults <- c()
    AlterCLRresults <- c()
    # initialize effects of dimensions
    LRresultsPara <- c()
    CLRresultsPara <- c()
    AlterLRresultsPara <- c()
    AlterCLRresultsPara <- c()
    for (p in dimensionVector) {
      # p: dimension
      n <- ceiling(p / RSD) # compute the rounded sample size
      
      # Run Test
      for (k in 1:testTimes) {
        # generating data for null and alter
        Sigma <- diag(x = 1, nrow = p)
        Test_data <-
          mvrnorm(n, rep(0, times = p), Sigma) 
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
        print(k)
        print(p)
        #SigmaPara <- diag(x = 1, nrow = p)
        ### Lower diagonal
        #for (i in 1:((ncol(SigmaPara)) - 2)) {
        #  diag(SigmaPara[-1:-i, (-ncol(SigmaPara) + i - 1):-ncol(SigmaPara)]) <-
        #    rep(rho ^ (i), times = ncol(SigmaPara) - i)
        #}
        ### Corner element
        #SigmaPara[ncol(SigmaPara), 1] <- rho ^ (ncol(SigmaPara))
        ### Upper diagonal
        #SigmaPara <-
        #  SigmaPara + t(SigmaPara) - diag(diag(SigmaPara))
        
        #Test_data_Para <-
        #  mvrnorm(n, rep(0, times = p), SigmaPara) # Generating data
        #LRresultsPara[length(LRresultsPara) + 1] <-
        #  one_sample_cov_test(Test_data_Para, 0.05, "none") / testTimes
        #CLRresultsPara[length(CLRresultsPara) + 1] <-
        #  one_sample_cov_test(Test_data_Para, 0.05, "RMT") / testTimes
      }
    }
    LRresultsPara <- matrix(LRresults, ncol = testTimes, byrow = TRUE)
    CLRresultsPara <- matrix(CLRresults, ncol = testTimes, byrow = TRUE)
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

SampleSize <- 1000
DimensionArray <- c(2, 10, 50, 100, 200, 500)
RsdArray <- DimensionArray/SampleSize
tt <- 10

for (i in 1:length(DimensionArray)) {
  ResultsDimensionEffects <- BenchMarkCov(testTimes = tt, RSD = RsdArray[i], dimensionVector = DimensionArray[i])
}
  
## dimension and sample size both growth ----
tt <- 10
ResultsBothGrowth <- BenchMarkCov(testTimes = tt)

write.csv(ResultsBothGrowth,file = "Covariance testing both growth.csv")
png(filename = "Covariance testing both growth.png",width = 1000,
    height = 618)
plot(
  x = DimensionArray,
  y = ResultsBothGrowth$`LR Dimension Effects`,
  "o",
  xlab = "Dimension",
  ylab = "Power",
  pch = 2,
  col = "red"
)
lines(x = DimensionArray,
      y = ResultsBothGrowth$`CLR Dimension Effects`,
      "o",
      col = "blue")
legend("left",
       legend = c("LR", "CLR"),
       pch = c(2, 1),
       col = c("red", "blue"))
}
dev.off()

FinalresultsPara <- BenchMarkCov(testTimes = 2000, Sigma = )
