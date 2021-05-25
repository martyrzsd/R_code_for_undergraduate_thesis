# call function
source("one_sample_covariance_testing.R")
# Benchmark Function
BenchMarkCov <-
  function(testTimes = 100,
           dimensionVector = c(2, 10, 50, 100, 200),
           RSD = 0.05,
           Sigma = diag(x = 1, nrow = dimensionVector[1])) {
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
          diag(c(1, rep(0.02, times = p - 1)), nrow = p) # Alternative hypothesis
        AlterTest_data <-
          mvrnorm(n, rep(0, times = p), AlterSigma) # Generating data under alternative
        AlterLRresults[length(AlterLRresults) + 1] <-
          one_sample_cov_test(AlterTest_data, 0.05, "none") / testTimes
        AlterCLRresults[length(AlterCLRresults) + 1] <-
          one_sample_cov_test(AlterTest_data, 0.05, "RMT") / testTimes
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
    LRresults <- matrix(LRresults, ncol = testTimes, byrow = TRUE)
    CLRresults <- matrix(CLRresults, ncol = testTimes, byrow = TRUE)
    AlterLRresults <-
      matrix(AlterLRresults, ncol = testTimes, byrow = TRUE)
    AlterCLRresults <-
      matrix(AlterCLRresults, ncol = testTimes, byrow = TRUE)
    return(
      list(
        "LR both growth" = apply(LRresults, 1, sum),
        "LR power" = apply(AlterLRresults, 1, sum),
        "LR Raw Results" = LRresults,
        "CLR both growth" = apply(CLRresults, 1, sum),
        "CLR power" = apply(AlterCLRresults, 1, sum),
        "CLR Raw Results" = CLRresults
      )
    )
  }


{
## Dimension effects ----
{
SampleSize <- 500
DimensionArray <- c(2, seq(10, 100, 10), seq(120, 300, 30))
RsdArray <- DimensionArray / SampleSize
tt <- 2000

ResultsDimensionEffectsLR <- c()
ResultsDimensionEffectsCLR <- c()
ResultsDimensionEffectsLRPower <- c()
ResultsDimensionEffectsCLRPower <- c()
for (i in 1:length(DimensionArray)) {
  temp <-
    BenchMarkCov(testTimes = tt,
                 RSD = RsdArray[i],
                 dimensionVector = DimensionArray[i])
  ResultsDimensionEffectsLR[i] <- temp$'LR both growth'
  ResultsDimensionEffectsCLR[i] <- temp$'CLR both growth'
  ResultsDimensionEffectsLRPower[i] <- temp$'LR power'
  ResultsDimensionEffectsCLRPower[i] <- temp$'CLR power'
}
ResultsDimensionEffects <-
  data.frame(
    LR = ResultsDimensionEffectsLR,
    CLR = ResultsDimensionEffectsCLR,
    LRPower = ResultsDimensionEffectsLRPower,
    CLRPower = ResultsDimensionEffectsCLRPower
  )

write.csv(ResultsDimensionEffects, file = "Covariance testing dimension effects.csv")
png(filename = "Covariance_testing_dimension_effects.png",
    width = 1000,
    height = 618)
plot(
  x = DimensionArray,
  y = ResultsDimensionEffectsLR,
  "o",
  xlab = "dimension p",
  ylab = "size",
  pch = 2,
  col = "red",
  ylim = c(0, 1)
)
lines(x = DimensionArray,
      y = ResultsDimensionEffectsCLR,
      "o",
      col = "blue")
legend(
  "left",
  legend = c("LR", "CLR"),
  pch = c(2, 1),
  col = c("red", "blue"),
  bty = "n"
)
dev.off()

png(filename = "Covariance_testing_dimension_effects_power.png",
    width = 1000,
    height = 618)
plot(
  x = DimensionArray,
  y = ResultsDimensionEffectsLRPower,
  "o",
  xlab = "dimension p",
  ylab = "power",
  pch = 2,
  col = "red",
  ylim = c(0, 1)
)
lines(x = DimensionArray,
      y = ResultsDimensionEffectsCLRPower,
      "o",
      col = "blue")
legend(
  "left",
  legend = c("LR", "CLR"),
  pch = c(2, 1),
  col = c("red", "blue"),
  bty = "n"
)
dev.off()
}
## dimension and sample size both growth (done) ---- 
ResultsBothGrowth <-
  BenchMarkCov(testTimes = tt, dimensionVector = DimensionArray)

write.csv(ResultsBothGrowth, file = "Covariance testing both growth.csv")
png(filename = "Covariance testing both growth.png",
    width = 1000,
    height = 618)
plot(
  x = DimensionArray,
  y = ResultsBothGrowth$`LR both growth`,
  "o",
  xlab = "dimension p",
  ylab = "size",
  pch = 2,
  col = "red",
  ylim = c(0, 1)
)
lines(x = DimensionArray,
      y = ResultsBothGrowth$`CLR both growth`,
      "o",
      col = "blue")
legend(
  "left",
  legend = c("LR", "CLR"),
  pch = c(2, 1),
  col = c("red", "blue"),
  bty = "n"
)
dev.off()

png(filename = "Covariance_testing_both_growth_power.png",
    width = 1000,
    height = 618)
plot(
  x = DimensionArray,
  y = ResultsBothGrowth$`LR power`,
  "o",
  xlab = "dimension p",
  ylab = "power",
  pch = 2,
  col = "red",
  ylim = c(0, 1)
)
lines(x = DimensionArray,
      y = ResultsBothGrowth$`CLR power`,
      "o",
      col = "blue")
legend(
  "left",
  legend = c("LR", "CLR"),
  pch = c(2, 1),
  col = c("red", "blue"),
  bty = "n"
)
dev.off()
}
