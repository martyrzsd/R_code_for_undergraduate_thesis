# Packages included

library(MASS)

# Generating test data

# Sigma <- diag(x = 1, nrow = p)
# Test_data<-mvrnorm(n,rep(0,times = p),Sigma)

# Likelihood ratio test function

LR <- function(input, alpha){
  Empirical_Sigma <- cov(input)
  L <- sum(diag(Empirical_Sigma)) - log(abs(det(Empirical_Sigma))) - p
  likelihood_statistics <- n*L
  size <- pchisq(likelihood_statistics, df = ncol(input)*(ncol(input)+1)/2, lower.tail = FALSE)
  if (size >= alpha) {
    return(0) #Accept
  }
  return(1) # Reject
}

CLR <- function(input,alpha){
  d <- ncol(input)
  yn <- d/nrow(input)
  Empirical_Sigma <- cov(input)
  L <- sum(diag(Empirical_Sigma)) - log(abs(det(Empirical_Sigma))) - p
  CLRn <- (L-d*(1-(1-1/yn)*log(1-yn))-log(1-yn)/2)/(sqrt(-2*log(1-yn)-2*yn))
  size <- pnorm(CLRn, mean =0,sd=1,lower.tail = FALSE)
  if (size >= alpha) {
    return(0) #Accept
  }
  return(1) # Reject
}

# Benchmark Function

BenchMarkLR <- function(testTimes = 100, dimensionVector = c(10,50,100,200,500)){
  results <- c()
  for (p in dimensionVector) {
    n <- 30*p
    for (k in 1:testTimes) {
      Sigma <- diag(x = 1, nrow = p)
      Test_data<-mvrnorm(n,rep(0,times = p),Sigma)
      results[length(results)+1] <- LR(Test_data,0.05)
    }
  }
  results <- matrix(results, ncol = testTimes, byrow = TRUE)
  return(list("Dimension effects"=apply(results,1,sum)/testTimes, "Raw results" = results))
}

LRresults <- BenchMarkLR(100)



curve(dchisq(x, df = p*(p+1)/2), from = 0, to = p*p)
