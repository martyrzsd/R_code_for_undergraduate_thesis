# Packages included

library(MASS)

# Likelihood Ratio Testing

LLR <-
  function(design_input,
           response_input,
           alpha = 0.05,
           block = 1,
           B1,
           BBC = TRUE) {
    # MLE computation of the whole model
    B_estimate <-
      t(crossprod(design_input, response_input)) %*% solve(crossprod(design_input, design_input))
    Sigma_estimate <-
      cov(response_input - t(B_estimate %*% t(design_input)))
    # MLE computation of the residue
    y <- response_input - t(B1 %*% t(design_input[, block]))
    B2_estimate <-
      crossprod(y, design_input[, -block]) %*% solve(crossprod(design_input[, -block], design_input[, -block]))
    Sigma1_estimate <-
      cov(y - t(B2_estimate %*% t(design_input[, -block])))
    # log-likelihood ratio statistics
    Lambda <- det(Sigma_estimate) / det(Sigma1_estimate)
    if (BBC) {    # BBC correction enable
      k <-
        nrow(design_input) - ncol(design_input) - (ncol(response_input) - length(block) +
                                                     1) / 2
      U <- -k * log(Lambda)
    } else{ # BBC correction disable
      U <- -nrow(input_design) * log(Lambda)
    }
    # Large sample threshold computation
    threshold <-
      qchisq(1 - alpha, df = ncol(response_input) * length(block))
    # Result determination
    if (U > threshold) {
      return(1) # reject
    }
    return(0) # accept
  }



CLLR <- function(design_input,
                 response_input,
                 alpha = 0.05,
                 block = 1,
                 B1,) {
  return()
}



DataPreparation <- function(testTimes = 100,
                            dimensionVector = 10,
                            RSD = 0.05,
                            input_dimension = 50,
                            block = 1:30) {
  # p: dimension of response
  # q: dimension of input
  # k: number of experiments
  # n: sample size
  
  # compute the rounded sample size
  n <- ceiling(p / RSD) 
  for (k in 1:testTimes) {
    # Def of the real B and the null B1
    B <-
      mvrnorm(p,
              rep(0, times = input_dimension),
              diag(1, nrow = input_dimension))
    B1 <- B[, block]
    # Data Generation
    ## Design generation
    independent_data <-
      mvrnorm(n,
              rep(1, times = input_dimension),
              diag(
                x = rep(0.5, times = input_dimension),
                nrow = input_dimension
              ))
    ## Generation of covariance matrix for noise
    Sigma <- diag(x = 1, nrow = p)
    rho <- 0.9
    ### Lower diagonal
    for (i in 1:((ncol(Sigma)) - 2)) {
      diag(Sigma[-1:-i, (-ncol(Sigma) + i - 1):-ncol(Sigma)]) <-
        rep(0.9 ^ (i), times = ncol(Sigma) - i)
    }
    ### Corner element
    Sigma[ncol(Sigma), 1] <- rho ^ (ncol(Sigma))
    ### Upper diagonal
    Sigma <-
      Sigma + t(Sigma) - diag(diag(Sigma)) 
    ## Generate Gaussian noise with respect to the covariance matrix
    noise <-
      mvrnorm(n, rep(1, times = p), Sigma) 
    # Generate response with respect to the real model
    response_data <-
      t(B %*% t(independent_data)) + noise # with rows being samples
  }
  # LLRresults <- matrix(LLRresults, ncol = testTimes, byrow = TRUE)
  return(list("Real Design"=B,"Null Design" = B1, "Independent data" = independent_data, "Response data" = response_data))
}

# Initialize array for results
LLRresults <- c()
CLRresults <- c()

DataPreparation(testTimes = 100)

# Running LLR test with BBC
LLRresults[length(LLRresults) + 1] <-
  LLR(
    Test_design,
    Test_response,
    alpha = 0.05,
    block = block,
    B1 = B[,block],
    TRUE
  ) / testTimes
# CLRresults[length(CLRresults) + 1] <-
# CLLR(Test_design, Test_response, 0.05) / testTimes

