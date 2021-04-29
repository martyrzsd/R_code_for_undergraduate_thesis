# Packages included

library(MASS)

# Likelihood Ratio Testing

GLHT <-
  function(independent_input,
           response_input,
           alpha = 0.05,
           block = 1,
           B1,
           correction_methods = "none") {
    # MLE computation of the whole model
    B_estimate <-
      t(crossprod(independent_input, response_input)) %*% solve(crossprod(independent_input, independent_input))
    Sigma_estimate <-
      cov(response_input - t(B_estimate %*% t(independent_input)))
    # MLE computation of the residue
    y <- response_input - t(B1 %*% t(independent_input[, block]))
    B2_estimate <-
      crossprod(y, independent_input[,-block]) %*% solve(crossprod(independent_input[,-block], independent_input[,-block]))
    Sigma1_estimate <-
      cov(y - t(B2_estimate %*% t(independent_input[,-block])))
    # log-likelihood ratio statistics
    Lambda <- det(Sigma_estimate) / det(Sigma1_estimate)
    # Different Correction
    if (correction_methods == "BBC") {
      # BBC correction enable
      k <-
        nrow(independent_input) - ncol(independent_input) - (ncol(response_input) - length(block) +
                                                               1) / 2
      U <- -k * log(Lambda)
      threshold <-
        qchisq(1 - alpha, df = ncol(response_input) * length(block))
      if (U > threshold) {
        return(1) # reject
      }
      return(0) # accept
    }
    if (correction_methods == "none") {
      # No Correction
      U <- -nrow(independent_input) * log(Lambda)
      threshold <-
        qchisq(1 - alpha, df = ncol(response_input) * length(block))
      if (U > threshold) {
        return(1) # reject
      }
      return(0) # accept
    }
    if (correction_methods == "RMT") {
      # Compute all needed constant with limit replaced by empirical.
      y1 <- ncol(response_input) / length(block)
      y2 <-
        ncol(response_input) / (nrow(response_input) - ncol(independent_input))
      hn <- sqrt(y1 + y2 - y1 * y2)
      an <- (1 - hn) ^ 2 / (1 - y2) ^ 2
      bn <- (1 + hn) ^ 2 / (1 - y2) ^ 2
      dn <- (sqrt(1 + y2 * bn / y1) - sqrt(1 + y2 * an / y1)) / 2
      cn <- (sqrt(1 + y2 * bn / y1) + sqrt(1 + y2 * an / y1)) / 2
      F_f <-
        (y2 - 1) / y2 * log(cn) + (y1 - 1) / y1 * log(cn - dn * hn) + (y1 + y2) *
        log((cn * hn - dn * y2) / hn) / (y1 * y2)
      m <- log((cn ^ 2 - dn ^ 2) * hn ^ 2 / (cn * hn - y2 * dn) ^ 2) / 2
      v <- 2 * log(cn ^ 2 / (cn ^ 2 - dn ^ 2))
      # Compute the CLRT statistics
      CLRT <- (-log(Lambda) - ncol(response_input) * F_f - m) * v ^ {
        -1 / 2
      }
      # Generate threshold with respect to asymptotic distribution
      Upperthreshold <- qnorm(1 - alpha / 2)
      Lowerthreshold <- qnorm(alpha / 2)
      if (CLRT > Upperthreshold || CLRT < Lowerthreshold) {
        return(1) # reject
      }
      return(0) # accept
    }
  }


# Data Generation Function

DataPreparation <- function(response_dimension ,
                            RSD ,
                            input_dimension) {
  # p: dimension of response
  # q: dimension of input
  # n: sample size
  # We are exporting all data directly inside the global environment
  # compute the rounded sample size
  n <- ceiling(response_dimension / RSD)
  # Def of the real B
  B <<-
    mvrnorm(response_dimension,
            rep(0, times = input_dimension),
            diag(1, nrow = input_dimension))
  
  # Data Generation
  ## Design generation
  independent_data <<-
    mvrnorm(n,
            rep(1, times = input_dimension),
            diag(x = rep(0.5, times = input_dimension),
                 nrow = input_dimension))
  ## Generation of covariance matrix for noise
  Sigma <- diag(x = 1, nrow = response_dimension)
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
    mvrnorm(n, rep(1, times = response_dimension), Sigma)
  # Generate response with respect to the real model
  response_data <<-
    t(B %*% t(independent_data)) + noise # with rows being samples
  # return()
}

# Testing

## Initialize constants
p = 20
RSD = 0.2
input_dimension = 60
block = 1:50
testTimes = 100

## Running LLR test for all three corrections for size

for (correction_methods in c("none", "BBC", "RMT")) {
  results_for_one_method <- c() # Initialize array for results
  for (k in 1:testTimes) {
    # Data preparation
    DataPreparation(p, RSD, input_dimension)
    # For convenience, we choose the following null
    B1 <- B[, block]
    results_for_one_method[length(results_for_one_method) + 1] <-
      GLHT(
        independent_data,
        response_data ,
        alpha = 0.05,
        block = block,
        B1 = B1,
        correction_methods
      ) / testTimes
  }
  assign(correction_methods, results_for_one_method)
}

sum(BBC)
sum(none)
sum(RMT)
