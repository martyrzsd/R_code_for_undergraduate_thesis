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