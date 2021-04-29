# call function
source("GLHT.R")
source("data_generation.R")

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
