# call function----
source("GLHT.R")
source("data_generation.R")

# non sparse----
## rho = 0, sample size grow----
final_plot_rho0 <- c()
testTimes = 2000
### first group----
#### Initialize constants
p = 10
RSD = 0.05
input_dimension = 50
block = 1:20

library(MASS)
B <- mvrnorm(p,
             rep(0, times = input_dimension),
             diag(1, nrow = input_dimension))

#### Running LLR test for all three corrections for size

for (correction_methods in c("none", "BBC", "RMT")) {
  results_for_one_method <- c() # Initialize array for results
  for (k in 1:testTimes) {
    # Data preparation
    DataPreparation(B, RSD, rho = 0)
    # For convenience, we choose the following null
    B1 <- B[, block]
    # Run the function
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
  # Name the out put by the correction methods used
  assign(correction_methods, results_for_one_method)
  final_plot_rho0[length(final_plot_rho0) + 1] <-
    sum(get(correction_methods))
}

### second group----
p = 30
RSD = 0.05
input_dimension = 100
block = 1:50

library(MASS)
B <- mvrnorm(p,
             rep(0, times = input_dimension),
             diag(1, nrow = input_dimension))

#### Running LLR test for all three corrections for size

for (correction_methods in c("none", "BBC", "RMT")) {
  results_for_one_method <- c() # Initialize array for results
  for (k in 1:testTimes) {
    # Data preparation
    DataPreparation(B, RSD, rho = 0)
    # For convenience, we choose the following null
    B1 <- B[, block]
    # Run the function
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
  # Name the out put by the correction methods used
  assign(correction_methods, results_for_one_method)
  final_plot_rho0[length(final_plot_rho0) + 1] <-
    sum(get(correction_methods))
}

### third group----
p = 50
RSD = 0.05
input_dimension = 200
block = 1:100

library(MASS)
B <- mvrnorm(p,
             rep(0, times = input_dimension),
             diag(1, nrow = input_dimension))

#### Running LLR test for all three corrections for size

for (correction_methods in c("none", "BBC", "RMT")) {
  results_for_one_method <- c() # Initialize array for results
  for (k in 1:testTimes) {
    # Data preparation
    DataPreparation(B, RSD, rho = 0)
    # For convenience, we choose the following null
    B1 <- B[, block]
    # Run the function
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
  # Name the out put by the correction methods used
  assign(correction_methods, results_for_one_method)
  final_plot_rho0[length(final_plot_rho0) + 1] <-
    sum(get(correction_methods))
}

### fourth group----
p = 100
RSD = 0.05
input_dimension = 500
block = 1:200

library(MASS)
B <- mvrnorm(p,
             rep(0, times = input_dimension),
             diag(1, nrow = input_dimension))

#### Running LLR test for all three corrections for size

for (correction_methods in c("none", "BBC", "RMT")) {
  results_for_one_method <- c() # Initialize array for results
  for (k in 1:testTimes) {
    # Data preparation
    DataPreparation(B, RSD, rho = 0)
    # For convenience, we choose the following null
    B1 <- B[, block]
    # Run the function
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
  # Name the out put by the correction methods used
  assign(correction_methods, results_for_one_method)
  final_plot_rho0[length(final_plot_rho0) + 1] <-
    sum(get(correction_methods))
}

### graphical processing ----
final_plot_rho0 <-
  as.data.frame(matrix(final_plot_rho0, ncol = 3, byrow = TRUE))
colnames(final_plot_rho0) <- c("None", "BBC", "RMT")

{
  png("COV dimension-sample both rho 0.png",
      width = 1000,
      height = 618)
  plot(
    x = c(1, 2, 3, 4),
    y = final_plot_rho0[, 1],
    "o",
    pch = 1,
    ylim = c(0, 1),
    col = "red",
    xlab = "dimension settings, p,q,q_1, rho = 0",
    ylab = "Size",
    xaxt = "n"
  )
  axis(
    1,
    at = 1:4,
    labels = c(
      "(10,50,20)",
      "(30,100,50)",
      "(50,200,100)",
      "(100,500,200)"
    )
  )
  lines(
    x = c(1, 2, 3, 4),
    y = final_plot_rho0[, 2],
    "o",
    pch = 2,
    col = "blue"
  )
  lines(
    x = c(1, 2, 3, 4),
    y = final_plot_rho0[, 3],
    "o",
    pch = 4,
    col = "green"
  )
  legend(
    "left",
    legend = c("None", "BBC", "RMT"),
    col = c("red", "blue", "green"),
    lty = 1,
    pch = c(1, 2, 4),
    title = "Methods",
    # text.font = 4,
    bty = "n"
  )
  dev.off()
}




## rho = 0.9----
final_plot_rho09 <- c()
### first group----
p = 10
RSD = 0.05
input_dimension = 50
block = 1:20
library(MASS)
B <- mvrnorm(p,
             rep(0, times = input_dimension),
             diag(1, nrow = input_dimension))

for (correction_methods in c("none", "BBC", "RMT")) {
  results_for_one_method <- c() # Initialize array for results
  for (k in 1:testTimes) {
    # Data preparation
    DataPreparation(B, RSD, rho = 0.9)
    # For convenience, we choose the following null
    B1 <- B[, block]
    # Run the function
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
  # Name the out put by the correction methods used
  assign(correction_methods, results_for_one_method)
  final_plot_rho09[length(final_plot_rho09) + 1] <-
    sum(get(correction_methods))
}
### second group----
p = 30
RSD = 0.05
input_dimension = 100
block = 1:50
library(MASS)
B <- mvrnorm(p,
             rep(0, times = input_dimension),
             diag(1, nrow = input_dimension))
for (correction_methods in c("none", "BBC", "RMT")) {
  results_for_one_method <- c() # Initialize array for results
  for (k in 1:testTimes) {
    # Data preparation
    DataPreparation(B, RSD, rho = 0.9)
    # For convenience, we choose the following null
    B1 <- B[, block]
    # Run the function
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
  # Name the out put by the correction methods used
  assign(correction_methods, results_for_one_method)
  final_plot_rho09[length(final_plot_rho09) + 1] <-
    sum(get(correction_methods))
}

### third group ----
p = 50
RSD = 0.05
input_dimension = 200
block = 1:100
library(MASS)
B <- mvrnorm(p,
             rep(0, times = input_dimension),
             diag(1, nrow = input_dimension))
for (correction_methods in c("none", "BBC", "RMT")) {
  results_for_one_method <- c() # Initialize array for results
  for (k in 1:testTimes) {
    # Data preparation
    DataPreparation(B, RSD, rho = 0.9)
    # For convenience, we choose the following null
    B1 <- B[, block]
    # Run the function
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
  # Name the out put by the correction methods used
  assign(correction_methods, results_for_one_method)
  final_plot_rho09[length(final_plot_rho09) + 1] <-
    sum(get(correction_methods))
}

### fourth group ----
p = 100
RSD = 0.05
input_dimension = 500
block = 1:200
library(MASS)
B <- mvrnorm(p,
             rep(0, times = input_dimension),
             diag(1, nrow = input_dimension))
for (correction_methods in c("none", "BBC", "RMT")) {
  results_for_one_method <- c() # Initialize array for results
  for (k in 1:testTimes) {
    # Data preparation
    DataPreparation(B, RSD, rho = 0.9)
    # For convenience, we choose the following null
    B1 <- B[, block]
    # Run the function
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
  # Name the out put by the correction methods used
  assign(correction_methods, results_for_one_method)
  final_plot_rho09[length(final_plot_rho09) + 1] <-
    sum(get(correction_methods))
}

### graphical processing----
final_plot_rho09 <-
  as.data.frame(matrix(final_plot_rho09, ncol = 3, byrow = TRUE))
colnames(final_plot_rho09) <- c("None", "BBC", "RMT")
png("COV dimension-sample both rho 09.png",
    width = 1000,
    height = 618)
{
  plot(
    x = c(1, 2, 3, 4),
    y = final_plot_rho09[, 1],
    "o",
    pch = 1,
    ylim = c(0, 1),
    col = "red",
    xlab = "dimension settings, p,q,q_1, rho = 0.9
",
    ylab = "Size",
    xaxt = "n"
  )
  axis(
    1,
    at = 1:4,
    labels = c(
      "(10,50,20)",
      "(30,100,50)",
      "(50,200,100)",
      "(100,500,200)"
    )
  )
  lines(
    x = c(1, 2, 3, 4),
    y = final_plot_rho09[, 2],
    "o",
    pch = 2,
    col = "blue"
  )
  lines(
    x = c(1, 2, 3, 4),
    y = final_plot_rho09[, 3],
    "o",
    pch = 4,
    col = "green"
  )
  legend(
    "left",
    legend = c("None", "BBC", "RMT"),
    col = c("red", "blue", "green"),
    lty = 1,
    pch = c(1, 2, 4),
    title = "Methods",
    text.font = 4,
    bty = "n"
  )
  dev.off()
}

# power test ----
p = 100
RSD = 0.05
input_dimension = 500
block = 1:200

library(MASS)
B <- mvrnorm(p,
             rep(0, times = input_dimension),
             diag(1, nrow = input_dimension))
powerPlot <- c()
# Running LLR test for all three corrections for size
for (c in seq(0, 1, 0.1)) {
  for (correction_methods in c("none", "BBC", "RMT")) {
    results_for_one_method <- c() # Initialize array for results
    for (k in 1:testTimes) {
      # Data preparation
      DataPreparation(c * B, RSD, rho = 0)
      # For convenience, we choose the following null
      B1 <- B[, block]
      # Run the function
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
    # Name the out put by the correction methods used
    assign(correction_methods, results_for_one_method)
    powerPlot[length(final_plot_rho0) + 1] <-
      sum(get(correction_methods))
  }
}
### graphical processing ----
powerPlot <-
  as.data.frame(matrix(powerPlot, ncol = 3, byrow = TRUE))
colnames(final_plot_rho0) <- c("None", "BBC", "RMT")
png("power rho 0.png",
    width = 1000,
    height = 618)
plot(
  x = c(1, 2, 3, 4),
  y = final_plot_rho0[, 1],
  "o",
  pch = 1,
  ylim = c(0, 1),
  col = "red",
  xlab = "dimension settings, p,q,q_1, rho = 0",
  ylab = "Size",
  xaxt = "n"
)
axis(
  1,
  at = 1:4,
  labels = c("(10,50,20)",
             "(30,100,50)",
             "(50,200,100)",
             "(100,500,200)")
)
lines(
  x = c(1, 2, 3, 4),
  y = final_plot_rho0[, 2],
  "o",
  pch = 2,
  col = "blue"
)
lines(
  x = c(1, 2, 3, 4),
  y = final_plot_rho0[, 3],
  "o",
  pch = 4,
  col = "green"
)
legend(
  "left",
  legend = c("None", "BBC", "RMT"),
  col = c("red", "blue", "green"),
  lty = 1,
  pch = c(1, 2, 4),
  title = "Methods",
  # text.font = 4,
  bty = "n"
)
dev.off()





# only dimension growth ----
samplesize <- 1000
testTimes <- 2000
parameter <-
  list(c(10, 50), c(30, 100), c(50, 200), c(100, 500))
blockList <- list(1:20, 1:50, 1:100, 1:200)
dimensionEffects <- c()
for (i in 1:4) {
  # size
  for (correction_methods in c("none", "BBC", "RMT")) {
    results_for_one_method <- c() # Initialize array for results
    for (k in 1:testTimes) {
      # Data preparation
      RSD <- unlist(parameter[i])[1] / (samplesize)
      DataPreparation(B, RSD, rho = 0)
      # For convenience, we choose the following null
      block <- unlist(blockList[i])
      B1 <- B[, block]
      # Run the function
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
    # Name the out put by the correction methods used
    assign(correction_methods, results_for_one_method)
    dimensionEffects[length(dimensionEffects) + 1] <-
      sum(get(correction_methods))
  }
}
### graphical processing----
dimensionEffects <-
  as.data.frame(matrix(dimensionEffects, ncol = 3, byrow = TRUE))
colnames(dimensionEffects) <- c("None", "BBC", "RMT")
png("COV dimension rho 0.png",
    width = 1000,
    height = 618)
plot(
  x = c(1, 2, 3, 4),
  y = dimensionEffects[, 1],
  "o",
  pch = 1,
  ylim = c(0, 1),
  col = "red",
  xlab = "dimension settings, p,q,q_1, rho = 0",
  ylab = "Size",
  xaxt = "n"
)
axis(
  1,
  at = 1:4,
  labels = c("(10,50,20)",
             "(30,100,50)",
             "(50,200,100)",
             "(100,500,200)")
)
lines(
  x = c(1, 2, 3, 4),
  y = dimensionEffects[, 2],
  "o",
  pch = 2,
  col = "blue"
)
lines(
  x = c(1, 2, 3, 4),
  y = dimensionEffects[, 3],
  "o",
  pch = 4,
  col = "green"
)
legend(
  "left",
  legend = c("None", "BBC", "RMT"),
  col = c("red", "blue", "green"),
  lty = 1,
  pch = c(1, 2, 4),
  title = "Methods",
  # text.font = 4,
  bty = "n"
)
dev.off()




# Sparse case----
## rho = 0----
sparse_final_plot_rho0 <- c()
### first group----
## Initialize constants
p = 10
RSD = 0.05
input_dimension = 50
block = 1:20
testTimes = 100

library(MASS)
B <- mvrnorm(p,
             rep(0, times = input_dimension),
             diag(1, nrow = input_dimension))
sparseB <-
  matrix(rbinom(p * input_dimension, 2, 0.1), nrow = p)
B <- sparseB * B
## Running LLR test for all three corrections for size

for (correction_methods in c("none", "BBC", "RMT")) {
  results_for_one_method <- c() # Initialize array for results
  for (k in 1:testTimes) {
    # Data preparation
    DataPreparation(B, RSD, rho = 0)
    # For convenience, we choose the following null
    B1 <- B[, block]
    # Run the function
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
  # Name the out put by the correction methods used
  assign(correction_methods, results_for_one_method)
  sparse_final_plot_rho0[length(sparse_final_plot_rho0) + 1] <-
    sum(get(correction_methods))
}
### second group----
p = 30
RSD = 0.05
input_dimension = 100
block = 1:50
testTimes = 100

library(MASS)
B <- mvrnorm(p,
             rep(0, times = input_dimension),
             diag(1, nrow = input_dimension))
sparseB <-
  matrix(rbinom(p * input_dimension, 2, 0.1), nrow = p)
B <- sparseB * B
## Running LLR test for all three corrections for size

for (correction_methods in c("none", "BBC", "RMT")) {
  results_for_one_method <- c() # Initialize array for results
  for (k in 1:testTimes) {
    # Data preparation
    DataPreparation(B, RSD, rho = 0)
    # For convenience, we choose the following null
    B1 <- B[, block]
    # Run the function
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
  # Name the out put by the correction methods used
  assign(correction_methods, results_for_one_method)
  sparse_final_plot_rho0[length(sparse_final_plot_rho0) + 1] <-
    sum(get(correction_methods))
}
### third group----
p = 50
RSD = 0.05
input_dimension = 200
block = 1:100
testTimes = 100

library(MASS)
B <- mvrnorm(p,
             rep(0, times = input_dimension),
             diag(1, nrow = input_dimension))
sparseB <-
  matrix(rbinom(p * input_dimension, 2, 0.1), nrow = p)
B <- sparseB * B
## Running LLR test for all three corrections for size

for (correction_methods in c("none", "BBC", "RMT")) {
  results_for_one_method <- c() # Initialize array for results
  for (k in 1:testTimes) {
    # Data preparation
    DataPreparation(B, RSD, rho = 0)
    # For convenience, we choose the following null
    B1 <- B[, block]
    # Run the function
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
  # Name the out put by the correction methods used
  assign(correction_methods, results_for_one_method)
  sparse_final_plot_rho0[length(sparse_final_plot_rho0) + 1] <-
    sum(get(correction_methods))
}
### fourth group----
p = 100
RSD = 0.05
input_dimension = 500
block = 1:200
testTimes = 100

library(MASS)
B <- mvrnorm(p,
             rep(0, times = input_dimension),
             diag(1, nrow = input_dimension))
sparseB <-
  matrix(rbinom(p * input_dimension, 2, 0.1), nrow = p)
B <- sparseB * B
## Running LLR test for all three corrections for size

for (correction_methods in c("none", "BBC", "RMT")) {
  results_for_one_method <- c() # Initialize array for results
  for (k in 1:testTimes) {
    # Data preparation
    DataPreparation(B, RSD, rho = 0)
    # For convenience, we choose the following null
    B1 <- B[, block]
    # Run the function
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
  # Name the out put by the correction methods used
  assign(correction_methods, results_for_one_method)
  sparse_final_plot_rho0[length(sparse_final_plot_rho0) + 1] <-
    sum(get(correction_methods))
}
### graphical processing ----
sparse_final_plot_rho0 <-
  as.data.frame(matrix(sparse_final_plot_rho0,
                       ncol = 3,
                       byrow = TRUE))
colnames(sparse_final_plot_rho0) <- c("None", "BBC", "RMT")

{
  plot(
    x = c(1, 2, 3, 4),
    y = sparse_final_plot_rho0[, 1],
    "o",
    pch = 1,
    ylim = c(0, 1),
    col = "red",
    xlab = "dimension settings, p,q,q_1, rho = 0",
    ylab = "Size",
    xaxt = "n"
  )
  axis(
    1,
    at = 1:4,
    labels = c(
      "(10,50,20)",
      "(30,100,50)",
      "(50,200,100)",
      "(100,500,200)"
    )
  )
  lines(
    x = c(1, 2, 3, 4),
    y = sparse_final_plot_rho0[, 2],
    "o",
    pch = 2,
    col = "blue"
  )
  lines(
    x = c(1, 2, 3, 4),
    y = sparse_final_plot_rho0[, 3],
    "o",
    pch = 4,
    col = "green"
  )
  legend(
    "left",
    legend = c("None", "BBC", "RMT"),
    col = c("red", "blue", "green"),
    lty = 1,
    pch = c(1, 2, 4),
    title = "Methods",
    text.font = 4,
    bty = "n"
  )
}
## rho = 0.9----
sparse_final_plot_rho09 <- c()
### first group----
p = 10
RSD = 0.05
input_dimension = 50
block = 1:20
testTimes = 100

library(MASS)
B <- mvrnorm(p,
             rep(0, times = input_dimension),
             diag(1, nrow = input_dimension))
sparseB <-
  matrix(rbinom(p * input_dimension, 2, 0.1), nrow = p)
B <- sparseB * B
for (correction_methods in c("none", "BBC", "RMT")) {
  results_for_one_method <- c() # Initialize array for results
  for (k in 1:testTimes) {
    # Data preparation
    DataPreparation(B, RSD, rho = 0.9)
    # For convenience, we choose the following null
    B1 <- B[, block]
    # Run the function
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
  # Name the out put by the correction methods used
  assign(correction_methods, results_for_one_method)
  sparse_final_plot_rho09[length(sparse_final_plot_rho09) + 1] <-
    sum(get(correction_methods))
}

### second group----
p = 30
RSD = 0.05
input_dimension = 100
block = 1:50
testTimes = 100
library(MASS)
B <- mvrnorm(p,
             rep(0, times = input_dimension),
             diag(1, nrow = input_dimension))
sparseB <-
  matrix(rbinom(p * input_dimension, 2, 0.1), nrow = p)
B <- sparseB * B
for (correction_methods in c("none", "BBC", "RMT")) {
  results_for_one_method <- c() # Initialize array for results
  for (k in 1:testTimes) {
    # Data preparation
    DataPreparation(B, RSD, rho = 0.9)
    # For convenience, we choose the following null
    B1 <- B[, block]
    # Run the function
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
  # Name the out put by the correction methods used
  assign(correction_methods, results_for_one_method)
  sparse_final_plot_rho09[length(sparse_final_plot_rho09) + 1] <-
    sum(get(correction_methods))
}

### third group----
p = 50
RSD = 0.05
input_dimension = 200
block = 1:100
testTimes = 100
library(MASS)
B <- mvrnorm(p,
             rep(0, times = input_dimension),
             diag(1, nrow = input_dimension))
sparseB <-
  matrix(rbinom(p * input_dimension, 2, 0.1), nrow = p)
B <- sparseB * B
for (correction_methods in c("none", "BBC", "RMT")) {
  results_for_one_method <- c() # Initialize array for results
  for (k in 1:testTimes) {
    # Data preparation
    DataPreparation(B, RSD, rho = 0.9)
    # For convenience, we choose the following null
    B1 <- B[, block]
    # Run the function
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
  # Name the out put by the correction methods used
  assign(correction_methods, results_for_one_method)
  sparse_final_plot_rho09[length(sparse_final_plot_rho09) + 1] <-
    sum(get(correction_methods))
}

### fourth group----
p = 100
RSD = 0.05
input_dimension = 500
block = 1:200
testTimes = 100
library(MASS)
B <- mvrnorm(p,
             rep(0, times = input_dimension),
             diag(1, nrow = input_dimension))
sparseB <-
  matrix(rbinom(p * input_dimension, 2, 0.1), nrow = p)
B <- sparseB * B
for (correction_methods in c("none", "BBC", "RMT")) {
  results_for_one_method <- c() # Initialize array for results
  for (k in 1:testTimes) {
    # Data preparation
    DataPreparation(B, RSD, rho = 0.9)
    # For convenience, we choose the following null
    B1 <- B[, block]
    # Run the function
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
  # Name the out put by the correction methods used
  assign(correction_methods, results_for_one_method)
  sparse_final_plot_rho09[length(sparse_final_plot_rho09) + 1] <-
    sum(get(correction_methods))
}

### graphical processing----
sparse_final_plot_rho09 <-
  as.data.frame(matrix(sparse_final_plot_rho09,
                       ncol = 3,
                       byrow = TRUE))
colnames(sparse_final_plot_rho09) <- c("None", "BBC", "RMT")
{
  plot(
    x = c(1, 2, 3, 4),
    y = sparse_final_plot_rho09[, 1],
    "o",
    pch = 1,
    ylim = c(0, 1),
    col = "red",
    xlab = "dimension settings, p,q,q_1, rho = 0.9",
    ylab = "Size",
    xaxt = "n"
  )
  axis(
    1,
    at = 1:4,
    labels = c(
      "(10,50,20)",
      "(30,100,50)",
      "(50,200,100)",
      "(100,500,200)"
    )
  )
  lines(
    x = c(1, 2, 3, 4),
    y = sparse_final_plot_rho09[, 2],
    "o",
    pch = 2,
    col = "blue"
  )
  lines(
    x = c(1, 2, 3, 4),
    y = sparse_final_plot_rho09[, 3],
    "o",
    pch = 4,
    col = "green"
  )
  legend(
    "left",
    legend = c("None", "BBC", "RMT"),
    col = c("red", "blue", "green"),
    lty = 1,
    pch = c(1, 2, 4),
    title = "Methods",
    text.font = 4,
    bty = "n"
  )
  
  
  
  
  # Sparse Oracle----
  