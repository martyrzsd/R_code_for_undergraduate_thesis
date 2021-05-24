# call function----
source("GLHT.R")
source("data_generation.R")

{
## rho = 0 ----
samplesize <- 1000
testTimes <- 10
parameter <-
  list(c(10, 50), c(30, 100), c(50, 200), c(100, 500))
blockList <- list(1:20, 1:50, 1:100, 1:200)
dimensionEffects <- c()
for (i in 1:4) {
  input_dimension <- unlist(parameter[i])[2]
  p <- unlist(parameter[i])[1]
  B <- mvrnorm(p,
               rep(0, times = input_dimension),
               diag(1, nrow = input_dimension))
  sparseB <-
    matrix(rbinom(p * input_dimension, 2, 0.1), nrow = p)
  B <- sparseB * B
  # size
  for (correction_methods in c("none", "BBC", "RMT")) {
    results_for_one_method <- c() # Initialize array for results
    for (k in 1:testTimes) {
      # Data preparation
      DataPreparation(B = B,
                      RSD = p / (samplesize),
                      rho = 0)
      # For convenience, we choose the following null
      block <- unlist(blockList[i])
      B1 <-
        matrix(rep(0, length(block) * nrow(B)), ncol = length(block))
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
      print(c)
      print(i)
      print(correction_methods)
      print(k)
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
write.csv(dimensionEffects, file = "Sparse_dimensionEffects.csv")
png("Sparse_linear_model_dimension_effects_rho_0.png",
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
  labels = c(
    "(10,50,20)",
    "(30,100,50)",
    "(50,200,100)",
    "(100,500,200)"
  )
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
}
