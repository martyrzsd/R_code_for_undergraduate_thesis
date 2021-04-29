# General Linear Hypothesis Testing Function

GLHT <-
  function(independent_input,
           response_input,
           alpha = 0.05,
           block = 1,
           B1,
           correction_methods = "none") {
    library(MASS)
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


