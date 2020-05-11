# ==========================================================================
# Setup: Cognitive models
# Author: Jana B. Jarecki
# ==========================================================================
library(cognitivemodels) # v0.0.4 of the package

## Options for parameter estimation
model_options <- list(
  lb = c(rp = 0, sigma = 0.0001), # par lower bound: alpha (rp) and sigma
  ub = c(sigma = 1),              # par upper bound
  solver = "solnp",               # optimization solver
  fit_args = list(                # type of likelihood function
    options = list(
      pdf = "truncnormal",
      a = 0,                      # lower bound of the trunc. normal dist.
      b = 1))                     # upper bound of the trunc. normal dist.
  )


## Set up the models -------------------------------------------------------
# RF: relative frequency model
# BVU: Bayesian value updating model
# BVU_d1: Bayesian value updating model with delta = 1
# Baseline: baseline model

# RF
RF <- function(d, fix = list(rn = NA)) {
  M <- cognitivemodel(data = d) +
    utility_pow_c(value_scaled ~ gamblex, fix = fix) +
    function(pred, data, par) {
      # re-transform the data onto the original scale
      y <- data$relfreq_x * pred
      rp <- par["rp"]
      y <- (sign(rp) * y)^((1/rp)*(rp!=0)) * exp(y)^(rp==0)
      y <- replace(y, y > data$gamblex, data$gamblex[y > data$gamblex])
      y / data$gamblex
    }
  fit(M, options = model_options)
  return(M)
}

# BVU Model
BVU<- function(d, fix_bayes = list()) {
  M <- cognitivemodel(data = d) +
    bayes_beta(~ count_x + count_0, format = "count", fix = fix_bayes) +
    utility_pow_c(value_scaled ~ gamblex, fix = list(rn = NA)) + # fix negative parameter to not exist
    function(pred, data, par) {
      # re-transform the data
      y <- data$pr_c * pred
      rp <- par["rp"]
      y <- (sign(rp) * y)^((1/rp)*(rp!=0)) * exp(y)^(rp==0)
      y / data$gamblex
    }
  fit(M, options = model_options)
  return(M)
}

# BVU Model with delta = 1 (Bayesian delta)
BVU_d1 <- function(d, fix_bayes = list()) {
  BVU(d, fix_bayes = list(delta = NA))
}

# Baseline Model
Baseline <- function(d) {
  baseline_mean_c(value_scaled ~ ., data = d)
}