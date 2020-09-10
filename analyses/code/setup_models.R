# ==========================================================================
# Setup: Cognitive models
# Author: Jana B. Jarecki
# ==========================================================================
# library(cognitivemodels) # from janajarecki/github


## Options for parameter estimation ----------------------------------------
model_options <- list(
  lb = c(rp = 0, sigma = 0.0001), # par lower bound: alpha (rp) and sigma
  ub = c(sigma = 1),              # par upper bound
  solver = c("solnp"),    # optimization solver
  fit_args = list(                # type of likelihood function
      pdf = "truncnormal",
      a = 0,                      # lower bound of the trunc. normal dist.
      b = 1                       # upper bound of the trunc. normal dist.
    ),
  solver_args = list(
    options = list(trace = 1)
  )                     
  )


## Set up the models -------------------------------------------------------
# RF: relative frequency model
# BVU: Bayesian value updating model
# BVU_d1: Bayesian value updating model with delta = 1
# Baseline: baseline model

# Reverse the power utility function
reverse_power <- function(x, rp) {
  return((sign(rp) * x)^((1/rp)*(rp!=0)) * exp(x)^(rp==0))
}

# RF
RF <- function(dt) {
  M <- cognitivemodel(data = dt) +
    utility_pow_c(value_scaled ~ gamblex, fix = list(rn = NA)) +
    function(pred, data, par) {
      # re-transform the data onto the original scale
      y <- pred * data$relfreq_x # pred = value prediction
      # Value on original scale
      y <- reverse_power(y, rp = par["rp"])
      # Ceiling: scale down all values that exceed the max of the scale
      y <- replace(y, y > data$gamblex, data$gamblex[y > data$gamblex])
      # Scale to common range 0 - 1
      y / data$gamblex
    }
  fit(M, options = c(model_options))
  return(M)
}

# BVU Model
BVU <- function(dt, fix_bayes = NULL) {
  M <- cognitivemodel(data = dt) +
    bayes_beta_d(~ count_x + count_0, format = "count", fix = fix_bayes, choicerule = "none", prior_sum = 2) +
    utility_pow_c(value_scaled ~ gamblex, fix = list(rn = NA)) +
    function(pred, data, par) {
      # re-transform the data
      y <- data$pr_c * pred # pred = value prediction pr_c = subj probability
      # Value on original scale
      y <- reverse_power(y, rp = par["rp"])
      # Common range 0 - 1
      y / data$gamblex
      # rp <- par["rp"]
      # y <- (sign(rp) * y)^((1/rp)*(rp!=0)) * exp(y)^(rp==0) 
    }
  fit(M, options = model_options)
  return(M)
}

# BVU Model with delta = 1 (Bayesian delta)
BVU_d1 <- function(dt) {
  BVU(dt = dt, fix_bayes = list(delta = NA))
}

# Baseline Model
BASE <- function(dt) {
  model_options$lb <- model_options$lb["sigma"] # only bound on sigma
  baseline_mean_c(value_scaled ~ ., data = dt, options = model_options)
}

# Function for cross-validation
do_cv <- function(model, train_data) {
  if (is.list(train_data)) train_data <- train_data[[1]]
  return(
    list(map(train_data$train, ~ model(dt = as.data.frame(.))))
  )
}