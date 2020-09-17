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
RF <- function(dt, fix = c(gamman=1,gammap=1), w="TK1992", v="TK1992") {
  model_options$lb <- model_options$lb["sigma"] # only bound on sigma
  dt$gamble_y <- 0
  fix <- c(fix, c(lambda=1,beta=1))
  M <- cognitivemodel(data = dt) +
    cpt_c(value_scaled ~ gamblex + relfreq_x + gamble_y , weighting=w, value=v, fix = fix) +
    function(pred, data, par) {
      y <- reverse_power(pred, rp = par["alpha"])
      y <- replace(y, y > data$gamblex, data$gamblex[y > data$gamblex])
      y / data$gamblex # Scale to common range 0 - 1
    }
  fit(M, options = c(model_options))
  return(M)
}

RF_w <- function(dt) {
  RF(dt = dt, fix = c(alpha=1), v = NA)
}

RF_wv <- function(dt) {
  RF(dt = dt, v = NA, w=NA)
}



# BVU Model
BVU <- function(dt, fix = NULL) {
  fixb <- fix[names(fix) %in% c("count_x", "count_0", "delta")]
  fixu <- fix[names(fix) %in% c("rp")]
  M <- cognitivemodel(data = dt) +
    bayes_beta_d(~ count_x + count_0, format = "count", fix = fixb, choicerule = "none", prior_sum = 2) +
    utility_pow_c(value_scaled ~ gamblex, fix = c(list(rn = NA), fixu)) +
    function(pred, data, par) {
      y <- data$pr_c * pred # pred = value prediction pr_c = subj probability
      # Value on original scale
      y <- reverse_power(y, rp = par["rp"])
      # Ceiling: scale down all values that exceed the max of the scale
      y <- replace(y, y > data$gamblex, data$gamblex[y > data$gamblex])
      y / data$gamblex # Scale to common range 0 - 1
    }
  fit(M, options = model_options)
  return(M)
}

BVU_a <- function(dt) {
  BVU(dt = dt, fix = list(rp = NA))
}

BVU_d <- function(dt) {
  BVU(dt = dt, fix = list(delta = 1))
}

BVU_p <- function(dt) {
  BVU(dt = dt, fix = list(count_x = 1, count_0 = 1))
}

BVU_pd <- function(dt) {
  BVU(dt = dt, fix = list(count_x = 1, count_0 = 1, delta = 1))
}

# BVU Model with delta = 1 (Bayesian delta)
BVU_ad <- function(dt) {
  BVU(dt = dt, fix = list(rp = NA, delta = 1))
}

# BVU Model with delta = 1 (Bayesian delta)
BVU_ap <- function(dt) {
  BVU(dt = dt, fix = list(rp = NA, count_x = 1, count_0 = 1))
}

# BVU Model with delta = 1 (Bayesian delta)
BVU_apd <- function(dt) {
  BVU(dt = dt, fix = list(rp = NA, count_x = 1, count_0 = 1, delta = 1))
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