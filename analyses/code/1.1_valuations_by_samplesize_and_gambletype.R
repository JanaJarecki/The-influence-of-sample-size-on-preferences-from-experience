# ==========================================================================
# Bayesian Analyses
# Predicting valuations from sample size and condition
# Author: Jana B. Jarecki
# ==========================================================================
library(data.table)   # fast data processing
library(BayesFactor)  # bayesian anova
library(bayesplot)    # bayesian diagnostic plots

# Load fitted cognitive models and participant data
#study <- 2
d <- fread(sub("x", study, "../../data/processed/studyx.csv"))




# Fit the models
# sample size as random effect
# rating ~ id + evf + ptype*s
# file.remove(sub("x", study, "studyx_bayes_lm_ss_re.rds"))
# fit_re <- brm(
#   formula = value ~ evf + gambletype + (1 | id),
#   data = d,
#   save_all_pars = TRUE,
#   file = sub("x", study, "studyx_bayes_lm_ss_re"),
#   warmup = 1000,
#   iter = 4000,
#   control = list(adapt_delta = 0.999),
#   cores = 4)
# # sample size as fixed effect
# fit_fe <- update(fit_re,
#   formula = . ~ . + samplesizecat,
#   newdata = d,
#   file = sub("x", study, "studyx_bayes_lm_ss_fe"))
# # sample size x gamble-type interaction
# fit_int <- update(fit_fe,
#   formula = . ~ . + gambletype:samplesize,
#   newdata = d,
#   file = sub("x", study, "studyx_bayes_lm_ssXgt"))

# # Obtain bayes factors
# BF_re_fe <- bayes_factor(fit_re, fit_fe)
# BF_re_int <- bayes_factor(fit_re, fit_int)

