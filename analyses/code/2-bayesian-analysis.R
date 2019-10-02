# ==========================================================================
# Bayesian Analyses: hierarchical lineara regression
# Predicting scaled valuations from sample size, gamble type and model prior
# Author: Jana B. Jarecki
# ==========================================================================
library(data.table)   # fast data processing
library(brms)         # bayesian analyses
library(cogscimodels) # handling cognitive models

# Load fitted cognitive models and participant data
fits <- readRDS("study1_cognitive_models_fit.rds")
d <- fread("../../data/processed/study1.csv", colClasses=list(character="id"))
d <- d[condition=="experience"] # only use experience condition

# Get best-fitting model and model parameters
prior <- fits[model=="bvu", .(prior = coef(fit[[1]])["count_x"]), by = id]
d <- d[prior, on = "id"]
weights <- fits[, as.data.table(cbind(model=c("base","rf","bvu"), weight = anova(fit[[1]], fit[[2]], fit[[3]])[, c("wAIC")])), by = id][, .(winner = model[which.max(weight)]), by = id]
d <- d[weights[, c("id", "winner")], on = "id"]

# Proprocess for fitting
d[, value_scaled := value / gamblex]
d[, id := factor(id)]
d[, samplesizecat_num := as.numeric(factor(samplesizecat, levels = c("xs", "s","m","l")))]
d[winner == "bvu", priorcat := cut(prior, c(0,1,2))]
d[, priorx_cat := factor(priorcat, labels = c(" - loss prior (0,1]", " - gain prior (1,2]", "RF"), exclude = NULL)]
d[, gambletype := factor(gambletype, levels = c("p-bet", "$-bet"))]  
options(contrasts = c("contr.sum", "contr.poly"))

# Fit full model
fit <- brm(
  formula = value_scaled ~ samplesizecat_num * priorx_cat * gambletype + (1 | id),
  data   = d,
  cores  = 3,
  save_all_pars = TRUE,
  iter = 5000)
saveRDS(fit, "study1_bayes_models_fit.rds")

# Fit model without priorx_cat
null_fit = update(fit, formula = ~ samplesizecat_num * gambletype + (1 | id))  # Same but without the predictor model-and-prior
saveRDS(null_fit, "study1_bayes_models_fit_noprior.rds")


# compare the models
BF01 <- bayes_factor(fit, null_fit)

# Compare the models
cv <- loo(fit, fit_noprior)
fit <- add_criterion(fit, "waic")
fit_noprior <- add_criterion(fit_noprior, "waic")
waic <- loo_compare(fit, fit_noprior, criterion = "waic")
