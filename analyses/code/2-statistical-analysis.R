# ==========================================================================
# Statistical Analyses
# Author: Jana B. Jarecki
# ==========================================================================
library(data.table)   # fast data processing
library(brms)         # bayesian analyses
library(cogscimodels) # handling cognitive models

# Load fitted cognitive models and participant data
#study <- 2
fits <- readRDS(sub("x", study, "studyx_cognitive_models_fit.rds"))
d <- fread(sub("x", study, "../../data/processed/studyx.csv"), colClasses=list(character="id"))
d <- d[condition=="experience"] # only use experience condition

#
# Influence of sample size on evaluations
# --------------------------------------------------------------------------
d[, c("id", "gambletype", "samplesizecat") := .(factor(id), factor(gambletype), factor(samplesizecat, levels = c("xs", "s", "m", "l", "--")))]
d[, ev := round(gamblex * gamblep, 3)]
d[, evf := factor(ev)]
# Fit the model and obtain Bfs
aovfit <- anovaBF(value ~ id + evf + gambletype * samplesizecat, data = d, whichRandom = c("id", "evf"), iterations = niter)
aovfit <- recompute(aovfit, iterations = 3 * niter)
BF_fe_fess <- extractBF(aovfit[1]/aovfit[3], onlybf = TRUE)
BF_fe_int <- extractBF(aovfit[1]/aovfit[4], onlybf = TRUE)
class(BF_fe_fess) <- class(BF_fe_int) <- "BF"

#
# Influence of gamble type
# --------------------------------------------------------------------------
aovfit <- anovaBF(value ~ id + evf + gambletype, data = d, whichRandom = c("id"), iterations = niter)
aovfit <- recompute(aovfit, iterations = 3 * niter)
BF_gambletype <- extractBF(aovfit[3]/aovfit[2], onlybf = TRUE)
class(BF_gambletype) <- "BF"

#
# Confidence predicted by sample size
# --------------------------------------------------------------------------
aovfit = anovaBF(confidence ~ samplesizecat + id, whichRandom = c("id"), data = d, iterations = niter)
aovfit <- recompute(aovfit, iterations = 3 * niter)
BF_conf = as.character(round(1/extractBF(aovfit, onlybf = T),2))



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
fit0 <- brm(
  formula = value_scaled ~ samplesizecat_num * priorx_cat * gambletype + (1 | id),
  data   = d,
  cores  = 3,
  save_all_pars = TRUE,
  iter = 5000,
  file = sub("x", study, "studyx_bayes_models_fit.rds"))

# Fit model without priorx_cat
null_fit0 <- update(fit0,
  formula = ~ samplesizecat_num * gambletype + (1 | id),
  file = sub("x", study, "studyx_bayes_models_fit_noprior.rds"))  # Same but without the predictor model-and-prior

# Model confidence

# Fit full model
fit1 <- brm(
  formula = confidence ~ samplesizecat + (1 | id),
  data   = d[winner=="bvu"],
  cores  = 3,
  iter = 5000,
  save_all_pars = TRUE,
  file = sub("x", study,"studyx_bayes_models_fit_confidence.rds"))

# Fit model without sample size cat
null_fit1 <- update(fit1,
  formula = ~ .-samplesizecat + (1 | id),
  file = sub("x", study, "studyx_bayes_models_fit0_confidence.rds"))  # Same but without the sample size

# Fit full model
fit2 <- brm(
  formula = confidence ~ samplesizecat + (1 | id),
  data   = d[winner=="rf"],
  cores  = 4,
  iter = 5000,
  save_all_pars = TRUE,
  control = list(adapt_delta = 0.95),
  file = "study1_bayes_models_fit_confidence_rf.rds")

# Fit model without priorx_cat
null_fit2 = update(fit2,
  formula = ~ .-samplesizecat + (1 | id),
  file = "study1_bayes_models_fit0_confidence_rf.rds")  # Same but without the sample size
