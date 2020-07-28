# ==========================================================================
# Analyze the results of the cogonitive model fiting
# ==========================================================================
library(data.table)

# ==========================================================================
# Uncomment if you wan to RE-FIT the models
# source("2-cognitive-modeling.R")
# ==========================================================================


# Read the data and model fits ---------------------------------------------
# study <- 1 # use 2 for study 2
d <- fread(sub("x", study, "../../data/processed/studyx.csv"))
fits <- readRDS(sub("x", study, "../modelfits/studyx_cognitive_models_fit.rds"))  

## ---- gof ----
# Goodness of model fit -----------------------------------------------------
fits <- dcast(fits, id ~ model, value.var = "fit")
aic <- fits[, lapply(.SD, function(x) AIC(x[[1]])), .SDcols =  c("base", "bvu", "rf"), by = id]
mean_aic <- unlist(aic[, lapply(.SD, mean), .SDcols = -1])
ll <- fits[, lapply(.SD, function(x) logLik(x[[1]])), .SDcols =  c("base", "bvu", "rf"), by = id]
unlist(ll[, lapply(.SD, function(x) mean(-2*x)), .SDcols = -1])

# AIC-weights
weights <- copy(aic)
weights[, c("bvu", "rf", "base") := as.list(cognitiveutils::akaike_weight(c(bvu, rf, base))), by = id]
weights[, winner := names(.SD)[which.max(.SD)], by = id]
weights[, winner := factor(winner, levels = c("bvu", "rf", "base"))]
winners <- sort(table(weights$winner))

# Model parameter -------------------------------------------------------------
fits <- fits[weights[, c("id", "winner")], on = .NATURAL]
fits[, fit_winner := .SD[, which(names(.SD) == winner), with = FALSE], by = .(id, winner)]
parameter <- fits[, .(par = names(coef(fit_winner[[1]])), val = coef(fit_winner[[1]])), by = .(id, winner)]
dcast(parameter, winner ~ par, fun.aggregate = paste_msd)
  
# Prediction data
pred <- fits[, fit_winner[[1]]$predict(), by = id]
setnames(pred, "V1", "pred_scaled")
d[condition == "experience", pred_scaled := ..pred[, pred_scaled]]
d <- weights[, c("id", "winner")][d, on = "id"]
d[, pred := pred_scaled * gamblex]
d[, samplesizecat := factor(samplesizecat, levels = c("xs", "s", "m", "l", "--"), ordered = TRUE)]

r_conf_bym <- d[condition == "experience", cor(as.numeric(samplesizecat), confidence, method = "kendall"), by = .(id, winner)][, dcast(.SD, 1 ~ winner, paste_msd, label = TRUE)]

d <- parameter[par=="count_x", c("id", "val", "winner")][d, on = c("id", "winner")]

## Confidence ratings by best-fitting model
d[condition == "experience", mean(confidence), by = .(id, winner, samplesizecat)][, rank(V1), by = .(winner, id)]
d[condition == "experience",
  .(M = mean(confidence),
    SD = sd(confidence)), by = .(winner, samplesizecat)]


# Analysis of the Bayesian model with delta = 1 -------------------------------
aic_d1 <- fits[, lapply(.SD, function(x) AIC(x[[1]])), .SDcols =  c("base", "bvud1", "rf"), by = id]

# AIC-weights
weights_d1 <- copy(aic_d1)
weights_d1[, c("bvud1", "rf", "base") := as.list(cognitiveutils::akaike_weight(c(bvud1, rf, base))), by = id]
weights_d1[, winner := names(.SD)[which.max(.SD)], by = id] # winning models
weights_d1$winner <- factor(weights_d1$winner, levels = c("bvud1", "rf", "base"))
winners_d1 <- sort(table(weights_d1$winner))