# ==========================================================================
# Analyze the results of the cogonitive model fiting
# ==========================================================================
# Read data, uncomment if you only run this script
# Read the data
# library(data.table)
# study <- 1
# d <- fread(sub("x", study, "../../data/processed/studyx.csv"))
# fits <- readRDS(sub("x", study, "studyx_cognitive_models_fit.rds"))  

## ---- gof ----
# Goodness of model fit
fits <- dcast(fits, id ~ model)
setnames(fits, "baseline", "base")
gof <- fits[, as.data.table(cbind(model=c("base","rf","bvu"), anova(base[[1]], bvu[[1]], rf[[1]])[, c("wAIC", "AIC", "BIC")])), by = id]

# AIC-weights
weights <- dcast(gof, id ~ model, value.var = "wAIC")
# winning model
weights[, winner := names(.SD)[which.max(.SD)], by = id] # winning models
# Winners of the model comparison by participant
winners <- sort(table(weights$winner))

# Model parameter
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



  # dpred[, relfreq_x := gamblep]
  # dpred[, count_x := round(gamblep * samplesize)]
  # dpred[, count_0 := samplesize - count_x]
  # dpred[, model := ifelse(tolower(winner)=="baseline", "base", tolower(winner))]
  # dpred[, winner := tolower(winner)]
  # dpred <- dpred[winner == model]
  # dpred <- 



d <- parameter[par=="count_x", c("id", "val", "winner")][d, on = c("id", "winner")]

## Confidence ratings by best-fitting model
d[condition == "experience", mean(confidence), by = .(id, winner, samplesizecat)][, rank(V1), by = .(winner, id)]

d[condition == "experience",
  .(M=mean(confidence), SD=sd(confidence)), by = .(winner, samplesizecat)]