# ==========================================================================
# Analyze the results of the cogonitive model fiting
# ==========================================================================
pacman::p_load(data.table)
pacman::p_load_gh("janajarecki/cognitivemodels")

# ==========================================================================
# Uncomment if you wan to RE-FIT the models
# source("2-cognitive-modeling.R")
# ==========================================================================
source("setup_models.R")
source("utilities.R")

# Read the data and model fits ---------------------------------------------
study <- 2 # use 2 for study 2
d <- fread(sub("X", study, "../../data/processed/studyX.csv"))
fits <- readRDS(sub("X", study,"../fittedmodels/studyX_cognitive_models.rds"))
fits <- dcast(fits, id ~ model, value.var = "fit")
hist(fits[, as.list(coef(bvu[[1]])), by = id]$delta)
parspace(fits$bvu[[1]])




# EXPLORE ------------------------------------------------------------------
# Data preprocessing -------------------------------------------------------
d <- d[condition == "experience"]
d[, relfreq_x := gamblep]
d[, count_x := round(gamblep * samplesize)]
d[, count_0 := samplesize - count_x]
# Rescale respondents' values to have a range of 0 - 1
d[, value_scaled := value / gamblex]
d[, value_scaled2 := (value - min(value)) / max(value - min(value)), by = id]

# Model predictions
d[, repetition := 1:.N, by = .(id, gambleid, samplesizecat)]
setkey(d, "id")
fits[id=="s41", logLik(rf[[1]])]

fits[id == "s41", lapply(.SD, function(x) logLik(x[[1]], newdata = d[id][repetition == 3])), .SDcols = c("rf", "bvu")]

pred <- fits[id == "s41", lapply(.SD, function(x) predict(x[[1]], newdata = d[id][repetition == 3])), .SDcols = c("rf", "bvu")]

cognitiveutils::loglikelihood(pred=pred$bvu, obs=d["s41"][repetition==3]$value_scaled, pdf = "truncnormal", sigma = fits[id=="s41"]$bvu[[1]]$coef()["sigma"], a = 0, b = 1)

sum(log(truncnorm::dtruncnorm(d["s41"][repetition==3]$value_scaled, a = 0, b = 1, mean = pred$rf, sd = fits[id=="s41"]$rf[[1]]$coef()["sigma"])))

predict(fits[id=="s41"]$bvu[[1]], newdata = d["s41"], type = "sd")

setkey(d,id)
setkey(fits,id)
d[by= id,,
  c("rf", "bvu", "bvu_sd", "rf_sd") := .(
    predict(fits[id]$rf[[1]], newdata=.SD),
    predict(fits[id]$bvu[[1]], newdata=.SD),
    fits[id]$bvu[[1]]$coef()["sigma"],
    fits[id]$rf[[1]]$coef()["sigma"]
  )]


d[by=id,, .(
  rf = -2*cognitiveutils::loglikelihood(obs=value_scaled, pred=rf, pdf="truncnormal", a=0, b=1, sigma=rf_sd) + 2,
  bvu = -2*cognitiveutils::loglikelihood(obs=value_scaled, pred=bvu, pdf="truncnormal", a=0, b=1, sigma=bvu_sd) + 2
  )][, mean(bvu<rf)]

npar(fits$bvu[[1]], "free")
coef(fits$bvu[[1]])

d[, var(value_scaled), by = .(bvu_sd, id, samplesize)][, round(cor.test(V1, samplesize)$p.value, 4), by=id]



source("setup_fig.R")
d$idf <- factor(d$id, levels = weights[order(rf), id])
d$samplesizecatf <- factor(d$samplesizecat, levels = c("xs", "s", "m", "l", "--"))
d$gambleidf <- factor(d$gambleid, levels = c(2,1,3,5,4,6))

p <- ggplot(melt(d, measure = c("rf","bvu"), value.name = "pred"),
  aes(interaction(samplesizecatf,gambleidf,sep="\n"), pred*gamblex)) +
  geom_bar(stat="summary", fun = mean, aes(y = value, fill=gambleidf)) +
  geom_point(aes(y = value, fill=gambleidf), color = "white", shape = 21) +
  geom_line(stat="summary", position = position_dodge(width=.5), aes(color = variable, group=variable)) +
  scale_y_continuous(expand = c(0,0)) +
  facet_wrap(~idf, scales="free_x") +
  scale_fill_grey() +
  themejj(facet=T)
suppressMessages(ggsave("../figures/_wip3.png", plot=p, h = 8, w = 16))



# ------------------------------------------------------------------------


## ---- gof ----
# Goodness of model fit -----------------------------------------------------

aic <- fits[by=id,,
  lapply(.SD, function(x) BIC(x[[1]])),
  .SDcols =  c("base", "bvu", "rf")]
mean_aic <- unlist(aic[, lapply(.SD, mean), .SDcols = -1])

ll <- fits[by = id ,,
  lapply(.SD, function(x) logLik(x[[1]])),
  .SDcols = c("base", "bvu", "rf"),
  ]
unlist(ll[, lapply(.SD, function(x) mean(-2*x)), .SDcols = -1])
ll[, sum(bvu>rf)]
fits$bvu[[1]]$options$fit_args
fits$rf[[1]]$options$fit_args

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