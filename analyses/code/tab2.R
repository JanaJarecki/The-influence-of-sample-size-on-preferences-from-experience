# ==========================================================================
# Table 2 for study 1
# ==========================================================================
library(data.table)
library(BayesFactor)
library(brms)

d <- fread("../../data/processed/study1.csv",
  select = c("id", "condition", "samplesizecat", "samplesize", "gambletype", "value", "gambleid"),
  colClasses = list(factor = c("id", "condition"), double = c("samplesize", "gambleid")))
d[condition=="description", samplesizecat := "--"]
# d[, samplesizecat := factor(samplesizecat, levels = c("xs", "s", "m", "l", "--"))]

M <- d[, .(
  Med = median(value),
  M = mean(value)
  ),
  by = .(condition, gambleid, samplesizecat, samplesize)]
M[, `D-E` := rep(M[condition == "description"], .N) - c(M[condition == "experience"], NA), by = .(gambleid)]

get_bf10 <- function(g, s) {
  fit <- brm(value ~ condition + (1|id), 
            data = d[gambleid == g & samplesizecat %in% c(s, "--")])
}

get_bf10 <- function(g, s, niter = 1000) {
  bf10 <- anovaBF(
    formula = value ~ condition + id,
    whichRandom = "id",
    data = as.data.frame(d[gambleid == g & samplesizecat %in% c(s, "--")]),
    iterations = niter)
  return(extractBF(bf10, onlybf = TRUE))
}
# Run get_bf10 by each gamble ID x sample-size
M[condition == "experience", `D-E BF10` := get_bf10(g = gambleid[1], s = samplesizecat[1]), by = .(gambleid, samplesizecat)]

M[, condition := ifelse(condition == "experience", "E", "D")]
M[, gambleid := as.numeric(gambleid)]
M[, samplesize := as.numeric(samplesize)]
M[, samplesizecat := factor(samplesizecat, levels = c("xs", "s","m","l","--"))]
M <- M[order(gambleid, samplesizecat)]
setcolorder(M, c(2,1,3:8))
M <- split(M, by = "gambleid", keep.by = F)
names(M) <- paste("Gamble ID", names(M))
