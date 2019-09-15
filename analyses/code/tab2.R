# ==========================================================================
# Table 2 for study 1
# ==========================================================================
library(data.table)
library(BayesFactor)

d <- fread("../../data/processed/study1.csv",
  select = c("id", "condition", "samplesizecat", "gambletype", "value", "gambleid"),
  colClasses = list(factor = c("id", "condition")))
d[, id := as.factor(id)]
d[, samplesizecat := factor(samplesizecat, levels = c("xs", "s", "m", "l", ""), labels = c("xs", "s", "m", "l", "--"))]

M <- d[, .(
  Med = median(value),
  M = mean(value)
  ),
  by = .(condition, gambleid, samplesizecat)][order(gambleid, samplesizecat)]
M[, `D-E` := rep(M[condition == "description"], .N) - c(M[condition == "experience"], NA), by = .(gambleid)]

getbf10 <- function(g, s, niter = 1000) {
  bf10 <- anovaBF(value ~ condition + id,
    whichRandom = "id",
    data = d[gambleid == g & samplesizecat %in% c(s, "--")],
    iterations = niter)
  return(extractBF(bf10, onlybf = T))
}

for (g in unique(M$gambleid)) {
  for (s in unique(M$samplesize)) {
    M[gambleid == g & samplesizecat == s, `D-E BF10` := getbf10(g, s)]
  }
}

setcolorder(M, c(2,1,3:7))
M[, condition := ifelse(condition == "experience", "E", "D")]
M <- split(M, by = "gambleid", keep.by = F)
names(M) <- paste("Gamble ID", names(M))