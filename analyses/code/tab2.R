library(data.table)
library(papaja)

d <- fread("../../data/processed/study1_values_confidence.csv", colClasses = list(factor = c('id', 'gametype', 'samplesizecat', 'gambleid')))
d[, samplesizecat := factor(samplesizecat, levels = c('xs', 's', 'm', 'l', ''))]

M <- d[, .(
  Med = median(valuation),
  M = mean(valuation)
  ),
  by = .(gametype, problem, samplesizecat)][order(problem, samplesizecat)]
M[, `D-E` := rep(M[gametype == 'description'], .N) - c(M[gametype == 'experience'], NA), by = .(problem)]

getbf10 <- function(p, s, niter = 1000) {
  bf10 <- anovaBF(valuation ~ gametype + id,
    whichRandom = "id",
    data = d[problem == p & samplesizecat %in% c(s, '')],
    iterations = niter)
  return(extractBF(bf10, onlybf = T))
}

for (p in unique(M$problem)) {
  for (s in unique(M$samplesize)) {
    M[problem == p & samplesizecat == s, `D-E BF10` := getbf10(p, s)]
  }
}