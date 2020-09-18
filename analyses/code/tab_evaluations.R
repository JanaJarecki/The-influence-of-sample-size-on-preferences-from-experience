# ==========================================================================
# Table 2 for study 1
# Author: Jana Jarecki
# ==========================================================================
pacman::p_load(data.table, BayesFactor, brms)
pacman::p_load_gh("crsh/papaja@devel")
# Set working directory to this file location


# Load data ----------------------------------------------------------------
study <- 2
d <- fread(sub("X", study, "../../data/processed/studyX.csv"),
  select = c("id", "condition", "samplesizecat", "samplesize", "gambletype", "value", "gambleid"),
  colClasses = list(factor = c("id", "condition"), double = c("samplesize", "gambleid")))
d[condition=="description", samplesizecat := "--"]


# Compute descriptive statistics ----------------------------------------------
# Mean and Median by conditions
M <- d[, .(Mdn = median(value), M = mean(value)),
  by = .(condition, gambleid, samplesizecat, samplesize)]
# Description-experience gap
M[, `D-E` := rep(M[condition == "description"], .N) - c(M[condition == "experience"], NA), by = .(gambleid)]


# Compute inferential statistics ----------------------------------------------
get_bf10 <- function(g, s, niter = 1000) {
  bf10 <- anovaBF(
    formula = value ~ condition + id,
    whichRandom = "id",
    data = as.data.frame(d[gambleid == g & samplesizecat %in% c(s, "--")]),
    iterations = niter)
  return(extractBF(bf10, onlybf = TRUE))
}
set.seed(42)
M[condition == "experience", `D-E BF10` := get_bf10(g = gambleid[1], s = samplesizecat[1]), by = .(gambleid, samplesizecat)]


# Make Table ------------------------------------------------------------------
options(papaja.na_string = "--")
# Cosmetics for the table
M[, condition := ifelse(condition == "experience", "E", "D")]
M[, gambleid := as.numeric(gambleid)]
M[, samplesize := as.numeric(samplesize)]
M[, samplesizecat := factor(samplesizecat, levels = c("xs", "s","m","l","--"))]
M <- M[order(gambleid, samplesizecat)]
setcolorder(M, c(2,1,3:8))
M[, gambleid := paste0(gambleid, rep(c(" (\\$-bet)", " (p-bet)"), each = 15))]
M <- split(M, by = "gambleid", keep.by = F)
names(M) <- paste("Gamble ID", names(M))
# format the Bayes Factor such that BF > 1000 is displayed as > 1000
M <- lapply(M, function(x) cbind(x[, 1:6], replace(round(x[, 7], 0), round(x[, 7], 0) > 1000, ">1000")))

tab <- apa_table(M
    , caption = gsub("X",study,"Valuations of Gambles in Study X
      \\label{tab:studyX_evaluations}")
    , col.names = c("Condition", 'Sample size category', 'Sample size', '\\textit{Mdn}', '\\textit{M}', 'D--E', 'D--E:$BF\\textsubscript{10}$')
    , align = c('l', rep('c', 4), 'r', 'r')
    , digits = c(0,0,0,2,2,2,0)
    , note = '\\textit{M} = mean, \\textit{Mdn} = median, D--E = difference between mean description-based valuations and experience-based valuations, $BF\\textsubscript{10}$ = Bayes Factor quantifying the evidence for a linear model $\\mathrm{M}\\textsubscript{1}$ predicting that valuations differ between description and experience over a linear model $\\mathrm{M}\\textsubscript{0}$ predicting no such differences; both models models contain a by-participant random effect. Gambles IDs 1, 2, and 3 are \\$-bets; Gamble IDs 4, 5, and 6 are p-bets.'
    , escape = FALSE
    )


# Append results for Rtex manuscript ------------------------------------------
R <- readRDS(sub("X", study, "../../manuscript/resultsX.rds"))
R$tab1 <- tab
saveRDS(R, file=sub(  "X", study, "../../manuscript/resultsX.rds"), version = 2, ascii=TRUE)
