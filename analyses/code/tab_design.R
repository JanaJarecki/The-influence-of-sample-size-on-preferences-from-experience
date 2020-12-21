# ==========================================================================
# Produces table of design
# Author: Jana B. Jarecki
# ==========================================================================
pacman::p_load(data.table)
pacman::p_load_gh("crsh/papaja@devel")
# Set working directory to this file location

# Load data ----------------------------------------------------
d <- fread("../../data/processed/study1.csv", colClasses = list(factor = c("id", "gambletype", "samplesizecat", "gambleid")))
d[, samplesizecat := factor(samplesizecat, levels = c("xs", "s", "m", "l"))]
d[, gambleev := gamblex * gamblep]

tab <- unique(d[condition != "description", c("gambleid", "gambletype", "gamblex", "gamblep", "gambleev", "samplesizecat", "samplesize")])[order(gambleid)]
tab <- dcast(tab, ... ~ samplesizecat, value.var = "samplesize")

tab <- apa_table(tab,
  col.names = c("Gamble ID", "Type", "X", "Pr", "EV", "xs", "s", "m", "l"),
  align = c("l", "l", rep("c", 7)),
  note = "X = gain in Swiss Fr., Pr = probability of gain, EV = expected value, Sample Size = total number of observations in the experience condition categorized as xs = extra small, s = small, m = medium, l = large. The probability is expressed as the ratio of the relative frequency of the number of gain observations to the number of observations in the smallest sample size category (xs) of this gamble, namely 1/5, 1/6, 1/7, 4/5, 5/6, and 6/7 for gamble IDS 1 through 6, respectively.",
  caption = "Gambles and Sample Sizes Used in Studies 1 and 2 \\label{table:Lotteries}}",
  placement = "H",
  col_spanners = list("Sample Size" = c(6,9)),
  escape = F
  )


saveRDS(list(tab = tab), file = "../../manuscript/tab_design.rds", version = 2, ascii = TRUE)