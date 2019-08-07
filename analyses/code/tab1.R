library(data.table)
# ==========================================================================
# Produces table 1
# ==========================================================================


d <- fread("../../data/processed/study1_values_confidence.csv", colClasses = list(factor = c('id', 'gametype', 'samplesizecat', 'gambleid')))
d[, samplesizecat := factor(samplesizecat, levels = c('xs', 's', 'm', 'l'))]

tab <- unique(d[gametype != 'description', c('gambleid', 'gambletype', 'gamblex', 'gamblep', 'gambleev', 'samplesizecat', 'samplesize')])[order(gambleid)]
tab <- dcast(tab, ... ~ samplesizecat, value.var = 'samplesize')


apa_table(tab,
  col.names = c('Gamble ID', 'Type', 'X', 'Pr', 'EV', 'xs', 's', 'm', 'l')
  , align = c('l', 'l', rep('c', 7))
  , note = '\textit{X} = gain in Swiss Fr., \textit{Pr} = probability of gain, \textit{EV} = expected value, \textit{Sample Size} = total number of observations in the experience condition categorized as \textit{xs} = extra small, \textit{s} = small, \textit{m} = medium, \textit{l} = large. The probability is expressed as the ratio of the relative frequency of the number of gain observations to the number of observations in the smallest sample size category (xs) of this gamble, namely 1/5, 1/6, 1/7, 4/5, 5/6, and 6/7 for gamble IDS 1 through 6, respectively.'
  , caption = 'Gambles and Sample Sizes Used in Studies 1 and 2{table:Lotteries}}'
  , placement = 'H'
  , col_spanners = list('Sample Size' = c(6,9))
  )
