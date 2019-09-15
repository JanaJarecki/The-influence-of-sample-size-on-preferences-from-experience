# ==========================================================================
# Preprocesses Raw Data from Study 1
# DO NOT RUN
# ==========================================================================
library(data.table)

d <- fread('../../data/raw/study1.csv', drop = 'V1')
setnames(d, c('id', 'block', 'condition', 'date', 'time', 'trial', 'subtrial', 'gambleidid', 'samplesize', 'gamblep', 'gamblex', 'ka1', 'rating', 'ratinghistory', 'ka2', 'subid', 'gambleid', 'samplesizecat', 'gambledrawcat', 'gambletype'))

d[, sampleno := ifelse(!grepl('description|sampling.|gamblex', block), 1:.N, NA), by = .(id,trial)]
d[, gambletype := factor(gambletype, labels = c('$-bet', 'p-bet'))]
d[samplesizecat == 'us', samplesizecat := 'xs']
d[, samplesizecat := factor(samplesizecat, levels = c('xs', 's', 'm', 'l'))]
d[, distractor := as.numeric(gamblep %in% c(0.25,0.75))]
d[, gamblexpy := paste0(gamblex, ' ', sprintf('%.1f', gamblep*100), '%', ' ', 0)]
d[, gambledraw := ifelse(gambledrawcat=='gain', gamblex, ifelse(gambledrawcat=='loss', 0, NA))]
d[, datetime := as.POSIXct(paste(date, time), format = "%m.%d.%y %H:%M:%S", tz ="GMT")]
d[, gambleev := gamblex * gamblep]
d[condition == 'description', c('samplesize', 'samplesizecat') := NA]

# Delete distractors
d <- d[distractor == 0]


#
# Valuation and Confidence Data
# --------------------------------------------------------------------------
columns <- c('datetime', 'id', 'trial', 'gambleid', 'gamblex', 'gamblep', 'gambleev', 'gambletype', 'condition', 'samplesizecat', 'samplesize', 'rating')
# Valuation data
V <- d[grepl('description$|sampling.decision', condition), ..columns]
V[, rating := as.numeric(gsub(',', '.', rating))]
V[, condition := factor(condition, labels = c('description', 'experience'))]
setnames(V, "rating", "value")
# Confidence data
C <- d[grepl('conf', condition), ..columns]
C[, rating := as.numeric(gsub(',', '.', rating))]
C[, condition := factor(condition, labels = c('description', 'experience'))]
setnames(C, "rating", "confidence")
# Combine V and C
D <- V[C[, c('id', 'trial', 'gambleid', 'confidence')], on =  c('id', 'trial', 'gambleid')]
setcolorder(D, c('datetime', 'id', 'condition', 'samplesizecat', 'trial', 'gambleid', 'gambletype', 'gamblep', 'gamblex', 'gambleev', 'samplesize', 'value', 'confidence'))


#
# Sampling History of Ourcomes in Wide Format
# --------------------------------------------------------------------------
S <- dcast(d[condition=='sampling'], id + trial ~ sampleno, value.var = 'gambledraw', sep = '')
setnames(S, grep('[0-9]', names(S)), paste0('sample', 1:42))

D <- D[S, on =  c('id', 'trial')]
D <- D[order(id, -condition, trial)]
D

fwrite(D, "../../data/processed/study1.csv")