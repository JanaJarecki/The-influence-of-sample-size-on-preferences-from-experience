library(data.table)

d <- fread('../../data/processed/study1.csv', drop = 'V1')
setnames(d, c('id', 'block', 'gametype', 'date', 'time', 'trial', 'subtrial', 'gambleidid', 'samplesize', 'gamblep', 'gamblex', 'ka1', 'rating', 'ratinghistory', 'ka2', 'subid', 'gambleid', 'samplesizecat', 'gambledrawcat', 'gambletype'))

d[, sampleno := ifelse(!grepl('description|sampling.|gamblex', block), 1:.N, NA), by = .(id,trial)]
d[, gambletype := factor(gambletype, labels = c('$-bet', 'p-bet'))]
d[samplesizecat == 'us', samplesizecat := 'xs']
d[, samplesizecat := factor(samplesizecat, levels = c('xs', 's', 'm', 'l'))]
d[, distractor := as.numeric(gamblep %in% c(0.25,0.75))]
d[, gamblexpy := paste0(gamblex, ' ', sprintf('%.1f', gamblep*100), '%', ' ', 0)]
d[, gambledraw := ifelse(gambledrawcat=='gain', gamblex, ifelse(gambledrawcat=='loss', 0, NA))]
d[, datetime := as.POSIXct(paste(date, time), format = "%m.%d.%y %H:%M:%S", tz ="GMT")]
d[, gambleev := gamblex * gamblep]
d[gametype == 'description', c('samplesize', 'samplesizecat') := NA]


# Delete distractors
d <- d[distractor == 0]

columns <- c('datetime', 'id', 'trial', 'gambleid', 'gamblexpy', 'gamblex', 'gamblep', 'gambleev', 'gambletype', 'gametype', 'samplesizecat', 'samplesize', 'rating')
# Valuation data
V <- d[grepl('description$|sampling.decision', gametype), ..columns]
V[, rating := as.numeric(gsub(',', '.', rating))]
V[, gametype := factor(gametype, labels = c('description', 'experience'))]
setnames(V, "rating", "value")

# Confidence data
C <- d[grepl('conf', gametype), ..columns]
C[, rating := as.numeric(gsub(',', '.', rating))]
C[, gametype := factor(gametype, labels = c('description', 'experience'))]
setnames(C, "rating", "confidence")

fwrite(V[C[, c('id', 'trial', 'gambleid', 'confidence')], on =  c('id', 'trial', 'gambleid')], "../../data/processed/study1_values_confidence.csv")



a <- 1