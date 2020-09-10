library(data.table)

d <- fread('../../data/processed/study1.csv', drop = 'V1')
setnames(d,
  c('SS', 's', 'gamble', 'p', 'ptype', 'gt', 'resp'),
  c('samplesz', 'sampleszcat', 'gambleid', 'probability', 'gambletype', 'question', 'outcomegl'))
d[, sampleno := ifelse(!grepl('description|sampling.|gain', question), 1:.N, NA), by = .(id,trial)]
d[, gambletype := factor(gambletype, labels = c('$-bet', 'p-bet'))]
d[sampleszcat == 'us', sampleszcat := 'xs']
d[, sampleszcat := factor(sampleszcat, levels = c('xs', 's', 'm', 'l'))]
d[, distractor := as.numeric(probability %in% c(0.25,0.75))]
d[, gamblexpy := paste0(gain, '-', sprintf('%.1f', probability*100), '%-', 0)]
d[, outcome := ifelse(outcomegl=='gain', gain, ifelse(outcomegl=='loss', 0, NA))]

gd <- d[grepl('description', question), c('question', 'id', 'trial', 'sampleszcat', 'gambleid', 'rating')]
gd <- dcast(gd, ... ~ question, value.var = 'rating')
setnames(gd, 5:6, c('value',  'confidence'))
gd[, value := as.numeric(gsub(',', '.', value))]
gd[, block := 'description']
ge <- d[grepl('sampling.', question), c('question', 'id', 'trial', 'sampleszcat', 'gambleid', 'rating')]
ge <- dcast(ge, ... ~ question, value.var = 'rating')
setnames(ge, 5:6, c('value',  'confidence'))
ge[, value := as.numeric(gsub(',', '.', value))]
ge[, block := 'experience']
gval <- rbind(ge, gd)
gval[, c(mean(value)), by = .(block, gambleid, sampleszcat)][order(gambleid, sampleszcat)]

e <- d[block=='Experience']
d <- d[distractor==0]
d <- d[!grepl('description|sampling.|gain', question)]

d[id<2 & gambleid == 1 & sampleszcat == 'xs']

d[id==1, max(trial), by = .(samplesz, gambleid)]


dcast(d[grepl('conf', gt)], id + gambleid ~ gt + trial, value.var = 'rating')

d[gt=='sampling' & trial==1, table(gamble.pgain, gain, samplesz.cat)]



d[block=='sampling' & gt == 'sampling' & trial==1, table(ptype, gamble.id)]
d[, table(block, gt)]


d[block=='description', median(as.numeric(gsub(',', '.', rating))), keyby = gamble]

drating <- d[grepl('description$|sampling.decision', gt)

d[grepl('description$|sampling.decision', gt), median(as.numeric(gsub(',', '.', rating))), key = .(gamble, block, SS)][order(gamble,SS)]
d[block=='sampling']

dcast(d, id ~ block + trial, value.var = 'rating')

