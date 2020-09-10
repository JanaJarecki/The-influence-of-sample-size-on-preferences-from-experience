# ==========================================================================
# Preprocesses Raw Data from Study 1
# DO NOT RUN
# ==========================================================================
library(data.table)

load("../../data/raw/data_analysis_B.RData")
sd <- as.data.table(sd)
keep_cols <- c("id", "block", "trial", "SS", "p", "gain", "rating", "gamble", "s", "ptypeprob") 
sd <- sd[, ..keep_cols]
setnames(sd, keep_cols, c("id", "condition", "trial", "samplesize", "gamblep", "gamblex", "value", "gambleid", "samplesizecat", "gambletype"))

co <- as.data.table(conf)
keep_cols <- c("id", "block", "trial", "SS", "p", "gain", "rating", "gamble", "s", "ptypeprob")
co <- co[, ..keep_cols]
setnames(co, c("id", "condition", "trial", "samplesize", "gamblep", "gamblex", "confidence", "gambleid", "samplesizecat", "gambletype"))
co[condition == "description.conf", c("samplesize","samplesizecat") := NA]
co[, gamblep := as.numeric(as.character(gamblep))]
co[, gamblex := as.numeric(as.character(gamblex))]
co[, condition := ifelse(condition == "sampling", "experience", "description")]
co <- co[!gamblep %in% c(0.25,0.75)]

de <- as.data.table(de)
de <- de[, c("id", "block", "trial", "p", "gain", "rating", "gamble", "ptypeprob")]
setnames(de, c("id", "condition", "trial", "gamblep", "gamblex", "value", "gambleid", "gambletype"))
de[, condition := ifelse(condition == "sampling", "experience", "description")]
de[, gamblep := as.numeric(levels(gamblep))[gamblep]]
de[, gamblex := as.numeric(levels(gamblex))[gamblex]]

d <- de[co, on = intersect(names(co), names(de))]
d[, gambletype := ifelse(gambletype == 1, "$-bet", "p-bet")]
d[, samplesizecat := factor(samplesizecat, levels = c("us","s","m","l",NA), labels = c("xs","s","m","l","--"), exclude = NULL)]
setcolorder(d, c("id", "condition", "trial", "gambleid", "gambletype", "gamblep", "gamblex", "samplesizecat", "samplesize", "value", "confidence"))

fwrite(d, "../../data/processed/study2.csv")






# d[condition=="experience", .N, by = id]
# # 2880 observations
# # 3 repetitions
# # 6 gambles
# # 4 sample sizes
# sd[subid > SS]

# d <- as.data.table(de)
# setnames(d, c("id", "block", "condition", "trial", "subtrial", "samplesize", "gamblep", "gamblex", "rating", "subid", "gambleid", "samplesizecat", "gambletype", "gambleev"))

# d <- as.data.table(d)

# d <- fread("../../data/raw/study1.csv", drop = "V1")
# setnames(d, c("id", "block", "condition", "date", "time", "trial", "subtrial", "gambleidid", "samplesize", "gamblep", "gamblex", "ka1", "rating", "ratinghistory", "ka2", "subid", "gambleid", "samplesizecat", "gambledrawcat", "gambletype"))

# # Delete distractors
# d[, gamblep := as.numeric(as.character(gamblep))]
# d[, distractor := as.numeric(gamblep %in% c(0.25,0.75))]
# d <- d[distractor == 0]


# # Clean variables up
# d[, sampleno := ifelse(!grepl("description|sampling.|gamblex", block), 1:.N, NA), by = .(id,trial)]
# d[, gambletype := factor(gambletype, labels = c("$-bet", "p-bet"))]
# d[samplesizecat == "us", samplesizecat := "xs"]
# d[, samplesizecat := factor(samplesizecat, levels = c("xs", "s", "m", "l"))]
# d[, samplesize := as.numeric(as.character(samplesize))]

# d[, gamblex := as.numeric(as.character(gamblex))]
# d[, gamblexpy := paste0(gamblex, " ", sprintf("%.1f", gamblep*100), "%", " ", 0)]


# d[id==1, table(gamblexpy, samplesizecat)]

# d[, gambledraw := ifelse(gambledrawcat=="gain", gamblex, ifelse(gambledrawcat=="loss", 0, NA))]
# d[, datetime := as.POSIXct(paste(date, time), format = "%m.%d.%y %H:%M:%S", tz ="GMT")]
# d[, gambleev := gamblex * gamblep]
# d[condition == "description", c("samplesize", "samplesizecat") := NA]




# #
# # Valuation and Confidence Data
# # --------------------------------------------------------------------------
# columns <- c("datetime", "id", "trial", "gambleid", "gamblex", "gamblep", "gambleev", "gambletype", "condition", "samplesizecat", "samplesize", "rating")
# # Valuation data
# V <- d[grepl("description$|sampling.decision", condition), ..columns]
# V[, rating := as.numeric(gsub(",", ".", rating))]
# V[, condition := factor(condition, labels = c("description", "experience"))]
# setnames(V, "rating", "value")
# # Confidence data
# C <- d[grepl("conf", condition), ..columns]
# C[, rating := as.numeric(gsub(",", ".", rating))]
# C[, condition := factor(condition, labels = c("description", "experience"))]
# setnames(C, "rating", "confidence")
# # Combine V and C
# D <- V[C[, c("id", "trial", "gambleid", "confidence")], on =  c("id", "trial", "gambleid")]
# setcolorder(D, c("datetime", "id", "condition", "samplesizecat", "trial", "gambleid", "gambletype", "gamblep", "gamblex", "gambleev", "samplesize", "value", "confidence"))


# #
# # Sampling History of Outcomes in Wide Format
# # --------------------------------------------------------------------------
# S <- dcast(d[condition == "sampling"], id + trial ~ sampleno, value.var = "gambledraw", sep = "")
# setnames(S, grep("[0-9]", names(S)), paste0("sample", 1:42))

# D <- D[S, on =  c("id", "trial")]
# D <- D[order(id, -condition, trial)]
# D

# fwrite(D, "../../data/processed/study1.csv")