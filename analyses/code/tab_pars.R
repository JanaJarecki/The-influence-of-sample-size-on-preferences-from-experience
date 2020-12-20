source("setup_fig.R")
options(papaja.na_string = "--")
tab <- dcast(parameter, winner ~ par, fun.aggregate=paste_msd, value.var="val")
tab[, winner_n := paste0(
  factor(winner, model_levels, model_labels),
  " (\\textit{n}$=$",
  ..winners[as.character(winner)], ")"), by=winner]
parnames <- intersect(c("tau", "delta", "count_x","sigma"), colnames(tab))

tab <- papaja::apa_table(tab[names(sort(-winners)), c("winner_n", ..parnames)]
      , caption = paste0("Parameter Estimates of Winning Models, \\textit{M (SD)} \\label{", sub("X", study, "tab:studyX_parameter"),"}")
      , col.names = c("Winning Model", paste0("$\\", parnames, "$"))
      , align = c("l", rep("c", 4)),
      , note = "\\textit{BVU}$=$ Bayesian value updating model, \\textit{RF}$=$ relative frequency model, $n=$ count of participants best-described by a model; parameters denote: $\\tau=$ power utility exponent, $\\delta=$ learning rate, $\\theta_G$ gain prior, $\\sigma$ standard deviation of the PDF."
      , escape = FALSE
      )