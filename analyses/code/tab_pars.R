options(papaja.na_string = "--")
tab <- dcast(parameter, winner ~ par, fun.aggregate=paste_msd, value.var="val")
tab[, winner_n := paste0(toupper(winner), " (\\textit{n}$=$", ..winners[as.character(winner)], ")"), by=winner]
tab <- papaja::apa_table(tab[names(sort(-winners)), c("winner_n", "tau", "delta", "count_x","sigma")]
      , caption = "Parameter Estimates of Winning Models, \\textit{M (SD)}"
      , col.names = c("Winning Model","$\\tau$", "$\\delta$", "$\\theta_G$","$\\sigma$")
      , align = c("l", rep("c", 4)),
      , note = "\\textit{BVU}$=$ Bayesian value updating model, \\textit{RF}$=$ relative frequency model, $n=$ count of participants best-described by a model; parameters denote: $\\tau=$ power utility exponent, $\\delta=$ learning rate, $\\theta_G$ gain prior, $\\sigma$ standard deviation of the PDF."
      , escape = FALSE
      )