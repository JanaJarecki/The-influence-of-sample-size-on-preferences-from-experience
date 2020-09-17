tab <- dcast(parameter, winner ~ par, fun.aggregate=paste_msd, value.var="val")
tab[, winner_n := paste0(winner, " (\\textit{n}$=$", ..winners[winner], ")"),
by=winner]
tab <- papaja::apa_table(tab[names(sort(-winners)), c("winner_n", "alpha", "delta", "count_x","sigma")]
      , caption = "Parameter Estimates of Winning Models, \\textit{M (SD)}"
      , col.names = c("Winning Model","$\\alpha$", "$\\delta$", "$\\theta_G$","$\\sigma$")
      , align = c("l", rep("c", 4)),
      , note = "\\textit{BVU}$=$ Bayesian value updating model, \\textit{RF}$=$ relative frequency model. Parameters denote: $\\alpha=$ power utility exponent, $\\theta_G$ gain prior, $\\sigma$ standard deviation."
      , escape = FALSE
      )