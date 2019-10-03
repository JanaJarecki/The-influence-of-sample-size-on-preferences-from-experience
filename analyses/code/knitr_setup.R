library(knitr)
opts_knit$set(
    base.dir = "C://Users//Jana Jarecki//Dropbox//01-Projects//SAM (w J. Hoffart, G. Duthil, etc)//analyses//figures" # absolute directory under which the plots are generated
   )
opts_chunk$set(
    comment = NA,
    results = "asis",
    warning = FALSE,
    message = FALSE,
    echo = FALSE,
    fig.path = "../figures/",
    knitr.kable.NA = "",
    dev = "cairo_pdf",
    fig.align = "center",
    fig.pos = "htb")

knit_hooks$set(inline = function (x) {
    # Register an inline hook for printing numbers with 3 digits
    if (is.numeric(x)) {
      res <- ifelse(x == ceiling(x) & x == floor(x),
        sprintf("%d", x),
        ifelse(round(x,2) == 0 & round(x,3) != 0,
          sprintf("%1.3f", x),
          sprintf("%1.2f", x)
          )
      )
      paste(res, collapse = ", ")
    } else {
      x
    }
  })