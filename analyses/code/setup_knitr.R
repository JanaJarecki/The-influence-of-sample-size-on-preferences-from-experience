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

format_bf <- function(x) {
  x <- ifelse(x < 1/1000, "< 1/1000",
      ifelse(x > 1000, "> 1000", 
        ifelse(x >= 1/10 & x <= 10,
          sprintf("%1.2f", x),
          sprintf("%1.0f", x)
          )
        )
      )
  return(x)
}

paste_meansd <- function(x, digits = 2, na.rm = TRUE){
  fmt <- paste0("%.", digits, "f")
  paste0(sprintf(fmt, round(mean(x, na.rm = na.rm), digits)), " (", sprintf(fmt, round(sd(x, na.rm = na.rm), digits)), ")")  
}

my_inline_hook <- function (x) {
    # Register an inline hook for printing numbers with 3 digits
    if (class(x) == "BF") {
      x <- format_bf(x)
    } else if (is.numeric(x)) {
      x <- ifelse(x == ceiling(x) & x == floor(x),
            sprintf("%.0f", x),
            sprintf("%1.2f", x)
          )
    }
    return(paste(x, collapse = ", "))
  }

# .inline.hook <- my_inline_hook
knitr::knit_hooks$set(inline = my_inline_hook)
