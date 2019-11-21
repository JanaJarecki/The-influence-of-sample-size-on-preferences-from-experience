library(knitr)

opts_knit$set(
    base.dir = "C://Users//Jana Jarecki//Dropbox//01-Projects//SAM (w J. Hoffart, G. Duthil, etc)//analyses//figures", # absolute directory under which the plots are generated
    root.dir = normalizePath("../")
   )
opts_chunk$set(
  warning = FALSE
  ,error = TRUE
  ,message = FALSE
  ,cache = FALSE
  ,echo = FALSE
  ,comment = NA
  ,fig.path = "../figures/"
  ,cache.path = "./cache/"
  ,knitr.kable.NA = ""
  ,dev = "cairo_pdf"
  ,fig.align = "center"
  ,fig.pos = "htb"
  ,out.width = ".9\\linewidth")

format_bf <- function(x) {
  x <- ifelse(x < 1/1000, "< 1/1000",
      ifelse(x > 1000, "> 1000", 
        ifelse(x >= 1/10 & x <= 10,
          paste("=", sprintf("%1.2f", x)),
          paste("=", sprintf("%1.0f", x))
          )
        )
      )
  return(x)
}

paste_msd <- function(x, digits = 2, na.rm = TRUE, label = FALSE){
  if (all(is.na(x))) {
    return(NA_character_)
  }
  fmt <- paste0("%.", digits, "f")
  if (label == TRUE) {
    label <- c("M=", ", SD=", "")
  } else {
    label <- c("", " (", ")")
  }
  paste0(label[1], sprintf(fmt, round(mean(x, na.rm = na.rm), digits)), label[2], sprintf(fmt, round(sd(x, na.rm = na.rm), digits)), label[3])  
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
