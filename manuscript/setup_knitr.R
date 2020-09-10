library(knitr)
print(getwd())
opts_knit$set(
    base.dir = sub("/reports", "/figures", getwd()), # absolute directory under which the plots are generated
    root.dir = normalizePath("../")
   )
cat("\n\n changing knitr$base.dir to ....\n  > ", sub("/reports", "/figures", getwd()), "\n\n")
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
