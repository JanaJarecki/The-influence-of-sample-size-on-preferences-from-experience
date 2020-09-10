# ==========================================================================
# Utility functions
# ==========================================================================
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