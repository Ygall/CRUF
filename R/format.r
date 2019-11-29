format_pv <- function(p, text = F) {
  if (p < 0.0001)
    return("<0.0001")
  if (p >= 0.0001 &
      p < 0.00095)
    ifelse(text == F, return(sprintf("%.4f", p)),
           return(paste("=", sprintf("%.4f", p), sep =
                                                                 "")))
  if (p >= 0.00095 &
      p <= 0.0095)
    ifelse(text == F, return(as.character(signif(p, 1))),
           return(paste("=", as.character(signif(
      p, 1
    )), sep = "")))
  if (p > 0.0095 &
      p < 0.0995)
    ifelse(text == F, return(sprintf("%.3f", signif(p, 2))),
           return(paste("=", sprintf(
      "%.3f", signif(p, 2)
    ), sep = "")))
  if (p >= 0.0995)
    ifelse(text == F, return(sprintf("%.2f", signif(p, 2))),
           return(paste("=", sprintf(
      "%.2f", signif(p, 2)
    ), sep = "")))
}

##
## arrondir la valeur du hr pour la construction des tableaux de résultats
##
format_hr <- function(z) {
  if (z < 0.05)
    return(sprintf("%.3f", z))
  if (z <= 9.95 & z >= 0.05)
    return(sprintf("%.2f", z))
  if (z > 9.95)
    return(sprintf("%.1f", z))
}
