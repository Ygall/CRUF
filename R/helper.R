#' p-value format
#'
#' Format a p-value into R display system with stars
#'
#' @param pval Numeric.
#'
#' @return "***" if < 0.001, "\*\*" if < 0.01, "\*" if < 0.05, "." if < 0.1
#' @export
#'
#' @examples
#'
#' pval_format(0.00025)
#'
#' pval_format(0.20)
#'
pval_format <- function(pval) {
  res <-
    ifelse(pval < 0.001, "***",
           ifelse(pval < 0.01,  "**",
                  ifelse(pval < 0.05,  "*",
                         ifelse(pval < 0.1,   ".", ""))))
}
pval_format_r <- function(pval) {
  res <-
    ifelse(pval < 0.001, "< 0.001", pval)
}

#' Numeric factor
#'
#' Coerce a factor to a numeric vector
#' @param x Factor to coerce
#'
#' @return A vector as a numeric
#' @export
#'
#' @examples
#'
#' vec <- as.factor(c(8:10))
#'
#' as.numeric(vec) # Return a false value
#' as_numeric_factor # Return the value of numeric vector
#'
as_numeric_factor <- function(x) {
  as.numeric(levels(x))[x]
}

# Formatage --------------------------------------------------------------------

format_pv <- function(p, text = FALSE) {
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

# Selection --------------------------------------------------------------------

#' Name selection
#'
#' @param fichier Le fichier a passer
#'
#' @importFrom utils menu select.list
#' @return Permet de sélectionner les noms dans un vecteur
#' @export
#'
select_noms <- function(fichier) {
  ##
  ## s?lectionner par un menu les noms des variables que l'on souhaite d?crire
  ## fichier : nom de la table R

  hop <- select.list(names(fichier), multiple = 1)
  print(paste("c(\'", paste(hop, collapse = "\',\'"), "\')", sep = ""))
  return(hop)
}

select_quali <- function(vect_var) {
  ##
  ## s?lectionner par un menu les types (quali ou quanti) des variables que l'on
  ##  souhaite d?crire
  ## vect_var  : vecteur des noms de variables concern?s

  j <- 1
  vect_quali <- rep(0, length(vect_var))
  for (i in vect_var) {
    vect_quali[j] <- menu(c("quantitatif", "qualitatif"), TRUE, title = i) -
      1
    j <- j + 1
  }
  print(paste("c(", paste(vect_quali, collapse = ","), ")", sep = ""))
  return(vect_quali)
}

select_tests <- function(vect_var) {
  ##
  ## s?lectionner par un menu les tests ? effectuer sur les variables que l'on
  ## souhaite d?crire
  ## vect_var  : vecteur des noms de variables concern?s
  j <- 1
  test <- rep("", length(vect_var))
  for (i in vect_var) {
    if (vect_var[j] == 1) {
      test[j] <-
        menu(c("fisher", "chisq", "mcnemar", "pas de test"),
             TRUE,
             title = i)
    }
    else {
      test[j] <-
        menu(c("t", "wilcox", "aov", "kruskal", "pas de test"),
             TRUE,
             title = i) + 4
    }
    j <- j + 1
  }
  test[test == "1"] <- "fisher"
  test[test == "2"] <- "chisq"
  test[test == "3"] <- "mcnemar"
  test[test == "4"] <- ""
  test[test == "5"] <- "t"
  test[test == "6"] <- "wilcox"
  test[test == "7"] <- "aov"
  test[test == "8"] <- "kruskal"
  test[test == "9"] <- ""
  print(paste("c(\'", paste(test, collapse = "\',\'"), "\')", sep = ""))
  return(test)
}

