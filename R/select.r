#' Title
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
