make_language <- function(data, lang) {
  # Permettre de transformer tous les noms dépendants de la fonction en une
  # autre langue
  #  Idéalement à mettre à la fin de la fonction.
  data <- switch(lang,
    en = data,
    fr = make_french(data)
  )

  return(data)
}

make_french <- function(data) {
 # Translate in french
  return(data)
}
