#' \code{make.language}
#'
#' Exit language from description table
#' @param data The table to be translated
#' @param lang The language to coerce to, default "en", "fr"
#'
#' @return The same table but translated
#' @export
#'
#' @examples
#' make.language(AirPassengers, "fr")

make.language <- function(data, lang) {
  # Permettre de transformer tous les noms dépendants de la fonction en une autre langue
  #  Idéalement à mettre à la fin de la fonction.
  data <- switch (lang,
    en = data,
    fr = make.french(data)
  )

  return(data)
}

make.french <- function(data) {

  # Translate in french
  #data[""] <- ""

  return(data())
}
