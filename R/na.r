# Choisir de prendre en compte explicitement les NA ou non pour les variables
# catégorielles
# Il faut toujours afficher les NA, mais il faut savoir si on veut les prendre
# en compte dans le calcul de proportion

# En cas de présentation binomiale, que faire ? Gestion directement dans la
# table de description avec NA toujours en dernier ?
# Gestion des NA différentes pour continue/binomial et les autres ?
make_na <- function(data, explicit_na = FALSE) {
  if (explicitNA != FALSE) {
    method <- make_method(data, defaultMethod = c("0", "1", "1", "1"))

    method <- method[method == 1]

    for (i in names(method)) {
      data[, i] <- addNA(data[, i])
    }
  }

  data
}
