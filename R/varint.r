# Checker si une variable d'intérêt est présente
#  Subset dans une liste des différents modalités de la variable
#  Reprendre le nom de la variable d'intérêt dans un vecteur et ses modalités
#  pour la suite, dans des attributs
#  Supprimer la variable d'intérêt de la description

make_varint <- function(data, varint = NULL) {
  if (!is.null(varint)) {
    n <- nlevels(data[, varint])

    res <- list(NULL)

    for (i in 1:n) {
      res[[i]] <-
        subset(data, data[, varint] == levels(data[, varint])[i])

      res[[i]] <- res[[i]][, !(names(res[[i]]) %in% varint)]
    }

    attr(res, "levels") <- levels(data[, varint])

  } else {
    res <- list(data)

    attr(res, "levels") <- 1
  }

  return(res)
}

# TODO : A revoir
varint_check <- function(varint) {
  if (nlevels(varint) == 0) {
    if (is.logical(unique(varint))) {
      varint <- factor(varint)
    } else {
      stop("Only factor or logical are accepted for variable of interest",
           .call = FALSE)
    }
  }
  # Return a factor
  varint
}
