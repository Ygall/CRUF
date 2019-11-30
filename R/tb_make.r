# Permet de sélectionner la méthode de description de tableau
# Numérique : Description par med[IQR] ou moy(SD) +/- (min,max)
# Binaire   : Choisir entre une description 0/1 (all) : Effectif + %
#                                        ou 1 = (inline) : Effectif/total + %
# Catégoriel
#     - Ordonné : Effectif, % des modalités, dans l'ordre des modalités
#     - Non ordonné : Selon la fréquence d'apparition : % + effectif



assign_method <- function(y) {
  if (is.numeric(y))
    return(1)
  if (nlevels(y) == 2)
    return(2)
  if (is.ordered(y) && nlevels(y) > 2)
    return(4)
  if (nlevels(y) > 2)
    return(3)
  if (is.logical(y))
    return(2)
  return(5)
}

make_method <- function(data,
                        default_method = c("cont", "bino", "cate", "ordo")) {
  # assign methods based on type,
  # use method 1 if there is no single
  method <- rep("", length(names(data)))
  names(method) <- names(data)
  for (j in names(data)) {
    y <- data[, j]
    def <- sapply(y, assign_method)
    k <- ifelse(all(diff(def) == 0), k <- def[1], 1)
    method[j] <- default_method[k]
  }
  method
}

make_test <- function(data,
                      default_test = c("stud", "chisq", "chisq", "chisq")) {
  test <- rep("", length(names(data)))
  names(test) <- names(data)
  for (j in names(data)) {
    y <- data[, j]
    def <- sapply(y, assign_method)
    k <- ifelse(all(diff(def) == 0), k <- def[1], 1)
    if (k == 5) {
      test[j] <- ""
    }
    test[j] <- default_test[k]
  }
  test
}

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

make_na <- function(data, explicit_na = FALSE) {
  if (explicit_na != FALSE) {
    method <- make_method(data, default_method = c("0", "1", "1", "1"))

    method <- method[method == 1]

    for (i in names(method)) {
      data[, i] <- addNA(data[, i])
    }
  }

  data
}


make_result <-
  function(data,
           names,
           varint,
           method,
           test,
           explicit_na,
           digits) {
    iter <- length(data)

    # Remove stratifying variable from data
    if (iter != 1) {
      pos_varint <- names(method) == varint
      method <- method[!(pos_varint)]
      test   <- test[!(pos_varint)]
      names  <- names[!(pos_varint)]
    }

    result <- make_first_colum(data, names, method, explicit_na)

    for (i in 1:iter) {
      temp <- data[[i]]

      description <- make_description(temp, method, explicit_na, digits)

      test <- make_table_test(temp, test, explicit_na, digits)

      result <- cbind.data.frame(result, description, test)
    }

  }

make_first_colum <- function(data, names, method, explicit_na) {
  #TODO
  for (i in names) {
    method[i]
  }
}


make_description <- function() {
  #TODO
}

make_table_test <- function() {
  #TODO
}
