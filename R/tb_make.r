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
           test_yn,
           explicit_na,
           digits,
           pres_quant,
           pres_quali) {
    iter <- length(data)

    # Remove stratifying variable from data
    if (iter != 1 & !is.null(varint)) {
      pos_varint <- names(method) == varint
      method <- method[!(pos_varint)]
      test   <- test[!(pos_varint)]
      names  <- names[!(pos_varint)]
    }

    result <- NULL

    for (i in names) {
      name <- names[names == i]
      result_first <-
        make_first_column(data, name, method, explicit_na)

      colnames(result_first) <- c("Variable", "Modality")

      result_desc <-
        data.frame(matrix(NA, nrow = nrow(result_first), ncol = 1))

      for (j in 1:iter) {
        temp <- data[[j]]

        description <-
          make_description(temp,
                           name,
                           method,
                           explicit_na,
                           digits,
                           pres_quant,
                           pres_quali)

        result_desc <- cbind.data.frame(result_desc, description)
      }

      result_test <-
        data.frame(matrix(NA, nrow = nrow(result_first), ncol = 1))

      if (test_yn == TRUE) {
        tested <-
          make_table_test(temp, name, method, test, explicit_na, digits)

        result_test <- cbind.data.frame(result_test, tested)
      }

      colnames(result_test) <- "p-value"

      result_tmp <-
        cbind.data.frame(result_first, result_desc, result_test)

      result <-
        rbind.data.frame(result, result_tmp, stringsAsFactors = F)
    }

    result <-
      result [, apply(result, 2, function(x) {
        !any(is.na(x))
      })]

    result
  }

make_first_column <- function(data, name, method, explicit_na) {
  res <- NULL

  exp_na <- as.numeric(explicit_na)

  r <- switch(
    method[name],
    cont = 1 + exp_na,
    bino = 1 + exp_na,
    cate = 1 + exp_na + length(levels(data[[1]][, name])),
    ordo = 1 + exp_na + length(levels(data[[1]][, name]))
  )

  mat <- matrix("", ncol = 2, nrow = r)
  mat[1, 1] <- name

  mat[, 2] <- switch(
    method[name],
    cont = c("",
             if (exp_na == 1)
               "NA"),
    bino = c("", levels(data[[1]][, name][2]),
             if (exp_na == 1)
               "NA"),
    cate = c("", levels(data[[1]][, name]),
             if (exp_na == 1)
               "NA"),
    ordo = c("", levels(data[[1]][, name]),
             if (exp_na == 1)
               "NA")
  )

  res <- data.frame(mat, stringsAsFactors = F)

  res
}

make_description <-
  function(temp,
           name,
           method,
           explicit_na,
           digits,
           pres_quant,
           pres_quali) {
    exp_na <- as.numeric(explicit_na)

    r <- switch(
      method[name],
      cont = 1 + exp_na,
      bino = 1 + exp_na,
      cate = 1 + exp_na + length(levels(temp[, name])),
      ordo = 1 + exp_na + length(levels(temp[, name]))
    )

    mat <- matrix("", ncol = 2, nrow = r)

    mat <- switch(
      method[name],
      cont = make_desc_cont(r, temp, name, digits, pres_quant),
      bino = matrix("bino", nrow = r, ncol = 2),
      cate = matrix("cate", nrow = r, ncol = 2),
      ordo = matrix("ordo", nrow = r, ncol = 2)
    )

    if (exp_na == 1) {
      mat[r, 1] <- sum(is.na(temp[, name]))
    }

    res <- data.frame(mat, stringsAsFactors = F)
    res
  }

make_desc_cont <- function(r, temp, name, digits, pres_quant) {
  mat <- matrix("", nrow = r, ncol = 2)

  vec <- temp[, name]

  res1 <- ""
  res2 <- ""
  res3 <- ""

  if ("mean" %in% pres_quant) {
    res1 <- paste0(round(mean(vec, na.rm = T), digits), " (",
                   round(sd(vec, na.rm = T), digits), ") ")
  }

  if ("med" %in% pres_quant) {
    res2 <- paste0(
      round(median(vec, na.rm = T), digits),
      " [",
      round(quantile(vec, 0.25, na.rm = T), digits),
      ";",
      round(quantile(vec, 0.75, na.rm = T), digits),
      "] "
    )
  }

  if ("range" %in% pres_quant) {
    res3 <- paste0("{", round(min(vec, na.rm = T), digits), ";",
                   round(max(vec, na.rm = T), digits), "}")
  }

  mat[1, 1] <- length(!is.na(vec))
  mat[1, 2] <- paste0(res1, res2, res3)

  return(mat)
}

make_table_test <-
  function(data,
           name,
           method,
           test,
           explicit_na,
           digits) {
    res <- NULL

    exp_na <- as.numeric(explicit_na)

    r <- switch(
      method[name],
      cont = 1 + exp_na,
      bino = 1 + exp_na,
      cate = 1 + exp_na + length(levels(data[, name])),
      ordo = 1 + exp_na + length(levels(data[, name]))
    )

    mat <- matrix("", ncol = 1, nrow = r)

    mat[1, 1] <- switch(
      test[name],
      stud   = "stud",
      fish   = "fisher",
      krusk  = "kruskal",
      chisq  = "chisq",
      wilcox = "wilcox",
    )

    res <- data.frame(mat, stringsAsFactors = F)
    res
  }
