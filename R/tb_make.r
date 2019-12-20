make_auto_detect <- function(data) {
  detect <- NULL
  type   <- NULL


  for (i in colnames(data)) {
    detect <- c(detect, !is.factor(data[, i]))
    lev <- nlevels(factor(data[, i]))

    if (lev == 2) {
      type <- c(type, "bino")
    } else if (lev < 10 & lev > 2) {
      type <- c(type, "cate")
    } else {
      type <- c(type, "unch")
    }
  }

  for (i in seq_along(data)) {
    if (detect[i] & type[i] != "unch") {
      data[, i] <- factor(data[, i])
      message(paste0("\"",
                     names(data)[i],
                     "\" -> ",
                     type[i]),
              "", appendLF = T)
    }
  }

  return(data)
}

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
  if (is.character(y))
    return(3)
  return(5)
}

make_method <- function(data,
                        default_method = c("cont", "bino", "cate", "ordo")) {
  # assign methods based on type,
  # use method 1 if there is no single
  #
  #
  method <- rep("", length(names(data)))
  names(method) <- names(data)
  for (j in names(data)) {
    y <- data[, j]
    def <- assign_method(y)
    if (def == 5) {
      stop(paste0("Argument : ",
                  j,
                  " not in supported variable type"))
    } else {
      method[j] <- default_method[def]
    }
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

make_result <-
  function(data,
           data_c,
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

    for (i in seq_along(names)) {
      name  <- names(data[[1]])[i]
      label <- names[i]

      lev <- levels(data[[1]][, i])

      result_first <-
        make_first_column(lev, label, name, method, explicit_na)

      result_desc <-
        data.frame(matrix(NA, nrow = nrow(result_first), ncol = 1))

      for (j in 1:iter) {
        temp <- data[[j]]

        description <-
          make_description(temp,
                           name,
                           label,
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
          make_table_test(data_c,
                          label,
                          name,
                          method,
                          test,
                          explicit_na,
                          digits,
                          varint)

        result_test <- cbind.data.frame(result_test, tested)
      }

      result_tmp <-
        cbind.data.frame(result_first, result_desc, result_test)

      result <-
        rbind.data.frame(result, result_tmp, stringsAsFactors = F)
    }

    result <-
      result [, apply(result, 2, function(x) {
        !any(is.na(x))
      })]
    result <-
      result [, apply(result, 2, function(x) {
        !all(x == "")
      })]


    lev <- attributes(data)$levels

    n <- sapply(data, function(x) {
      nrow(x)
    })
    names(n) <- lev

    result <- make_first_row(result, lev, n, varint, test_yn)


    result
  }

make_first_column <-
  function(lev, label, name, method, explicit_na) {
    res <- NULL

    exp_na <- as.numeric(explicit_na)

    r <- switch(
      method[name],
      cont = 1 + exp_na,
      bino = 1 + exp_na,
      cate = 1 + exp_na + length(lev),
      ordo = 1 + exp_na + length(lev)
    )

    mat <- matrix("", ncol = 2, nrow = r)
    mat[1, 1] <- label

    mat[, 2] <- switch(
      method[name],
      cont = c("",
               if (exp_na == 1)
                 "NA"),
      bino = c(lev[2],
               if (exp_na == 1)
                 "NA"),
      cate = c("", lev,
               if (exp_na == 1)
                 "NA"),
      ordo = c("", lev,
               if (exp_na == 1)
                 "NA")
    )

    res <- data.frame(mat, stringsAsFactors = F)

    res
  }

make_first_row    <- function(result, lev, n, varint, test_yn) {
  n_result <- dim(result)[2]

  n_varint <- length(lev)
  n_test   <- as.numeric(test_yn)

  exp <- 4 + 2 * (n_varint - 1) + n_test

  varint_name <- as.vector(sapply(lev,
                                  function(x) {
                                    c("", paste0(x, " (N = ", n[x], ")"))
                                  }))
  if (exp == n_result & test_yn == TRUE) {
    colnames(result) <- c("Variable", "Modality",
                          rep(c("N", "Statistics"), n_varint),
                          "p-value")

    first_row <- c("", "",
                   varint_name,
                   "")

  } else if (exp == n_result & test_yn == FALSE) {
    colnames(result) <- c("Variable", "Modality",
                          rep(c("N", "Statistics"), n_varint))

    first_row <- c("", "",
                   varint_name)

  } else if (exp != n_result & test_yn == TRUE) {
    colnames(result) <- c("Variable",
                          rep(c("N", "Statistics"), n_varint),
                          "p-value")

    first_row <- c("",
                   varint_name,
                   "")

  } else if (exp == (n_result + 1) & test_yn == FALSE) {
    colnames(result) <- c("Variable",
                          rep(c("N", "Statistics"), n_varint))

    first_row <- c("",
                   varint_name)
  } else if (exp == (n_result + 2) & test_yn == FALSE) {
    colnames(result) <- c("Variable",
                          rep(c("Statistics"), n_varint))

    first_row <- c(varint_name)
  }

  if (!is.null(varint)) {
    result <- rbind.data.frame(first_row, result, stringsAsFactors = F)
  } else {
    attributes(result)$names[grep("N", colnames(result))] <- paste0("N = ", n)
  }

  result
}

make_description <-
  function(temp,
           name,
           label,
           method,
           explicit_na,
           digits,
           pres_quant,
           pres_quali) {
    exp_na <- as.numeric(explicit_na)

    vec <- temp[, name]

    r <- switch(
      method[name],
      cont = 1 + exp_na,
      bino = 1 + exp_na,
      cate = 1 + exp_na + length(levels(vec)),
      ordo = 1 + exp_na + length(levels(vec))
    )

    mat <- matrix("", ncol = 2, nrow = r)

    mat <- switch(
      method[name],
      cont = make_desc_cont(r, vec, digits, pres_quant),
      bino = make_desc_bino(r, vec, digits, pres_quali),
      cate = make_desc_cate(r, vec, digits, pres_quali),
      ordo = make_desc_ordo(r, vec, digits, pres_quali)
    )

    if (exp_na == 1) {
      mat[r, 1] <- sum(is.na(vec))
    }

    res <- data.frame(mat, stringsAsFactors = F)
    res
  }

make_desc_cont <- function(r, vec, digits, pres_quant) {
  mat <- matrix("", nrow = r, ncol = 2)

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

  if (sum(is.na(vec)) != 0) {
    mat[1, 1] <- sum(!is.na(vec))
  }

  mat[1, 2] <- paste0(res1, res2, res3)

  return(mat)
}

make_desc_bino <- function(r, vec, digits, pres_quali) {
  vec <- factor(vec)
  mat <- matrix("", nrow = r, ncol = 2)

  n <- NULL
  tot <- NULL
  per <- NULL

  if ("n" %in% pres_quali) {
    n <- sum(levels(vec)[2] == vec, na.rm = T)
  }

  if ("total" %in% pres_quali) {
    tot <- paste0("/", sum(!is.na(vec), na.rm = T))
  }

  if ("per" %in% pres_quali) {
    per <-
      paste0(" (",
             round(
               sum(levels(vec)[2] == vec, na.rm = T) / sum(!is.na(vec),
                                                           na.rm = T) * 100,
               digits
             ), "%)")
  }

  if (!("total" %in% pres_quali) && (sum(is.na(vec)) != 0)) {
    mat[1, 1] <- sum(!is.na(vec), na.rm = T)
  }
  mat[1, 2] <- paste0(n, tot, per)

  return(mat)
}

make_desc_cate <- function(r, vec, digits, pres_quali) {
  vec <- factor(vec)
  mat <- matrix("", nrow = r, ncol = 2)

  for (i in seq_len(nlevels(vec))) {
    n <- NULL
    tot <- NULL
    per <- NULL

    if ("n" %in% pres_quali) {
      n <- sum(levels(vec)[i] == vec, na.rm = T)
    }

    if ("total" %in% pres_quali) {
      tot <- paste0("/", sum(!is.na(vec), na.rm = T))
    }
    if ("per" %in% pres_quali) {
      per <-
        paste0(" (",
               round(
                 sum(levels(vec)[i] == vec, na.rm = T) / sum(!is.na(vec),
                                                             na.rm = T) * 100,
                 digits
               ), "%)")
    }

    mat[i + 1, 2] <- (paste0(n, tot, per))
  }

  if (sum(is.na(vec)) != 0) {
    mat[1, 1] <- sum(!is.na(vec), na.rm = T)
  }
  return(mat)
}

make_desc_ordo <- function(r, vec, digits, pres_quali) {
  make_desc_cate(r, vec, digits, pres_quali)
}

make_table_test <-
  function(data_c,
           label,
           name,
           method,
           test,
           explicit_na,
           digits,
           varint) {
    res <- NULL

    exp_na <- as.numeric(explicit_na)

    r <- switch(
      method[label],
      cont = 1 + exp_na,
      bino = 1 + exp_na,
      cate = 1 + exp_na + length(levels(data_c[, label])),
      ordo = 1 + exp_na + length(levels(data_c[, label]))
    )

    mat <- matrix("", ncol = 1, nrow = r)

    mat[1, 1] <- switch(
      test[label],
      stud   = signif(t.test(data_c[, label] ~ data_c[, varint])$p.value,
                      digits),
      wilcox = signif(wilcox.test(data_c[, label] ~ data_c[, varint])$p.value,
                      digits),
      kruskal = signif(kruskal.test(data_c[, label] ~ data_c[, varint])$p.value,
                       digits),
      chisq  = signif(chisq.test(table(data_c[, label],
                                       data_c[, varint]))$p.value, digits),
      fish   = signif(fisher.test(table(data_c[, label],
                                        data_c[, varint]))$p.value, digits)
    )

    res <- data.frame(mat, stringsAsFactors = F)
    res
  }
