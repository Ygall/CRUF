# TABKRIS 2 --------------------------------------------------------------------

#' Data description function
#'
#' \code{tabkris_2} computes descriptive statistics for data
#'
#' @details The \code{tabkris_2} function is a function to describe a set of
#'   data. Main purpose is to create a typical table one in biomedical
#'   litterature, either a patient characteristic table or population
#'   characteristic table.
#'
#'   \code{names} is a vector to name the variable of data. Default will use the
#'   colnames of data.
#'
#'   \code{varint} is a variable to stratify the analysis. It must be included
#'   in the initial dataset. It will not be displayed in the final table if
#'   chosen as the stratifying variable
#'
#'   \code{lang} is useful to choose the language for the final display. The
#'   default is english. French is also supported.
#'
#'   \code{default_method} and \code{method} are used to set the methods used
#'   for display. default_method must be length 4, to set the default method for
#'   continuous, binomial, categorical and ordered variable. \code{method} must
#'   be length of data columns, used to fine-tune every method for each
#'   variable.
#'
#'   \code{default_test} and \code{test} are used to set the tests performed.
#'   default_test must be lenght 4, to set the default method for continuous,
#'   binomial, categorical and ordered variable. \code{test} must be length of
#'   data columns, used to fine-tune every test for each variable.
#'
#'   \code{pres_quant} is used to set the display of quantitative variable.
#'   \code{mean (SD)}, \code{median [IQR]} and \code{range} are available,
#'   default is \code{median}.
#'
#'   \code{pres_quali} is used to set the display of qualitative variable.
#'   \code{"n"} for number, \code{"total"} to add "/ total" and \code{"per"} for
#'   percentages, default is \code{"n / per"}.
#'
#'   \code{explicit_na} is used to display.
#'
#'   \code{digits} is the number of digits to display for numbers. Usually if
#'   \code{n < 100}, \code{digits = 0} if \code{100< n < 200}, \code{digits = 1}
#'   else \code{digits = 2}.
#'
#'   \code{return_table} choose if the user wants to directly display a table or
#'   if the user wants to get an object with parametrable objects.
#'
#'   \code{auto_detect} will test if each column can be coerced to a factor
#'   (i.e. having between 2 and 10 levels) and change the type of variable if
#'   so.
#'
#'   \code{lev_co} will set the number of maximum levels to coerce a column in a
#'   factor
#'
#' @param data Dataframe to describe or a "desctable" object
#' @param names Vectors of variables to display in the final table, length of
#'   \code{ncol(data)}
#' @param varint Variable to stratify on, factor only
#' @param lang Language to display, default \code{"en"}, \code{"fr"}
#' @param method Vectors of variables to customize the methods used for
#'   description, length of data columns
#' @param test Either a logical indicating statistical tests execution or a
#'   vectors of variables to customize the tests, length of data columns.
#'   Default \code{FALSE}
#' @param pres_quant Descriptive statistics for quantitative variables. Possible
#'   values are \code{"mean"} for mean, SD, \code{"med"} for median, IQR,
#'   \code{"range"} for range
#' @param pres_quali Descriptive statistics for qualitative variables. Possible
#'   values are \code{"n"} for number, \code{"total"} to add "/ total" and
#'   \code{"per"} for percentages
#' @param default_method Default method to compute the table for each variable.
#'   Default \code{default_method = c("cont", "bino", "cate", "ordo")}
#' @param default_test Default test to apply for each variable type. Default
#'   \code{c("stud", "chisq", "chisq", "chisq")}. Available \code{"stud",
#'   "wilcox", "kruskal", "chisq", "fish"}
#' @param explicit_na Whether to display NA in description, Default \code{FALSE}
#' @param digits Number of significant number to display, default \code{2}
#' @param return_table Whether to return a dataframe or an object to customize
#'   option easily, default \code{TRUE}
#' @param auto_detect Whether to automatically detect variable type,
#'   transforming to factors numeric variable with moderate levels (< 10),
#'   default \code{TRUE}. Possible to set the cut-off number with \code{lev_co}
#' @param lev_co Numeric. When auto_detect is \code{TRUE}, set the number of
#'   level to cutoff for categorical variables
#' @param verbose Logical. Display information about transformation of
#'   variables. default \code{FALSE}
#'
#' @importFrom stats sd median quantile chisq.test fisher.test kruskal.test
#'   t.test wilcox.test
#'
#' @return Depending on argument \code{return_table}, an object of class
#'   data.frame, which is the descriptive table or an object of class
#'   \code{"desctable"}, which is a customizable object.
#'
#' @author Yves Gallien \email{yves.gallien@@gmail.com}, 2019
#'
#' @seealso \url{https://github.com/Ygall/CRUF} for manual and examples.
#'
#' @concept clinical research
#' @export


tabkris_2 <- function(data,
                      names = NULL,
                      varint = NULL,
                      lang = "en",
                      method = NULL,
                      test = FALSE,
                      pres_quant = c("med"),
                      pres_quali = c("n", "per"),
                      default_method = c("cont", "bino", "cate", "ordo"),
                      default_test   = c("stud", "chisq", "chisq", "chisq"),
                      explicit_na = FALSE,
                      digits = 2,
                      return_table = TRUE,
                      auto_detect = TRUE,
                      lev_co = 10,
                      verbose = FALSE) {
  # Logical junction

  if ("desctable" %in% attributes(data)$class) {
    # Make argument in environment
    env <- environment()
    data  <- check_data(data, env)
  }

  if (auto_detect == TRUE) {
    # Make auto_detect
    data <- make_auto_detect(data, lev_co, verbose)
  }

  check_args(lang,
             pres_quant,
             pres_quali,
             default_method,
             default_test,
             explicit_na,
             digits,
             return_table,
             auto_detect)

  names <- check_names(data, names)

  check_varint(data, varint)

  method <- check_default_method(data, method, default_method)

  if (!is.null(method)) {
    method <- check_method(data, method, names)
  } else {
    method <- make_method(data, default_method)
    method <- check_method(data, method, names)
  }

  if (test == TRUE) {
    default_test <- check_default_test(data, varint, default_test)
  }

  test_yn <- check_test_yn(data, test, varint)

  if (test_yn == TRUE) {
    test <- make_test(data, default_test)
    check_test(data, test, default_test, varint, method)
  }

  if (return_table == TRUE) {
    # Transform the data in list to iterate
    data_c <- data
    data <- make_varint(data, varint)
    # Description
    result <-
      make_result(
        data,
        data_c,
        names,
        varint,
        method,
        test,
        test_yn,
        explicit_na,
        digits,
        pres_quant,
        pres_quali
      )

    # Translate
    result <- make_language(result, lang)

  } else {
    result <- list(
      data = data,
      names = names,
      varint = varint,
      lang = lang,
      method = method,
      test = test,
      pres_quant = pres_quant,
      pres_quali = pres_quali,
      default_method = default_method,
      default_test   = default_test,
      explicit_na = explicit_na,
      digits = digits
    )
    attr(result, "class") <- "desctable"
  }

  return(result)
}

# Check ------------------------------------------------------------------------

check_data <- function(data, env) {
  if (is.null(data)) {
    stop("Data not provided", call. = FALSE)
  }

  if ("desctable" %in% attributes(data)$class) {
    list2env(data, env)
    data <- data$data
  } else {
    if (!(is.matrix(data) || is.data.frame(data)))
      stop("Data should be a matrix or data frame", call. = FALSE)
    data <- as.data.frame(data)
  }

  dup <- duplicated(colnames(data))
  if (any(dup))
    stop("Duplicate names found: ",
         paste(colnames(data)[dup], collapse = ", "),
         call. = F)

  data
}

check_names <- function(data, names) {
  if (!is.null(names)) {
    if (!is.vector(names)) {
      stop("Argument names not a vector", call. = FALSE)
    }

    if (length(names) != dim(data)[2]) {
      stop(paste0("Argument names must be length of data columns : ",
                  dim(data)[2]),
           call. = FALSE)
    }
  } else {
    names <- colnames(data)
  }

  names
}

check_varint <- function(data, varint) {
  if (!is.null(varint)) {
    if (!(varint %in% colnames(data))) {
      stop("Argument varint not in data", call. = F)
    }

    if (!is.character(varint)) {
      stop("Argument varint not a character", call. = F)
    }

    if (!is.factor(data[, varint])) {
      stop("Argument varint must refer to a factor variable in data",
           call. = F)
    }
  }
}

check_args <- function(lang,
                       pres_quant,
                       pres_quali,
                       default_method,
                       default_test,
                       explicit_na,
                       digits,
                       return_table,
                       auto_detect) {
  # Check le format de la langue
  lang <- match.arg(lang, c("english", "french"))
  if (!(lang %in% c("english", "french"))) {
    stop("Argument lang error.
             Supported language are \"english\", \"french\"",
         call. = FALSE)
  }

  # Check le format de la méthode de présentation des quantitatifs
  if (!all(sapply(pres_quant, function(x)
    x %in% c("mean",
             "med", "range")))) {
    stop("Argument pres_quant not a correct value", call. = FALSE)
  }

  # Check le format de la méthode de présentation des qualitatifs
  if (!all(sapply(pres_quali, function(x)
    x %in% c("n", "total", "per")))) {
    stop("Argument pres_quali not a correct value", call. = FALSE)
  }

  # Check default method
  if (!is.vector(default_method)) {
    stop("Argument default_method not a vector", call. = F)
  } else if (length(default_method) != 4) {
    stop("Argument default_method must be length 4", call. = F)
  }
  # Check default test
  if (!is.vector(default_test)) {
    stop("Argument default_test not a vector", call. = F)
  } else if (length(default_test) != 4) {
    stop("Argument default_test must be length 4", call. = F)
  }

  # Check le format de explicit_na
  if (!is.logical(explicit_na)) {
    stop("Argument explicit_na not logical", call. = FALSE)
  }

  # Check le format de digits
  if (!is.numeric(digits)) {
    stop("Argument digits not numeric", call. = FALSE)
  } else if (digits < 0 || digits > 15) {
    stop("Argument digits not between 0 and 15", call. = FALSE)
  }

  # Check le format de return_table
  if (!is.logical(return_table)) {
    stop("Argument explicit_na not logical", call. = FALSE)
  }

  # Check le format de auto_detect
  if (!is.logical(auto_detect)) {
    stop("Argument explicit_na not logical", call. = FALSE)
  }
}

check_default_method <- function(data, method, default_method) {
  imp_method  <- c("cont", "bino", "cate", "ordo")

  if (!all(default_method == imp_method)) {
    method <- NULL
  }

  for (i in seq_along(default_method)) {
    if (!(default_method[i] %in% c("cont", "bino", "cate", "ordo"))) {
      stop(paste0(
        "Argument method : ",
        default_method[i],
        " not in supported default_methods"
      ),
      call. = FALSE)
    }
  }

  return(method)
}

check_method <- function(data, method, names) {
  if (!is.vector(method)) {
    stop("Argument method not a vector", call. = F)
  } else if (length(method) != dim(data)[2]) {
    stop("Argument method must be length of data columns", call. = FALSE)
  }

  for (i in seq_along(method)) {
    if (!(method[i] %in% c("cont", "bino", "cate", "ordo"))) {
      stop(paste0(
        "Argument method : ",
        method[i],
        " not in supported methods"
      ),
      call. = FALSE)
    }

    d <- switch(assign_method(data[, i]),
                "1" = "cont",
                "2" = c("bino", "cate", "ordo"),
                "3" = c("cate", "ordo"),
                "4" = c("ordo", "cate"))

    if (!(method[i] %in% d)) {
      stop(paste0(
        "Method\" ",
        method[i],
        "\" not supported for variable \"",
        colnames(data)[i],
        "\". Method should be in : ",
        paste0(d, collapse = " "), "\n"
      ),
      call. = FALSE)
    }
  }

  return(method)

}

check_test_yn <- function(data, test, varint) {
  if (test[1] == FALSE) {
    test_yn <- FALSE
    return(test_yn)
  }

  if (test[1] == TRUE & is.null(varint)) {
    test_yn <- FALSE
    warning("Argument test true but varint null, tests not executed",
            call. = FALSE)
    return(test_yn)
  } else if (test[1] == TRUE) {
    test_yn <- TRUE
    return(test_yn)
  }

  if (!is.vector(test)) {
    stop("Argument test not a vector", call. = F)
  } else if (length(test) != dim(data)[2]) {
    stop("Argument test must be length of data columns", call. = FALSE)
  }

  for (i in seq_along(test)) {
    if (!(test[i] %in% c("stud", "chisq", "fisher", "kruskal", "wilcox"))) {
      stop(paste0("Argument test : ",
                  test[i],
                  " not in supported tests"),
           call. = FALSE)
    }
  }

  test_yn <- TRUE

  return(test_yn)
}

check_default_test <- function(data, varint, default_test) {
  nlev <- length(levels(data[, varint]))

  if (default_test[1] == "stud" && nlev > 2) {
    default_test[1] <- "kruskal"
    warning("Argument varint is more than 2 levels.
Default test for continuous variables set to \"kruskal\"",
            call. = FALSE)
  }

  if (default_test[4] == "chisq" && nlev > 2) {
    default_test[4] <- "kruskal"
    warning("Argument varint is more than 2 levels.
Default test for ordinal variables set to \"kruskal\"",
            call. = FALSE)
  }

  return(default_test)
}

check_test <- function(data, test, default_test, varint, method) {
  nlev <- length(levels(data[, varint]))

  for (i in seq_along(test)) {
    met <- method[i]
    tes <- test[i]

    ad <- if (nlev > 2) {
      switch(
        met,
        cont = c("kruskal"),
        bino = c("chisq", "fish"),
        cate = c("chisq", "fish"),
        ordo = c("kruskal")
      )} else {
        switch(
          met,
          cont = c("stud", "wilcox"),
          bino = c("chisq", "fish"),
          cate = c("chisq", "fish"),
          ordo = c("chisq", "fish")
        )}

    if (!(tes %in% ad)) {
      stop(paste0(
        "Test \"",
        tes,
        "\" not supported for variable \"",
        names(tes),
        "\" with method \"",
        met,
        "\" and with levels of varint ",
        nlev,
        "\". Test should be in : ",
        ad
      ),
      call. = FALSE)
    }
  }
}

# Make -------------------------------------------------------------------------

make_auto_detect <- function(data, lev_co, verbose) {
  detect <- NULL
  type   <- NULL


  for (i in colnames(data)) {
    detect <- c(detect, !is.factor(data[, i]))
    lev <- nlevels(factor(data[, i]))

    if (lev == 2) {
      type <- c(type, "bino")
    } else if (lev < lev_co & lev > 2) {
      type <- c(type, "cate")
    } else {
      type <- c(type, "unch")
    }
  }

  for (i in seq_along(data)) {
    if (detect[i] & type[i] != "unch") {
      data[, i] <- factor(data[, i])
      if (verbose == TRUE) {
        message(paste0("\"",
                       names(data)[i],
                       "\" -> ",
                       type[i]),
                "", appendLF = T)
      }
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

      result_tmp <-
        make_first_column(lev, label, name, method, explicit_na)

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

        result_tmp <- cbind.data.frame(result_tmp, description)
      }

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

        result_tmp <- cbind.data.frame(result_tmp, tested)
      }

      result <-
        rbind.data.frame(result, result_tmp, stringsAsFactors = F)

      rm(description, result_tmp, temp)
    }

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
                                    c(paste0("(N = ", n[x], ")"), x)
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
  mat <- matrix("", nrow = r, ncol = 2)

  for (i in seq_len(nlevels(vec))) {
    n <- NULL
    tot <- NULL
    per <- NULL

    if ("n" %in% pres_quali) {
      n <- sum(levels(vec)[i] == vec, na.rm = T)

      if (n == 0) {
        next ()
      }
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
      method[name],
      cont = 1 + exp_na,
      bino = 1 + exp_na,
      cate = 1 + exp_na + length(levels(data_c[, label])),
      ordo = 1 + exp_na + length(levels(data_c[, label]))
    )

    mat <- matrix("", ncol = 1, nrow = r)

    mat[1, 1] <- switch(
      test[name],
      stud   = signif(t.test(data_c[, name] ~ data_c[, varint])$p.value,
                      digits),
      wilcox = signif(wilcox.test(data_c[, name] ~ data_c[, varint])$p.value,
                      digits),
      kruskal = signif(kruskal.test(data_c[, name] ~ data_c[, varint])$p.value,
                       digits),
      chisq  = signif(chisq.test(table(data_c[, name],
                                       data_c[, varint]))$p.value, digits),
      fish   = signif(fisher.test(table(data_c[, name],
                                        data_c[, varint]))$p.value, digits)
    )

    res <- data.frame(mat, stringsAsFactors = F)
    res
  }

# Language post-processing -----------------------------------------------------
make_language <- function(data, lang) {
  # Permettre de transformer tous les noms dépendants de la fonction en une
  # autre langue
  #  Idéalement à mettre à la fin de la fonction.
  data <- switch(lang,
                 en = data,
                 fr = make_french(data))

  return(data)
}

make_french <- function(data) {
  colonnes <- colnames(data)

  colonnes[colonnes == "Variable"] <- "Variable"
  colonnes[colonnes == "Modality"] <- "Modalit\u00E9"
  colonnes[colonnes == "Statistics"] <- "Statistiques"

  colnames(data) <- colonnes

  return(data)
}

