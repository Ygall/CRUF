#' Univariate Logistic Regression
#'
#' A function used to generate multiple result table for univariate logistic
#' regression model with \code{y ~ x}. For each specified \code{y_names}, a
#' result table is computed, including all \code{x_names} variables.
#'
#' @param data A dataframe including all the variables needed in all the models
#' @param y_names Vector. Name(s) of response variable(s)
#' @param x_names Vector. Name(s) of predictor variable(s)
#' @param twobytwo Logical. Either to include the two by two table for each
#'   variable. Default is \code{TRUE}.
#' @param formula Formula for logistic regression to customize. Default is
#'   \code{(y ~ x)}.
#' @param collapse \code{"NULL"}, \code{"OR"}, \code{"CI"}. Collapse columns in
#'   one column. \code{"OR"} collapses OR, Upper and Lower CI. \code{"CI"}
#'   collapses Upper and Lower CI.
#' @param ref_label Character. Set the label for reference estimate.
#' @param digits Numeric. Number of digits to display.
#'
#' @return The returned value is a list of length \code{y_names}, which consists
#'   of a dataframe having the univariate logistic regressions of the
#'   \code{x_names}.
#'
#' @importFrom stats as.formula glm confint coef anova
#'
#' @export

logistic_univariate <- function(data, y_names, x_names,
                                twobytwo = TRUE, formula = "(y ~ x)",
                                collapse = FALSE, ref_label = "1",
                                digits = 2) {
  y <- y_names
  x <- x_names

  check_args_log(data, y, x, twobytwo, formula, collapse, ref_label, digits)

  dep <- y
  col <- x

  res_uni <- list()

  for (j in seq_along(dep)) {
    res <- data.frame()

    for (i in seq_along(col)) {
      x <- col[i]
      y <- dep[j]

      res_one <- data.frame(glm_univar(y, x, data, twobytwo, formula,
                                       digits, ref_label),
                            check.names = F, stringsAsFactors = F)

      res_one <- collapse_table(res_one, collapse)

      res <- rbind.data.frame(res,
                              res_one,
                              stringsAsFactors = F)
    }
    res_uni[[j]] <- res
  }

  if (length(y_names) == 1) {
    res_uni <- data.frame(res_uni, check.names = F)
  }

  res_uni
}

#' Univariate Logistic Regression with cluster
#'
#' A function used to generate multiple result table for univariate logistic
#' regression model with \code{y ~ x} using a cluster variable. For each
#' specified \code{y_names}, a result table is computed, including all
#' \code{x_names} variables. Compute robust variance using sandwich
#'
#' @param data A dataframe including all the variables needed in all the models
#' @param y_names Vector. Name(s) of response variable(s)
#' @param x_names Vector. Name(s) of predictor variable(s)
#' @param twobytwo Logical. Either to include the two by two table for each
#'   variable. Default is \code{TRUE}.
#' @param formula Formula for logistic regression to customize. Default is
#'   \code{(y ~ x)}.
#' @param collapse \code{"NULL"}, \code{"OR"}, \code{"CI"}. Collapse columns in
#'   one column. \code{"OR"} collapses OR, Upper and Lower CI. \code{"CI"}
#'   collapses Upper and Lower CI.
#' @param ref_label Character. Set the label for reference estimate.
#' @param digits Numeric. Number of digits to display.
#' @param cluster Character. Name of the clustering variable.
#'
#' @return The returned value is a list of length \code{y_names}, which consists
#'   of a dataframe having the univariate logistic regressions of the
#'   \code{x_names}.
#'
#' @importFrom stats as.formula glm confint coef anova
#' @importFrom miceadds glm.cluster
#' @importFrom utils capture.output
#' @importFrom aod wald.test
#'
#' @export
logistic_cluster_univariate <- function(data, y_names, x_names, cluster,
                                        twobytwo = TRUE, formula = "(y ~ x)",
                                        collapse = FALSE, ref_label = "1",
                                        digits = 2) {
  y <- y_names
  x <- x_names

  check_args_log(data, y, x, twobytwo, formula, collapse, ref_label, digits)

  dep <- y
  col <- x

  res_uni <- list()

  for (j in seq_along(dep)) {
    res <- data.frame()

    for (i in seq_along(col)) {
      x <- col[i]
      y <- dep[j]

      res_one <- data.frame(glm_cluster_univar(y, x, data, twobytwo, formula,
                                               digits, ref_label, cluster),
                            check.names = F, stringsAsFactors = F)

      res_one <- collapse_table(res_one, collapse)

      res <- rbind.data.frame(res,
                              res_one,
                              stringsAsFactors = F)
    }
    res_uni[[j]] <- res
  }

  if (length(y_names) == 1) {
    res_uni <- data.frame(res_uni, check.names = F)
  }

  res_uni
}

check_args_log <- function(data, y, x, twobytwo, formula, collapse, ref_label,
                           digits) {
  if (!("data.frame" %in% attributes(data)$class)) {
    stop("Data should be a data frame", call. = FALSE)
  }

  x_error <- NULL
  for (i in seq_along(x)) {
    if (!(x[i] %in% colnames(data))) {
      x_error <- c(x_error, x[i])
    }
  }
  if (!is.null(x_error)) {
    stop(paste0("Arg for x not in data : ",
                paste(x_error, collapse = ", "),
                collapse = ""), call. = FALSE)
  }

  y_error <- NULL
  for (i in seq_along(y)) {
    if (!(y[i] %in% colnames(data))) {
      y_error <- c(y_error, y[i])
    }
  }
  if (!is.null(y_error)) {
    stop(paste0("Arg for y not in data : ",
                paste(y_error, collapse = ", "),
                collapse = ""), call. = FALSE)
  }


  if (any(x %in% y) | any(x %in% y)) {
    stop("A common variable has been found for x and y.
         All x and y must differ.", call. = FALSE)
  }

  if (!is.logical(twobytwo)) {
    stop("Arg twobytwo must be logical", call. = FALSE)
  }


  if (!is.character(formula)) {
    stop("Arg formula must be specified as character")
  } else if (!grepl("y", formula)) { # Modif possible du regexp
    stop("Arg formula must include a y character")
  } else if (!grepl("x", formula)) {
    stop("Arg formula must include a x character")
  }

  if (!(collapse %in% c("OR", "CI", FALSE))) {
    stop("Arg collapse must be in \"OR\", \"CI\" or NULL", call. = FALSE)
  }

  if (!is.character(ref_label)) {
    stop("Arg ref_label must be character", call. = FALSE)
  }

  if (!is.numeric(digits)) {
    stop("Arg digits must be numeric", call. = FALSE)
  }
}
glm_univar             <- function(y, x, data, twobytwo, formula, digits,
                                   ref_label) {

  formula <- sub("y", y, formula)
  formula <- sub("x", x, formula)
  formula <- as.formula(formula)

  nlev <- length(levels(data[, x]))

  fit <- glm(formula, data = data, family = "binomial")

  res <- matrix("", ncol = 9, nrow = nlev)

  res[1, 1] <- x
  res[, 2]  <- fit$xlevels[[1]]
  res[, 3]  <- table(fit$model[, c(x, y)])[, 1]
  res[, 4]  <- table(fit$model[, c(x, y)])[, 2]

  if (!any(table(fit$model[, c(x, y)]) %in% 0)) {
    res[1, 5] <- ref_label
    res[2:nlev, 5] <- round(exp(fit$coefficients), digits)[2:nlev]
    res[2:nlev, 6:7] <- suppressMessages(round(exp(confint(fit)),
                                               digits)[2:nlev, ])

    if (nlev > 2) {
      res[1, 8] <- paste0("Global: ",
                          pval_format_r(signif(anova(fit,
                                                     test = "Chisq")[2, 5], 2)))
      res[1:nlev, 9] <- pval_format(anova(fit, test = "Chisq")[2, 5])
    }
    res[2:nlev, 8] <- pval_format_r(signif(coef(summary(fit))[2:nlev, 4], 2))
    res[2:nlev, 9] <- pval_format(coef(summary(fit))[2:nlev, 4])
  }

  colnames(res) <- c(y, "Modality", levels(data[, y])[1], levels(data[, y])[2],
                     "OR", "CI Lower", "CI Upper", "p-value", "Sign")

  if (!twobytwo) {
    res <- res[, -3:-4]
  }

  res

}
glm_cluster_univar     <- function(y, x, data, twobytwo, formula, digits,
                                   ref_label, cluster) {

  formula <- sub("y", y, formula)
  formula <- sub("x", x, formula)
  formula <- as.formula(formula)

  nlev <- length(levels(data[, x]))

  fitcl <- glm.cluster(formula, data = data, family = "binomial",
                       cluster = cluster)

  fit <- fitcl$glm_res

  res <- matrix("", ncol = 9, nrow = nlev)

  res[1, 1] <- x
  res[, 2]  <- fit$xlevels[[1]]
  res[, 3]  <- table(fit$model[, c(x, y)])[, 1]
  res[, 4]  <- table(fit$model[, c(x, y)])[, 2]

  if (!any(table(fit$model[, c(x, y)]) %in% 0)) {
    res[1, 5] <- ref_label
    res[2:nlev, 5] <- round(exp(fit$coefficients), digits)[2:nlev]
    res[2:nlev, 6:7] <- suppressMessages(round(exp(confint(fitcl)),
                                               digits)[2:nlev, ])

    if (nlev > 2) {
      res[1, 8] <- paste0("Global: ",
                          pval_format_r(signif(wald.test(fitcl$vcov,
                                                         fit$coefficients,
                                                         2:nlev)$result$chi2[3],
                                               2)))
      res[1:nlev, 9] <- pval_format(anova(fit, test = "Chisq")[2, 5])
    }

    inut <- capture.output(pval <- summary(fitcl))
    rm(inut)

    res[2:nlev, 8] <- pval_format_r(signif(pval[2:nlev, 4], 2))
    res[2:nlev, 9] <- pval_format(pval[2:nlev, 4])
  }

  colnames(res) <- c(y, "Modality", levels(data[, y])[1], levels(data[, y])[2],
                     "OR", "CI Lower", "CI Upper", "p-value", "Sign")

  if (!twobytwo) {
    res <- res[, -3:-4]
  }

  res

}
collapse_table <- function(data, collapse) {
  if (collapse == FALSE) {
    return(data)
  } else if (collapse == "CI") {
    data$`CI Lower` <- paste0("[", data$`CI Lower`, ";", data$`CI Upper`, "]")
    data$`CI Lower` <- ifelse(data$`CI Lower` == "[;]", "", data$`CI Lower`)
    names(data)[names(data) == "CI Lower"] <- "CI"
    data$`CI Upper` <- NULL

    return(data)

  } else if (collapse == "OR") {

    data[2:nrow(data), "OR"] <- paste0(data[2:nrow(data), "OR"],
                                       " [", data[2:nrow(data), "CI Lower"], ";",
                                       data[2:nrow(data), "CI Upper"], "]")
    names(data)[names(data) == "OR"] <- "OR [CI]"
    data[, c("CI Lower", "CI Upper")] <- NULL

    return(data)
  }
}

#' Backward stepwise selection with pvalue for logistic regression with clustering
#'
#' @param fitcl Initial multivariate model
#' @param cluster Character. Name of the clustering variable of the model
#' @param threshold Numeric [0,1].
#' @param verbose Whether to display messages or not. Default TRUE
#'
#' @return A final multivariate model
#' @export
step_lrcl_pval <- function(fitcl, cluster, threshold = 0.05, verbose = TRUE) {

  data    <- fitcl$glm_res$data
  family  <- fitcl$glm_res$family
  formula <- fitcl$glm_res$formula

  stop <- FALSE
  j <- 0
  while (stop == FALSE) {
    j <- j + 1
    fitcl <- glm.cluster(formula = formula, data = data, family = family, cluster = cluster)

    fit <- fitcl$glm_res
    vcov <- fitcl$vcov[-1, -1]
    betas    <- coef(fit)[-1]

    # Nombre de modalité par variable du fit
    nmod <- unlist(lapply(fit$xlevels, function(x){length(x)}))

    # Index de départ/arrivée de la matrice pour chaque variable
    indvcov  <- cumsum(nmod - 1) - (nmod - 1) + 1
    indvcovf <- cumsum(nmod - 1)

    pval <- NULL
    for (i in seq_along(fit$xlevels)) {
      pval[i] <- wald.test(vcov, betas, indvcov[i]:indvcovf[i])$result$chi2[3]
    }

    if (max(pval) < threshold) {
      break()
    }

    vartokeep <- names(fit$xlevels)[!(pval == max(pval))]

    formula <- as.formula(paste(as.character(fit$formula)[2], "~", paste(vartokeep, collapse = " + ")))

    if (verbose %in% "TRUE") {
      cat(paste("Model", j,": \n",
                paste(as.character(formula)[2],
                      as.character(formula)[1],
                      as.character(formula)[3])), sep = "\n")
      cat(paste("Discarded :", names(fit$xlevels)[(pval == max(pval))]),
          sep = "\n")
      cat("\n")
    }
  }

  if (verbose %in% c("Final", TRUE)) {
    cat(paste("Final model :",
              as.character(formula)[2],
              as.character(formula)[1],
              as.character(formula)[3]), sep = "\n")
  }

  return(fitcl)

}

#' Multivariate Logistic Regression with cluster
#'
#' A function used to generate result table for multivariate logistic regression
#' model using a cluster variable. Compute robust variance using sandwich
#'
#' @param fit Class glm.cluster. Multivariate model to format
#'
#' @return A dataframe of the multivariate parameters formatted
#' @export
#'
logistic_cluster_multivariate <- function(fit){
  nmod <- unlist(lapply(fit$glm_res$xlevels, function(x){length(x)}))

  posmod <- cumsum(nmod) - nmod + 1
  posemp <- (1:sum(nmod))[!(1:sum(nmod) %in% posmod)]

  res <- matrix("", ncol = 9, nrow = sum(nmod))
  varname <- names(fit$glm_res$xlevels)

  res[posmod, 1] <- varname
  res[, 2]       <- unlist(fit$glm_res$xlevels)

  #coef(final_Serome)[grepl(names(final_Serome$glm_res$xlevels)[4], names(coef(final_Serome)))]

  table <- NULL
  for (i in varname) {
    table <- rbind(table, table(fit$glm_res$model[, i], fit$glm_res$model[, 1]))
  }

  res[, 3:4] <- table

  res[posmod, 5]   <- 1
  res[posemp, 5]   <- round(exp(coef(fit)[-1]), 2)
  res[posemp, 6:7] <- round(exp(confint(fit)[-1, ]), 2)

  inut <- capture.output(pval <- summary(fit))
  res[posemp, 8] <- pval_format_r(signif(pval[-1, 4], 2))
  res[posemp, 9] <- pval_format(signif(pval[-1, 4], 2))

  lev <- levels(fit$glm_res$data[, as.character(fit$glm_res$formula)[2]])

  colnames(res) <- c(as.character(fit$glm_res$formula)[2], "Modality", lev[1], lev[2],
                     "OR", "CI Lower", "CI Upper", "p-value", "Sign")

  res
}
