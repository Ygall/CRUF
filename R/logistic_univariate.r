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
#' @param ref.label Character. Set the label for reference estimate.
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
                                collapse = FALSE, ref.label = "1",
                                digits = 2) {
  y <- y_names
  x <- x_names

  check_args_log(data, y, x, twobytwo, formula, collapse, ref.label, digits)

  dep <- y
  col <- x

  res_uni <- list()

  for (j in seq_along(dep)) {
    res <- data.frame()

    for (i in seq_along(col)) {
      x <- col[i]
      y <- dep[j]

      res_one <- data.frame(glm_univar(y, x, data, twobytwo, formula,
                           digits, ref.label),
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

check_args_log <- function(data, y, x, twobytwo, formula, collapse, ref.label, digits) {
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

  if (!is.character(ref.label)) {
    stop("Arg ref.label must be character", call. = FALSE)
  }

  if (!is.numeric(digits)) {
    stop("Arg digits must be numeric", call. = FALSE)
  }
}
glm_univar     <- function(y, x, data, twobytwo, formula, digits, ref.label) {

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
    res[1, 5] <- ref.label
    res[2:nlev, 5] <- round(exp(fit$coefficients), digits)[2:nlev]
    res[2:nlev, 6:7] <- suppressMessages(round(exp(confint(fit)),
                                               digits)[2:nlev, ])

    if (nlev > 2) {
      res[1, 8] <- paste0("Global: ",
                          signif(anova(fit, test = "Chisq")[2, 5], 2))
      res[1:nlev, 9] <- pval_format(anova(fit, test = "Chisq")[2, 5])
    }
    res[2:nlev, 8] <- signif(coef(summary(fit))[2:nlev, 4], 2)
    res[2:nlev, 9] <- pval_format(coef(summary(fit))[2:nlev, 4])
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

