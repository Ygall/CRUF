#' Univariate Logistic Regression
#'
#' A function used to generate multiple result table for univariate logistic
#' regression model with \code{y ~ x}. For each specified \code{y_names}, a
#' result table is computed, including all \code{x_names} variables.
#'
#' @param data A dataframe including all the variables needed in all the models
#' @param y_names Vector, name(s) of response variable(s)
#' @param x_names Vector, name(s) of predictor variable(s)
#' @param twobytwo Either to include the two by two table for each variable.
#'   Default is \code{TRUE}.
#' @param formula Formula for logistic regression to customize. Default is
#'   \code{(y ~ x)}.
#'
#' @return The returned value is a list of length \code{y_names}, which consists
#'   of a dataframe having the univariate logistic regressions of the
#'   \code{x_names}.
#'
#' @importFrom stats as.formula glm confint coef anova
#'
#' @export

logistic_univariate <- function(data, y_names, x_names,
                                twobytwo = TRUE, formula = "(y ~ x)") {
  y <- y_names
  x <- x_names

  check_args_log(data, y, x, twobytwo, formula)

  dep <- y
  col <- x

  res_uni <- list()

  for (j in seq_along(dep)) {
    res <- data.frame()

    for (i in seq_along(col)) {
      x <- col[i]
      y <- dep[j]
      res <- rbind.data.frame(res, glm_univar(y, x, data, twobytwo, formula),
                              stringsAsFactors = F)
    }
    res_uni[[j]] <- res
  }

  if (length(y_names) == 1) {
    res_uni <- data.frame(res_uni, check.names = F)
  }

  res_uni
}

glm_univar <- function(y, x, data, twobytwo, formula) {

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
    res[1, 5] <- 1
    res[2:nlev, 5] <- round(exp(fit$coefficients), 2)[2:nlev]
    res[2:nlev, 6:7] <- suppressMessages(round(exp(confint(fit)), 2)[2:nlev, ])

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

check_args_log <- function(data, y, x, twobytwo, formula) {
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
}
