#' Univariate Logistic
#'
#' A function used to generate multiple result table for univariate logistic
#' regression model with \code{y ~ x}. For each specified \code{y}, a result
#' table is computed, including every \code{x} variable.
#'
#' @param data A dataframe including all the variable needed in all the models
#' @param y A vector of the names of response variable
#' @param x A vector of the names of predictor variable
#'
#' @return If \code{lenght(y) > 1}, the returned value is a list including all
#'   table result for each response variable. If \code{lenght(y) > 1}, the
#'   returned value is a dataframe object, the table result.
#'
#' @importFrom stats as.formula glm confint coef
#'
#' @export

logistic_univariate <- function(data, y, x) {
  dep <- y
  col <- x

  res_uni <- list()

  for (j in seq_along(dep)) {
    res <- data.frame()

    for (i in seq_along(col)) {
      x <- col[i]
      y <- dep[j]
      res <- rbind.data.frame(res, glm.univar(y, x, data), stringsAsFactors = F)
    }
    res_uni[[j]] <- res
  }

}

glm.univar <- function(y, x, data) {

  formula <- as.formula(paste0(y, " ~ ", x))

  nlev <- length(levels(data[, x]))

  fit <- glm(formula, data = data, family = "binomial")

  res <- matrix("", ncol = 9, nrow = nlev)

  res[1, 1] <- x
  res[, 2]  <- fit$xlevels[[1]]
  res[, 3]  <- t(table(fit$model))[, 1]
  res[, 4]  <- t(table(fit$model))[, 2]


  if (!any(table(fit$model) %in% 0)) {
    res[1, 5] <- 1
    res[2:nlev, 5] <- round(exp(fit$coefficients), 2)[2:nlev]
    res[2:nlev, 6:7] <- round(exp(confint(fit)), 2)[2:nlev, ]
    res[2:nlev, 8] <- signif(coef(summary(fit))[2:nlev, 4], 2)
    res[2:nlev, 9] <- pval_format(coef(summary(fit))[2:nlev, 4])
  }

  colnames(res) <- c(y, "Modality", levels(data[, y])[1], levels(data[, y])[2],
                     "OR", "CI Lower", "CI Upper", "p-value", "Sign")

  res

}
