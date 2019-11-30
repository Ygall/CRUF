#' Title
#'
#' @param obj      L'objet à présenter
#' @param varname  Le nom de la variable
#' @param test     Test à réaliser
#'
#' @return Une dataframe renvoyant la présentation d'un modèle de survie
#'
#' @importFrom stats as.formula coef pchisq
#' @importFrom survival survfit survdiff
#'
#' @export
#'
# #' @examples
cox_result <- function(obj, varname = NULL, test = "Likelihood") {

# Transformation d'un objet coxph_fit en survfit
  if (!(class(obj) %in% c("coxph_fit", "survfit"))) {
    stop("Unavailable object type, either \"coxph_fit\" or \"survfit\" needed")
  }

  if (class(obj) == "coxph_fit") {
    formula <- as.formula(obj$call$formula)
    data    <- eval(obj$call$data)
  }

  if (class(obj) == "survfit") {
    formula <- as.formula(obj$call$formula)
    data    <- eval(obj$call$data)
  }

  if (is.null(varname)) {
    varname <- as.character(substitute(obj))
  }

  if (!(test %in% c("Likelihood", "LogRank"))) {
    stop("Test should be \"Likelihood\" or \"LogRank\"")
  }

  # Fit des trois classes nécessaires
suppressWarnings({
  km    <- survfit(formula = formula,
                    data = data)
  coxph_fit <- coxph_fit(formula = formula,
                    data = data)
})
  if (test == "LogRank") {
    lr        <- NULL
    try(lr    <- survdiff(formula = formula,
                          data = data),
        silent = T)
  }


  varint <- names(coxph_fit$assign)
  lev <- levels(factor(data[, varint]))

  if (length(unique(data[, varint])) == 1) {
    stop("Data for only one level of variable, impossible to compute results")
  }

  if (sum(km$n.event) == 0) {
    warning("No event, impossible to compute results")
    res_table <- t(as.data.frame(summary(km)$table[4:3]))
  } else {
    res_table <- summary(km)$table[, 4:3]
  }

  res_table2 <- NULL
  for (i in seq_along(lev)) {
    if (i == 1) {
      hr <- 1
      ic <- ""
      pval <- ""
      if (test == "LogRank") {
        hr <- ""
      }
    } else {
      hr <- round(exp(coef(coxph_fit)), 2)
      ic <- paste0("[", round(summary(coxph_fit)$conf.int[3], 2), ";",
                        round(summary(coxph_fit)$conf.int[4], 2), "]")
      pval <- signif(summary(coxph_fit)$coef[5], 2)
      if (test == "LogRank") {
        if (!is.null(lr)) {
          pval <- paste0(signif(pchisq(lr$chisq, length(lr$n) - 1), 2), "*")
        } else {
          pval <- paste0(signif(summary(coxph_fit)$coef[5], 2), "*")
        }
        hr <- ""
        ic <- ""
      }
    }

    res_table2 <- rbind(res_table2, c(hr, ic, pval))
  }

  res_table <- cbind("", lev, res_table, res_table2)

  wip <- c(varname, "", "", "", "", "", "")

  res_table <- rbind(wip, res_table)

  colnames(res_table) <- c("Variable", "Group", "N Event", "N",
                           "HR", "IC", "pval")
  return(res_table)
}
