#' Title
#'
#' @param obj      L'objet à présenter
#' @param varname  Le nom de la variable
#' @param test     Test à réaliser
#'
#' @return Une dataframe renvoyant la présentation d'un modèle de survie
#' @import survival
#' @import survminer
#' @import stats
#'
#' @export
#'
# #' @examples
cox.result <- function(obj, varname = NULL, test = "Likelihood"){

# Transformation d'un objet coxph en survfit
  if (!(class(obj) %in% c("coxph", "survfit"))) {
    stop("Unavailable object type, either \"coxph\" or \"survfit\" needed")
  }

  if (class(obj) == "coxph") {
    formula = as.formula(obj$call$formula)
    data = eval(obj$call$data)
  }

  if (class(obj) == "survfit") {
    formula = as.formula(obj$call$formula)
    data = eval(obj$call$data)
  }

  if (is.null(varname)) {
    varname <- as.character(substitute(obj))
  }

  if (!(test %in% c("Likelihood", "LogRank"))) {
    stop("Test should be \"Likelihood\" or \"LogRank\"")
  }

  # Fit des trois classes nécessaires
suppressWarnings({
  KM    <- survfit( formula = as.formula(obj$call$formula), data = eval(obj$call$data))
  COXPH <- coxph(   formula = as.formula(obj$call$formula), data = eval(obj$call$data))
})
  if (test == "LogRank") {
    LR    <- NULL
    try(LR    <- survdiff(formula = as.formula(obj$call$formula), data = eval(obj$call$data)), silent = T)
  }


  varint <- names(COXPH$assign)
  lev <- levels(factor(data[ , varint]))

  if (length(unique(eval(obj$call$data)[ , varint])) == 1){
    stop("Data for only one level of variable, impossible to compute results")
  }

  if (sum(KM$n.event) == 0) {
    warning("No event, impossible to compute results")
    res.table <- t(as.data.frame(summary(KM)$table[4:3]))
  } else {
    res.table <- summary(KM)$table[ , 4:3]
  }

  res.table2 <- NULL
  for(i in seq_along(lev)){
    if (i == 1) {
      HR <- 1
      IC <- ""
      pval <- ""
      if (test == "LogRank") {
        HR <- ""
      }
    } else {
      HR <- round(exp(coef(COXPH)), 2)
      IC <- paste0("[", round(summary(COXPH)$conf.int[3], 2), ";", round(summary(COXPH)$conf.int[4], 2), "]")
      pval <- signif(summary(COXPH)$coef[5], 2)
      if (test == "LogRank") {
        if (!is.null(LR)) {
          pval <- paste0(signif(pchisq(LR$chisq, length(LR$n) - 1), 2), "*")
        } else {
          pval <- paste0(signif(summary(COXPH)$coef[5], 2), "*")
        }
        HR <- ""
        IC <- ""
      }
    }

    res.table2 <- rbind(res.table2, c(HR, IC, pval))
  }

  res.table <- cbind("", lev, res.table, res.table2)

  wip <- c(varname, "", "", "", "", "", "")

  res.table <- rbind(wip, res.table)

  colnames(res.table) <- c("Variable", "Group", "N Event", "N", "HR", "IC", "pval")
  return(res.table)
}
