#' p-value format
#'
#' Format a p-value into R display system with stars
#'
#' @param pval Numeric.
#'
#' @return "***" if < 0.001, "\*\*" if < 0.01, "\*" if < 0.05, "." if < 0.1
#' @export
#'
pval_format <- function(pval) {
  res <-
    ifelse(pval < 0.001, "***",
           ifelse(pval < 0.01,  "**",
                  ifelse(pval < 0.05,  "*",
                         ifelse(pval < 0.1,   ".", ""))))
}
pval_format_r <- function(pval) {
  res <-
    ifelse(pval < 0.001, "< 0.001", pval)
}

#' Numeric factor
#'
#' Coerce a factor to a numeric vector
#' @param x Factor to coerce
#'
#' @return A vector as a numeric
#' @export
#'
as_numeric_factor <- function(x) {
  as.numeric(levels(x))[x]
}
