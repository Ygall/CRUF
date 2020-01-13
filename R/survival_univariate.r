#' Univariate Surival
#'
#' @param data A dataframe inclunding all the variable needed, one variable for
#'   time to event and one variable for event indicator.
#' @param time Name of the variable used for time to event.
#' @param event Name of the column used for event indicator.
#' @param test Which test to use for p-value, possible values are "LRT" for
#'   Likelihood Ratio Test, "Wald" for Wald Test and "LogRank" for Log-Rank
#'   Test"
#'
#' @return Return a table with model parameters for every variable included in
#'   data.
#' @export
#'
survival_univariate <- function(data, time, event, test = "LRT") {

}
