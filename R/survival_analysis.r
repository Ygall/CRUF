#' Univariate Survival Regression
#'
#' @param data A dataframe including all the variable needed, one variable for
#'   time to event and one variable for event indicator.
#' @param time Name of the variable used for time to event or for start time if
#'   Start-Stop format
#' @param event Name of the column used for event indicator.
#' @param names Names of the variables to display. Lenght must be minus 2 the
#'   number of column of data, excluding time and event
#' @param test Which test to use for p-value, possible values are "LRT" for
#'   Likelihood Ratio Test, "Wald" for Wald Test and "LogRank" for Log-Rank
#'   Test"
#' @param strata Name of the variable used for analysis with strata
#' @param cluster Name of the variable used for analysis with cluster
#' @param time2 Stop time if the data are in Start-Stop format
#'
#' @importFrom survival coxph
#' @importFrom stats as.formula
#' @return Return a table with model parameters for every variable included in
#'   data.
#' @export
#'
survival_univariate <- function(data, time, time2 = NULL, event, names = NULL,
                                strata = NULL, cluster = NULL,
                                test = "LRT") {
  # Check sanity
  check_args_uni(data, time, time2, event, names, test, strata, cluster)

  # Prepare data
  vecnames      <- subset(colnames(data),
                          !(colnames(data) %in% c(time, time2, event,
                                                  strata, cluster)))
  if (is.null(names)) names <- vecnames

  data[, event] <- as_numeric_factor(factor(data[, event]))
  nevent        <- sum(data[, event])

  if (!missing(time2)) {
    time2 <- paste0(" ", time2, ", ")
  }
  if (!missing(cluster)) {
    cluster <- paste0("+ cluster(", cluster, ")")
  }
  if (!missing(strata)) {
    strata <- paste0("+ strata(", strata, ")")
  }

  # Fit the data
  res_list <- list()

  for (i in seq_along(vecnames)) {
    formula <-  as.formula(paste0("Surv(", time, ",", time2, event, ") ~ ",
                                  vecnames[i], cluster, strata))
    res_list[[i]] <- coxph(formula = formula, data = data)
  }

  # Get the number of level for each model
  veclevel <- NULL
  for (i in seq_along(vecnames)) {
    veclevel[i] <- length(res_list[[i]]$xlevels[[vecnames[i]]])
  }
  veclevel <- ifelse(veclevel == 0, 1, veclevel)


  # Make the result table
  result <- make_result_uni(res_list, vecnames, names,
                            veclevel, test, data, event)

  # Post process : Display N only if different from
  result <- post_process_result_uni(result, data, nevent)

  return(result)
}

check_args_uni <- function(data, time, time2, event, names, test,
                           strata, cluster) {
  if (!("data.frame" %in% attributes(data)$class)) {
    stop("Data should be a data frame", call. = FALSE)
  }

  if (!is.null(names)) {
    off <- 2 + !is.null(cluster) + !is.null(strata)
    if (length(names) != (length(colnames(data)) - off)) {
      stop(paste0("Names must be length of ",
                  length(colnames(data)) - off),
           call. = FALSE)
    }

  }

  if (missing(time)) {
    stop("Time to event variable not set", call. = FALSE)
  } else if (!(time %in% colnames(data))) {
    stop("Time to event name is not in dataframe", call. = FALSE)
  } else if (!is.numeric(data[, time])) {
    stop("Time to event is not numeric", call. = FALSE)
  }

  if (missing(event)) {
    stop("Event variable not set", call. = FALSE)
  } else if (!(event %in% colnames(data))) {
    stop("Event name is not in dataframe", call. = FALSE)
  } else if (!(nlevels(factor(data[, event])) == 2)) {
    stop("Event must have two levels", call. = FALSE)
  }

  if (!(test %in% c("LRT", "Wald", "LogRank"))) {
    stop("Test method not in \"LRT\", \"Wald\", \"LogRank\"", call. = FALSE)
  }

  if (!is.null(time2)) {
    if (!(time2 %in% colnames(data))) {
      stop("Time2 name is not in dataframe", call. = FALSE)
    }
  }

  if (!is.null(cluster)) {
    if (!(cluster %in% colnames(data))) {
      stop("Cluster name is not in dataframe", call. = FALSE)
    }
  }

  if (!is.null(strata)) {
    if (!(strata %in% colnames(data))) {
      stop("Strata name is not in dataframe", call. = FALSE)
    }
  }
}

make_result_uni <- function(res_list, vecnames, names,
                            veclevel, test, data, event) {
  result <- data.frame()

  for (i in seq_along(res_list)) {
    model   <- res_list[[i]]
    level   <- veclevel[i]
    vecname <- vecnames[i]
    name    <- names[i]

    if (level == 1) {
      res <- make_result_cont(model, vecname, name, test)
    } else {
      res <- make_result_fact(model, vecname, name, level, test, data, event)
    }

    result <- rbind.data.frame(result, res, stringsAsFactors = FALSE)
  }

  data.frame(result, stringsAsFactors = FALSE)
  colnames(result) <- c("Variable", "Modality", "N Event", "N Group",
                        "HR", "CI.Lower", "CI.Upper", "p-value", "Sig",
                        "Method")

  result
}

make_result_cont <- function(model, vecname, name, test) {
  t.switch <- switch(test,
                      "LRT"     = summary(model)$logtest[3],
                      "Wald"    = summary(model)$waldtest[3],
                      "LogRank" = summary(model)$sctest[3]
  )

  if (length(as.character(model$call)) > 3) {
    if (summary(model)$used.robust) {
      t.switch <- summary(model)$robscore[3]
      test <- "Robust Score"
    }
  }

  result <- matrix("", nrow = 1, ncol = 10)

  result[1, 1]   <- name
  result[1, 3]   <- model$nevent
  result[1, 4]   <- model$n
  result[1, 5:7] <- round(summary(model)$conf.int[1, -2], 3)

  result[1, 8]   <- signif(t.switch, 2)
  result[1, 9]   <- pval_format(t.switch)
  result[1, 10]  <- test

  result
}

make_result_fact <- function(model, vecname, name, level, test, data, event) {
  t.switch <- switch(test,
                      "LRT"     = summary(model)$logtest[3],
                      "Wald"    = summary(model)$waldtest[3],
                      "LogRank" = summary(model)$sctest[3]
  )

  if (length(as.character(model$call)) > 3) {
    if (summary(model)$used.robust) {
      t.switch <- summary(model)$robscore[3]
      test <- "Robust Score"
    }
  }

  result <- matrix("", nrow = level, ncol = 10)
  result[1, 1]   <- name

  result[, 2] <- levels(factor(data[, vecname]))

  result[, 3] <- as.vector(by(data[, event], data[, vecname], sum))
  result[, 4] <- as.vector(by(data[, event], data[, vecname], length))

  result[1, 5:7] <- c(1, "", "")
  result[2:level, 5:7] <- round(summary(model)$conf.int[1:(level - 1), -2], 3)

  result[1, 8]   <- signif(t.switch, 2)
  result[1, 9]   <- pval_format(t.switch)
  result[1, 10]  <- test

  if (min(result[, 3:4]) %in% c(0, NA)) {
    result[, 5:7] <- ""
  }

  result
}

post_process_result_uni <- function(result, data, nevent) {
    # Delete N event and N group if identical to the sum of event or group

  result$`N Event` <- ifelse(result$`N Event` == nevent &
                               result$`N Group` == nrow(data), "",
                             result$`N Event`)
  result$`N Group` <- ifelse(result$`N Event` == nevent &
                               result$`N Group` == nrow(data), "",
                             result$`N Group`)

    # Delete HR estimand if non convergence
  result$HR <- ifelse(result$CI.Upper == "Inf", "", result$HR)
  result$CI.Lower <- ifelse(result$CI.Upper == "Inf", "", result$CI.Lower)
  result$CI.Upper <- ifelse(result$CI.Upper == "Inf", "", result$CI.Upper)

  result
}
