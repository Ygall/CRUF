#' Univariate Surival
#'
#' @param data A dataframe inclunding all the variable needed, one variable for
#'   time to event and one variable for event indicator.
#' @param time Name of the variable used for time to event.
#' @param event Name of the column used for event indicator.
#' @param names Names of the variables to display. Lenght must be minus 2 the
#'   number of column of data, excluding time and event
#' @param test Which test to use for p-value, possible values are "LRT" for
#'   Likelihood Ratio Test, "Wald" for Wald Test and "LogRank" for Log-Rank
#'   Test"
#'
#' @importFrom survival coxph
#' @importFrom stats as.formula
#' @return Return a table with model parameters for every variable included in
#'   data.
#' @export
#'
survival_univariate <- function(data, time, event, names = NULL, test = "LRT") {
  # Workflow :
  #
  # - Check si time et event sont dans data, si data est une dataframe, si test
  # dans LRT/Wald/LogRank. Si time est un numérique et si event est un binaire
  #
  # - Préparation des données : Faire un vecteur de names en enlevant time et
  # event. Repérer celles qui sont continues et celle qui sont des facteurs +
  # ajouter le nombre de facteur. Calculer le nombre d'événement total et le nombre d'observations
  #
  # - Faire les fit avec itération sur les vecteur de names, storer dans une
  # liste, laisse la possibilité de rendre la liste de tous les fits fait pour
  # inspection
  #
  # - Faire le tableau de présentation en fonction du type de variable.
  #
  ##  -- Commencer par le header
  ##  -- Pour continues : une seule ligne donnée
  ##  -- Pour factorielles : une ligne par modalité, la première ligne étant spéciale

  # Check sanity
  check_args_uni(data, time, event, names, test)

  # Prepare data
  vecnames      <- subset(colnames(data), !(colnames(data) %in% c(time, event)))
  if (is.null(names)) names <- vecnames

  data[, event] <- as.numeric.factor(factor(data[, event]))
  nevent        <- sum(data[, event])

  # Fit the data
  res_list <- list()

  for (i in seq_along(vecnames)) {
    formula <-  as.formula(paste0("Surv(", time, ",", event, ") ~ " , vecnames[i]))
    res_list[[i]] <- coxph(formula = formula, data = data)
  }

  # Get the number of level for each model
  veclevel <- unlist(lapply(res_list, function(x){(length(x$xlevels[[1]]))}))
  veclevel <- ifelse(veclevel == 0, 1, veclevel)

  # Make the result table
  result <- make_result_uni(res_list, vecnames, names, veclevel, test, data, event)

  # Post process : Display N only if different from
  result <- post_process_result_uni(result, data, nevent)

  return(result)
}

check_args_uni <- function(data, time, event, names, test) {
  if (!("data.frame" %in% attributes(data)$class)) {
    stop("Data should be a data frame", call. = FALSE)
  }

  if (!is.null(names)) {
    if (length(names) != (length(colnames(data)) - 2)) {
      stop(paste0("Names must be length of ",
                  length(colnames(data)) - 2),
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
}

make_result_uni <- function(res_list, vecnames, names, veclevel, test, data, event) {
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

    result <- rbind.data.frame(result, res, stringsAsFactors = F)
  }

  data.frame(result, stringsAsFactors = F)
  colnames(result) <- c("Variable", "Modality", "N Event", "N Group",
                        "HR", "CI.Lower", "CI.Upper", "p-value", "Sig",
                        "Method")

  result
}

make_result_cont <- function(model, vecname, name, test) {
  t.switch <- switch (test,
                      "LRT"     = summary(model)$logtest[3],
                      "Wald"    = summary(model)$waldtest[3],
                      "LogRank" = summary(model)$sctest[3]
  )

  result <- matrix("", nrow = 1, ncol = 10)

  result[1, 1]   <- name
  result[1, 3]   <- model$nevent
  result[1, 4]   <- model$n
  result[1, 5:7] <- round(summary(model)$conf.int[-2], 3)

  result[1, 8]   <- signif(t.switch, 2)
  result[1, 9]   <- pval_format(t.switch)
  result[1, 10]  <- test

  result
}

make_result_fact <- function(model, vecname, name, level, test, data, event) {
  t.switch <- switch (test,
                      "LRT"     = summary(model)$logtest[3],
                      "Wald"    = summary(model)$waldtest[3],
                      "LogRank" = summary(model)$sctest[3]
  )

  result <- matrix("", nrow = level, ncol = 10)
  result[1, 1]   <- name

  result[, 2] <- model$xlevels[[1]]

  result[, 3] <- as.vector(by(data[, event], data[, vecname], sum))
  result[, 4] <- as.vector(by(data[, event], data[, vecname], length))

  result[1, 5:7] <- c(1, "", "")
  result[2:level, 5:7] <- round(summary(model)$conf.int[, -2], 3)

  result[1, 8]   <- signif(t.switch, 2)
  result[1, 9]   <- pval_format(t.switch)
  result[1, 10]  <- test

  result
}

# Helper functions
pval_format <- function(pval) {
  res <-
      ifelse(pval < 0.001, "***",
      ifelse(pval < 0.01,  "**",
      ifelse(pval < 0.05,  "*",
      ifelse(pval < 0.1,   ".", ""))))
}
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

post_process_result_uni <- function(result, data, nevent) {
    # Delete N event and N group if identical to the sum of event or group

  result$`N Event` <- ifelse(result$`N Event` == nevent & result$`N Group` == nrow(data), "", result$`N Event`)
  result$`N Group` <- ifelse(result$`N Event` == nevent & result$`N Group` == nrow(data), "", result$`N Group`)

    # Delete HR estimand if non convergence
  result$HR <- ifelse(result$CI.Upper == "Inf", "Inf HR", result$HR)
  result$CI.Lower <- ifelse(result$CI.Upper == "Inf", "", result$CI.Lower)
  result$CI.Upper <- ifelse(result$CI.Upper == "Inf", "", result$CI.Upper)

  result
}
