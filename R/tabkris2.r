#' Description of data
#'
#' \code{tabkris_2} returns a dataframe with descriptive statistics for each
#' variable
#'
#' @details The \code{tabkris_2} function is a function to describe a set of
#'   data. Main purpose is to create a typical table one in biomedical
#'   litterature, either a patient characteristic table or population
#'   characteristic table. \code{names} is a vector to name the variable of
#'   data. Default will use the colnames of data. \code{varint} is a variable to
#'   stratify the analysis. It must be included in the initial dataset. It will
#'   not be displayed in the final table if chosen as the stratifying variable
#'   \code{lang} is useful to choose the language for the final display. The
#'   default is english. French is also supported. \code{explicit_na}.
#'
#' @param data Dataframe to describe
#' @param names Vectors of variables to display in the final table, length of
#'   data columns
#' @param varint Variable to stratify on, factor only
#' @param lang Language to display, default \code{"en"}, \code{"fr"}
#' @param method Vectors of variables to customize the methods used for
#'   description, length of data columns
#' @param test Either a logical indicating statistical tests execution or a
#'   vectors of variables to customize the tests, length of
#'   data columns. Default FALSE
#' @param pres_quant Descriptive statistics for quantitative variables. Possible
#'   values are "mean" for mean, SD, "med" for median, IQR, "range" for range
#' @param pres_quali Descriptive statistics for qualitative variables. Possible
#'   values are "n" for number, "total" to add "/ total" and "per" for
#'   percentages
#' @param default_method Default method to compute the table for each variable
#' @param default_test Default test to apply for each variable type
#' @param explicit_na Whether to display NA in description
#' @param digits Number of significant number to display, default system option
#' @param return_table Whether to return a dataframe or an object to customize
#'   option easily, default TRUE
#'
#' @importFrom stats median quantile
#'
#' @return A dataframe or an object with all arguments to customize function
#'   call
#' @export

tabkris_2 <- function(data,
                      names = NULL,
                      varint = NULL,
                      lang = "en",
                      method = NULL,
                      test = FALSE,
                      pres_quant = c("mean"),
                      pres_quali = c("n", "total", "per"),
                      default_method = c("cont", "bino", "cate", "ordo"),
                      default_test   = c("stud", "chisq", "chisq", "chisq"),
                      explicit_na = FALSE,
                      digits = 2,
                      return_table = TRUE) {
  # Check
  data  <- check_data(data)
  names <- check_names(data, names)

  check_varint(data, varint)
  check_args(lang,
             pres_quant,
             pres_quali,
             default_method,
             default_test,
             explicit_na,
             digits)

  test_yn <- check_test(data, test, varint)

  if (test_yn == TRUE) {
    test <- make_test(data, default_test)
  }

  if (!is.null(method)) {
    check_method(data, method)
  } else {
    method <- make_method(data, default_method)
  }

  # Transform the data in list to iterate
  data <- make_varint(data, varint)

  if (return_table == TRUE) {
    # Description
    result <-
      make_result(
        data,
        names,
        varint,
        method,
        test,
        test_yn,
        explicit_na,
        digits,
        pres_quant,
        pres_quali
      )

    # Translate
    result <- make_language(result, lang)

  } else {
    result <- list(names = names,
                   method = method,
                   test = test)
  }

  return(result)
}

# data <- boys
# names <- colnames(boys)
# lang <- "en"
#
# varint <- NULL
# explicit_na <- FALSE
#
#
# method = NULL
# default_method = c("cont", "bino", "cate", "ordo")
# test = TRUE
# default_test   = c("stud", "chisq", "chisq", "chisq")
# digits <- 2
# return_table = TRUE
# pres_quant = c("mean", "med", "range")
# pres_quali = c("n", "total", "per")
#
# tabkris_2(data = boys)
