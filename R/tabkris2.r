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
#'   \code{ncol(data)}
#' @param varint Variable to stratify on, factor only
#' @param lang Language to display, default \code{"en"}, \code{"fr"}
#' @param method Vectors of variables to customize the methods used for
#'   description, length of data columns
#' @param test Either a logical indicating statistical tests execution or a
#'   vectors of variables to customize the tests, length of data columns.
#'   Default \code{FALSE}
#' @param pres_quant Descriptive statistics for quantitative variables. Possible
#'   values are \code{"mean"} for mean, SD, \code{"med"} for median, IQR,
#'   \code{"range"} for range
#' @param pres_quali Descriptive statistics for qualitative variables. Possible
#'   values are \code{"n"} for number, \code{"total"} to add "/ total" and
#'   \code{"per"} for percentages
#' @param default_method Default method to compute the table for each variable.
#'   Default \code{default_method = c("cont", "bino", "cate", "ordo")}
#' @param default_test Default test to apply for each variable type. Default
#'   \code{c("stud", "chisq", "chisq", "chisq")}. Available \code{"stud",
#'   "wilcox", "kruskal", "chisq", "fish"}
#' @param explicit_na Whether to display NA in description, Default \code{FALSE}
#' @param digits Number of significant number to display, default \code{2}
#' @param return_table Whether to return a dataframe or an object to customize
#'   option easily, default \code{TRUE}
#'
#' @importFrom stats median quantile chisq.test fisher.test kruskal.test t.test
#'   wilcox.test
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
                      pres_quali = c("n", "per"),
                      default_method = c("cont", "bino", "cate", "ordo"),
                      default_test   = c("stud", "chisq", "chisq", "chisq"),
                      explicit_na = FALSE,
                      digits = 2,
                      return_table = TRUE) {

  # Check
  env <- environment()
  data  <- check_data(data, env)

  names <- check_names(data, names)

  check_varint(data, varint)
  check_args(lang,
             pres_quant,
             pres_quali,
             default_method,
             default_test,
             explicit_na,
             digits)

  if (!is.null(method)) {
    method <- check_method(data, method, names)
  } else {
    method <- make_method(data, default_method)
  }

  default_test <- check_default_test(data, varint, default_test)

  test_yn <- check_test_yn(data, test, varint)

  if (test_yn == TRUE) {
    test <- make_test(data, default_test)
    check_test(data, test, default_test, varint, method)
  }

  if (return_table == TRUE) {
    # Transform the data in list to iterate
    data_c <- data
    data <- make_varint(data, varint)
    # Description
    result <-
      make_result(
        data,
        data_c,
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
    result <- list(
      data = data,
      names = names,
      varint = varint,
      lang = lang,
      method = method,
      test = test,
      pres_quant = pres_quant,
      pres_quali = pres_quali,
      default_method = default_method,
      default_test   = default_test,
      explicit_na = explicit_na,
      digits = digits
    )
    attr(result, "class") <- "desctable"
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
