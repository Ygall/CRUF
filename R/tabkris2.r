#' Data description function
#'
#' \code{tabkris_2} computes descriptive statistics for data
#'
#' @details The \code{tabkris_2} function is a function to describe a set of
#'   data. Main purpose is to create a typical table one in biomedical
#'   litterature, either a patient characteristic table or population
#'   characteristic table.
#'
#'   \code{names} is a vector to name the variable of data. Default will use the
#'   colnames of data.
#'
#'   \code{varint} is a variable to stratify the analysis. It must be included
#'   in the initial dataset. It will not be displayed in the final table if
#'   chosen as the stratifying variable
#'
#'   \code{lang} is useful to choose the language for the final display. The
#'   default is english. French is also supported.
#'
#'   \code{default_method} and \code{method} are used to set the methods used
#'   for display. default_method must be length 4, to set the default method for
#'   continuous, binomial, categorical and ordered variable. \code{method} must
#'   be length of data columns, used to fine-tune every method for each
#'   variable.
#'
#'   \code{default_test} and \code{test} are used to set the tests performed.
#'   default_test must be lenght 4, to set the default method for continuous,
#'   binomial, categorical and ordered variable. \code{test} must be length of
#'   data columns, used to fine-tune every test for each variable.
#'
#'   \code{pres_quant} is used to set the display of quantitative variable.
#'   \code{mean (SD)}, \code{median [IQR]} and \code{range} are available,
#'   default is \code{median}.
#'
#'   \code{pres_quali} is used to set the display of qualitative variable.
#'   \code{"n"} for number, \code{"total"} to add "/ total" and \code{"per"} for
#'   percentages, default is \code{"n / per"}.
#'
#'   \code{explicit_na} is used to display.
#'
#'   \code{digits} is the number of digits to display for numbers. Usually if
#'   \code{n < 100}, \code{digits = 0} if \code{100< n < 200}, \code{digits = 1}
#'   else \code{digits = 2}.
#'
#'   \code{return_table} choose if the user wants to directly display a table or
#'   if the user wants to get an object with parametrable objects.
#'
#'   \code{auto_detect} will test if each column can be coerced to a factor
#'   (i.e. having between 2 and 10 levels) and change the type of variable if
#'   so.
#'
#'   \code{lev_co} will set the number of maximum levels to coerce a column in a
#'   factor
#'
#' @param data Dataframe to describe or a "desctable" object
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
#' @param auto_detect Whether to automatically detect variable type,
#'   transforming to factors numeric variable with moderate levels (< 10),
#'   default \code{TRUE}. Possible to set the cut-off number with \code{lev_co}
#' @param lev_co Numeric. When auto_detect is \code{TRUE}, set the number of
#'   level to cutoff for categorical variables
#' @param verbose Logical. Display information about transformation of
#'   variables. default \code{FALSE}
#'
#' @importFrom stats sd median quantile chisq.test fisher.test kruskal.test
#'   t.test wilcox.test
#'
#' @return Depending on argument \code{return_table}, an object of class
#'   data.frame, which is the descriptive table or an object of class
#'   \code{"desctable"}, which is a customizable object.
#'
#' @author Yves Gallien \email{yves.gallien@@gmail.com}, 2019
#'
#' @seealso \url{https://github.com/Ygall/CRUF} for manual and examples.
#'
#' @keywords descriptive, table one
#' @export


tabkris_2 <- function(data,
                      names = NULL,
                      varint = NULL,
                      lang = "en",
                      method = NULL,
                      test = FALSE,
                      pres_quant = c("med"),
                      pres_quali = c("n", "per"),
                      default_method = c("cont", "bino", "cate", "ordo"),
                      default_test   = c("stud", "chisq", "chisq", "chisq"),
                      explicit_na = FALSE,
                      digits = 2,
                      return_table = TRUE,
                      auto_detect = TRUE,
                      lev_co = 10,
                      verbose = FALSE) {
  # Logical junction

  if ("desctable" %in% attributes(data)$class) {
    # Make argument in environment
    env <- environment()
    data  <- check_data(data, env)
  }

  if (auto_detect == TRUE) {
    # Make auto_detect
    data <- make_auto_detect(data, lev_co, verbose)
  }

  check_args(lang,
             pres_quant,
             pres_quali,
             default_method,
             default_test,
             explicit_na,
             digits,
             return_table,
             auto_detect)

  names <- check_names(data, names)

  check_varint(data, varint)

  method <- check_default_method(data, method, default_method)

  if (!is.null(method)) {
    method <- check_method(data, method, names)
  } else {
    method <- make_method(data, default_method)
    method <- check_method(data, method, names)
  }

  if (test == TRUE) {
    default_test <- check_default_test(data, varint, default_test)
  }

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
