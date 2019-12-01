check_data <- function(data) {
    if (is.null(data)) {
        stop("Data not provided", call. = FALSE)
    }

    if (!(is.matrix(data) || is.data.frame(data)))
        stop("Data should be a matrix or data frame", call. = FALSE)
    data <- as.data.frame(data)

    dup <- duplicated(colnames(data))
    if (any(dup))
        stop("Duplicate names found: ",
             paste(colnames(data)[dup], collapse = ", "),
             call. = F)

    data
}

check_names <- function(data, names) {
    if (!is.null(names)) {
        if (!is.vector(names)) {
            stop("Argument names not a vector", call. = FALSE)
        }

        if (length(names) != dim(data)[2]) {
            stop("Argument names must be length of data columns",
                 call. = FALSE)
        }
    } else {
        names <- colnames(data)
    }

    names
}

check_varint <- function(data, varint) {
    if (!is.null(varint)) {
        if (!(varint %in% colnames(data))) {
            stop("Argument varint not in data", call. = F)
        }

        if (!is.character(varint)) {
            stop("Argument varint not a character", call. = F)
        }

        if (!is.factor(data[, varint])) {
            stop("Argument varint must refer to a factor variable in data",
                 call. = F)
        }
    }
}

check_args <- function(lang,
                       pres_quant,
                       pres_quali,
                       default_method,
                       default_test,
                       explicit_na,
                       digits) {
    # Check le format de la langue
    lang <- match.arg(lang, c("english", "french"))
    if (!(lang %in% c("english", "french"))) {
        stop("Argument lang error.
             Supported language are \"english\", \"french\"",
             call. = FALSE)
    }

    # Check le format de la méthode de présentation des quantitatifs
    if (!all(sapply(pres_quant, function(x)
        x %in% c("mean",
                 "med", "range")))) {
        stop("Argument pres_quant not a correct value", call. = FALSE)
    }

    # Check le format de la méthode de présentation des qualitatifs
    if (!all(sapply(pres_quali, function(x)
        x %in% c("n", "total", "per")))) {
        stop("Argument pres_quali not a correct value", call. = FALSE)
    }

    # Check default method
    if (!is.vector(default_method)) {
        stop("Argument default_method not a vector", call. = F)
    } else if (length(default_method) != 4) {
        stop("Argument default_method must be length 4", call. = F)
    }
    # Check default test
    if (!is.vector(default_test)) {
        stop("Argument default_test not a vector", call. = F)
    } else if (length(default_test) != 4) {
        stop("Argument default_test must be length 4", call. = F)
    }

    # Check le format de explicit_na
    if (!is.logical(explicit_na)) {
        stop("Argument explicit_na not logical", call. = FALSE)
    }

    # Check le format de digits
    if (!is.numeric(digits)) {
        stop("Argument digits not numeric", call. = FALSE)
    } else if (digits < 0 || digits > 15) {
        stop("Argument digits not between 0 and 15", call. = FALSE)
    }
}

check_method <- function(data, method) {
    # Vérifier si l'input de l'utilisateur correspond à des méthodes viables
    if (!is.vector(method)) {
        stop("Argument method not a vector", call. = F)
    } else if (length(method) != dim(data)[2]) {
        stop("Argument method must be length of data columns", call. = FALSE)
    }

    for (i in seq_along(method)) {
        if (!(method[i] %in% c("cont", "bino", "cate", "ordo"))) {
            stop(paste0(
                "Argument method : ",
                method[i],
                " not in supported methods"
            ), call. = FALSE)
        }
    }

    #TODO Check l'adéquation entre les methods et les data
}

check_test <- function(data, test, varint) {
    # Vérifier si l'input de l'utilisateur correspond à des test viables

    if (test == FALSE) {
        test_yn <- FALSE
        return(test_yn)
    }

    if (test == TRUE & is.null(varint)) {
        test_yn <- FALSE
        warning("Argument test true but varint null, tests not executed",
                call. = FALSE)
        return(test_yn)
    } else if (test == TRUE) {
        test_yn <- TRUE
        return(test_yn)
    }

    if (!is.vector(test)) {
        stop("Argument test not a vector", call. = F)
    } else if (length(test) != dim(data)[2]) {
        stop("Argument test must be length of data columns", call. = FALSE)
    }

    for (i in seq_along(test)) {
        if (!(test[i] %in% c("stud", "chisq", "fisher", "kruskal", "wilcox"))) {
            stop(paste0(
                "Argument test : ",
                test[i],
                " not in supported tests"
            ), call. = FALSE)
        }
    }

    test_yn <- TRUE

    return(test_yn)
}
