
<!-- README.md is generated from README.Rmd. Please edit that file -->

# YPJ

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/Ygall/YPJ.svg?branch=master)](https://travis-ci.org/Ygall/YPJ)
<!-- badges: end -->

Clinical Research Utilities Functions Useful functions for clinical
research data analysis.

## Functions

  - tabkris\_2 : Description table with options

## Installation

Future released version will be available on
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("YPJ")
```

For now, development version is available on
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Ygall/YPJ")
```

# Manual

# TABKRIS2 : Goal and principles

TABKRIS2 aims to provide a ready to use **descriptive table with easy
customization**. The main principles is that for a given dataframe, it
computes descriptive statistics for each variable in the data. Only a
dataframe with no other arguments is required.

Then, user can add several options to customize the aspect of the
results such as changing the descriptive statistics method, adding a
stratifying variable, performing tests, changing the default methods and
tests.

The result is a dataframe ready to export into a Markdown or LaTeX
document.

-----

# Minimal example

It detects the type of each variable from the input dataframe. Variable
types are:

  - **Continuous** for integer/double/numeric variables.
  - **Binomial** for logical or factor with 2 levels.
  - **Categorical** for factor with more than 2 levels and not ordered.
  - **Ordered** for factor with more than 2 levels and ordered.

The default presentation for quantitative variables is *“mean (SD)”* and
for qualitative variable is *“n (percent %)”*. Binomial variables are
displayed in one line, unordered and ordered categorical variables are
displayed with one line for every level.

``` r
library(YPJ)

# Data must be prepared for variable type, here is the transformation of "cyl", "vs", "am", "gear" and "carb" in factors.
mtcars <- datasets::mtcars

for (i in c("cyl", "vs", "am", "gear", "carb")) {
  mtcars[, i] <- factor(mtcars[, i])
}

desctable <- tabkris_2(mtcars)

# knitr::kable(desctable)
```

-----

# Customize presentation of results

## **return\_table = FALSE**

Using the argument **return\_table = FALSE** will not return a table but
a list including all parameters used for the computation of the table.
The user can modify only the argument he wants without needing to
specify for every variable an unchanged paramater. To compute the table,
pass the list to the function once more with **return\_table = TRUE**.

It is possbile to create a *desc\_prep* object with every default
parameter, change a parameter, compute a table and re-use the
*desc\_prep* to rechange another parameter for another table.

``` r
desc_prep <- tabkris_2(mtcars, return_table = F)

# Change the method for variable "vs" from a binomial to a categorical method
desc_prep$method["vs"] <- "cate"

desctable <- tabkris_2(desc_prep)

# Variable of interest set to "am", also using the previous changed arguments
desc_prep$varint <- "am"

desctable_2 <- tabkris_2(desc_prep)
```

## Customization of result

Several options are useful to render the results in another shape. It
includes changing the names of each variable, changing the default
presentation for qualitative and quantitative variables, displaying the
NA number, changing the default number of digits and changing the
language of the first row of the result table.

## Method used for each variable

Default methods use for descriptive statistics is detected depending the
variable type. It is possible to change the behavior in two different
ways :

  - Change **default\_method** argument to change behavior for all
    variable of one type. **default\_method** is a vector of length 4.
    In order, each element refers to continuous variables (1), binomial
    variables (2), unordered categorical variables (3) and ordered
    categorical variables (4). It passes the default method to construct
    the descriptive statistics. See table below for accepted values of
    default method. Default is *“default\_method =
    c(”cont“,”bino“,”cate“,”ordo“)”*.
  - Change **method** argument to change behavior only for selected
    variable. **method** is a vector of length of the number of
    variables. See table below for possible values for each variable
    type.

**default\_method** is useful for changing every variable type method in
one value, **method** is useful to fine-tune every variable.

| default\_method\[x\] | cont | bino | cate | ordo |
| :------------------: | :--: | :--: | :--: | :--: |
|     x = 1 (cont)     |  X   |      |      |      |
|     x = 2 (bino)     |      |  X   |  X   |  X   |
|     x = 3 (cate)     |      |      |  X   |  X   |
|     x = 4 (ordo)     |      |      |  X   |  X   |

``` r
desc_prep <- tabkris_2(mtcars, return_table = F)

# Change the method for all binomial variable to categorical
desc_prep$default_method[2] <- "cate"

desctable <- tabkris_2(desc_prep)

# Changing only the method for "vs" to categorical
desc_prep$method["vs"] <- "cate"

desctable_2 <- tabkris_2(desc_prep)
```

## Names

The user provides a vector of length of the number of variables with
customs labels in the **names** argument.

## Descriptive statistics presentation

  - For quantitative variable, with **pres\_quanti**, options for
    presentation include *“mean (SD)”* (with “mean”), *“median \[IQR\]”*
    (with “med”) and *“{range}”* (with “range”). It is possible to
    display all three statistics, providing a vector with intended
    method.
  - For qualitative variable, with **pres\_quali**, options for
    presentation include *“number”* (with “N”), *“/ total”* (with
    “total”), *“percentages”* (with “per”). It is also possible to
    display only one, two or three of the options, providing a vector
    with intended method. For convenience, if “total” is used, total
    column will be empty because of redundancy of information.

## Displaying NA

With **explicit\_na**, user can choose to display NA for each variable
or not. NA are not accounted in the percentages. Use “addNA(x)” to a
factor variable to account for NA in descriptive statistics.

## Other presentation options

  - **digits** modify the number of digits displayed in descriptive
    statistics and number of significant numbers in p-value for tests.
  - **lang** modify the first row of the result table, available
    language are *“en”* for english and *“fr”* for french.

<!-- end list -->

``` r

# Changing the names
lab <- c("Miles/US gallon", "Number of cylinders", "Displacement", "Horsepower", "Rear axle ratio", "Weight", "1/4 mile time", "Engine", "Transmission", "N Forward gears", "N carburetors")

desctable <- tabkris_2(mtcars, names = lab,
                       pres_quant = c("mean", "range"),
                       pres_quali = c("n", "total", "per"),
                       explicit_na = T,
                       digits = 1,
                       lang = "fr")

# knitr::kable(desctable)
```

-----

# Adding a variable of interest and tests

## Variable of interest

With **varint** argument, user can specify a variable in the data to
stratify the results on. The variable of interest will be removed from
descriptive table. **varint** must be a factor with at least two levels.

## Tests

If a variable of interest is specified, statistical tests with the
hypothesis of a difference in levels of *“varint”* can be computed.
Nature of test made depends on the *“varint”* and type of other
variable. Only p-value of test is displayed with a type I error set to
0.05 and bilateral hypothesis.

It is possible to change the behavior of tests in two different ways :

  - Change **default\_test** argument to change behavior for all
    variable of one type. **default\_test** is a vector of length 4. In
    order, each element refers to continuous variables (1), binomial
    variables (2), unordered categorical variables (3) and ordered
    categorical variables (4). It passes the default test to compute the
    test. Default is *“default\_test =
    c(”stud“,”chisq“,”chisq“,”chisq“)”*.
  - Change **test** argument to change behavior only for selected
    variable. **test** is a vector of length of the number of variables.

**default\_test** is useful for changing every variable type test in one
value, **test** is useful to fine-tune every variable.

Implemented tests include **t.test** (with “stud”), **wilcox.test**
(with “wilcox”), **kruskal.test** (with “kruskal”), **chisq.test** (with
“chisq”), **fisher.test** (with “fish”). See table below to understand
which tests are implemented and when it is possible to use them.

Note : If the number of levels of *“varint”* is greater than 2,
*“default\_test”* will be automatically set to "*kruskal"* for
continuous and ordered variables.

#### Level of varint equal to 2

|  test   | cont | bino | cate | ordo |
| :-----: | :--: | :--: | :--: | :--: |
|  stud   |  X   |      |      |      |
| wilcox  |  X   |      |      |      |
| kruskal |      |      |      |      |
|  chisq  |      |  X   |  X   |  X   |
| fisher  |      |  X   |      |      |

#### Level of varint greater than 2

|  test   | cont | bino | cate | ordo |
| :-----: | :--: | :--: | :--: | :--: |
|  stud   |      |      |      |      |
| wilcox  |      |      |      |      |
| kruskal |  X   |      |      |      |
|  chisq  |      |  X   |  X   |      |
| fisher  |      |      |      |  X   |
