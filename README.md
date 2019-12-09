YPJ
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/Ygall/YPJ.svg?branch=master)](https://travis-ci.org/Ygall/YPJ)
<!-- badges: end -->

Clinical Research Utilities Functions (CRUF) : Useful functions for
clinical research data analysis.

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

desctable <- tabkris_2(mtcars)

knitr::kable(desctable)
```

| Variable | N  | Statistics      |
| :------- | :- | :-------------- |
| mpg      | 32 | 20.09 (6.03)    |
| cyl      | 32 | 6.19 (1.79)     |
| disp     | 32 | 230.72 (123.94) |
| hp       | 32 | 146.69 (68.56)  |
| drat     | 32 | 3.6 (0.53)      |
| wt       | 32 | 3.22 (0.98)     |
| qsec     | 32 | 17.85 (1.79)    |
| vs       | 32 | 0.44 (0.5)      |
| am       | 32 | 0.41 (0.5)      |
| gear     | 32 | 3.69 (0.74)     |
| carb     | 32 | 2.81 (1.62)     |

-----

# Auto-detect variable type

Using the argument **auto\_detect = TRUE** will test if each numeric
variable can be coerced to a factor variable. It tests the potential
levels of each variable and coerce to a factor type if the number of
levels is moderate (i.e \< 10). For variable with two levels, method
used will be “bino”, else it will be
“cate”.

``` r
# In mtcars, "cyl", "vs", "am", "gear" and "carb" are encoded as numeric but they are factors in reality.
# tabkris_2 changes each variable and display a message for each transformation.
desctable <- tabkris_2(mtcars, auto_detect = T)
# > Variable "cyl" have been coerced to factor, with method "cate"
# > Variable "vs" have been coerced to factor, with method "bino"
# > Variable "am" have been coerced to factor, with method "bino"
# > Variable "gear" have been coerced to factor, with method "cate"
# > Variable "carb" have been coerced to factor, with method "cate"


knitr::kable(desctable)
```

| Variable | Modality | N  | Statistics      |
| :------- | :------- | :- | :-------------- |
| mpg      |          | 32 | 20.09 (6.03)    |
| cyl      |          | 32 |                 |
|          | 4        |    | 11 (34.38%)     |
|          | 6        |    | 7 (21.88%)      |
|          | 8        |    | 14 (43.75%)     |
| disp     |          | 32 | 230.72 (123.94) |
| hp       |          | 32 | 146.69 (68.56)  |
| drat     |          | 32 | 3.6 (0.53)      |
| wt       |          | 32 | 3.22 (0.98)     |
| qsec     |          | 32 | 17.85 (1.79)    |
| vs       | 1        | 32 | 14 (43.75%)     |
| am       | 1        | 32 | 13 (40.62%)     |
| gear     |          | 32 |                 |
|          | 3        |    | 15 (46.88%)     |
|          | 4        |    | 12 (37.5%)      |
|          | 5        |    | 5 (15.62%)      |
| carb     |          | 32 |                 |
|          | 1        |    | 7 (21.88%)      |
|          | 2        |    | 10 (31.25%)     |
|          | 3        |    | 3 (9.38%)       |
|          | 4        |    | 10 (31.25%)     |
|          | 6        |    | 1 (3.12%)       |
|          | 8        |    | 1 (3.12%)       |

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
desc_prep <- tabkris_2(mtcars, return_table = F, auto_detect = T)
# > Variable "cyl" have been coerced to factor, with method "cate"
# > Variable "vs" have been coerced to factor, with method "bino"
# > Variable "am" have been coerced to factor, with method "bino"
# > Variable "gear" have been coerced to factor, with method "cate"
# > Variable "carb" have been coerced to factor, with method "cate"

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
                       lang = "fr",
                       auto_detect = T)
# > Variable "cyl" have been coerced to factor, with method "cate"
# > Variable "vs" have been coerced to factor, with method "bino"
# > Variable "am" have been coerced to factor, with method "bino"
# > Variable "gear" have been coerced to factor, with method "cate"
# > Variable "carb" have been coerced to factor, with method "cate"

knitr::kable(desctable)
```

| Variable            | Modalité | N  | Statistiques             |
| :------------------ | :------- | :- | :----------------------- |
| Miles/US gallon     |          | 32 | 20.1 (6) {10.4;33.9}     |
|                     | NA       | 0  |                          |
| Number of cylinders |          | 32 |                          |
|                     | 4        |    | 11/32 (34.4%)            |
|                     | 6        |    | 7/32 (21.9%)             |
|                     | 8        |    | 14/32 (43.8%)            |
|                     | NA       | 0  |                          |
| Displacement        |          | 32 | 230.7 (123.9) {71.1;472} |
|                     | NA       | 0  |                          |
| Horsepower          |          | 32 | 146.7 (68.6) {52;335}    |
|                     | NA       | 0  |                          |
| Rear axle ratio     |          | 32 | 3.6 (0.5) {2.8;4.9}      |
|                     | NA       | 0  |                          |
| Weight              |          | 32 | 3.2 (1) {1.5;5.4}        |
|                     | NA       | 0  |                          |
| 1/4 mile time       |          | 32 | 17.8 (1.8) {14.5;22.9}   |
|                     | NA       | 0  |                          |
| Engine              | 1        |    | 14/32 (43.8%)            |
|                     | NA       | 0  |                          |
| Transmission        | 1        |    | 13/32 (40.6%)            |
|                     | NA       | 0  |                          |
| N Forward gears     |          | 32 |                          |
|                     | 3        |    | 15/32 (46.9%)            |
|                     | 4        |    | 12/32 (37.5%)            |
|                     | 5        |    | 5/32 (15.6%)             |
|                     | NA       | 0  |                          |
| N carburetors       |          | 32 |                          |
|                     | 1        |    | 7/32 (21.9%)             |
|                     | 2        |    | 10/32 (31.2%)            |
|                     | 3        |    | 3/32 (9.4%)              |
|                     | 4        |    | 10/32 (31.2%)            |
|                     | 6        |    | 1/32 (3.1%)              |
|                     | 8        |    | 1/32 (3.1%)              |
|                     | NA       | 0  |                          |

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
