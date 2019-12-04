
<!-- README.md is generated from README.Rmd. Please edit that file -->

# YPJ

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/Ygall/YPJ.svg?branch=master)](https://travis-ci.org/Ygall/YPJ)
<!-- badges: end -->

Fonctions utilitaires personnelles Le but de ce package est de réunir au
même endroit les fonctions utilitaires pour les analyses de recherche
clinique.

## Fonctions disponibles

### tabkris\_2

Fonction de description

## Installation

La version publiée d’YPJ peut être installée à partir de
[CRAN](https://CRAN.R-project.org) avec :

``` r
install.packages("YPJ")
```

La version de développement est disponible sur
[GitHub](https://github.com/) avec :

``` r
# install.packages("devtools")
devtools::install_github("Ygall/YPJ")
```

## Exemple

``` r
library(YPJ)

desctable <- tabkris_2(boys)
desctable
#>    Variable Modality   N      Statistics
#> 1       age          748    9.16 (6.89) 
#> 2       hgt          728 132.15 (46.51) 
#> 3       wgt          744  37.15 (26.03) 
#> 4       bmi          727   18.07 (3.05) 
#> 5        hc          702   51.51 (5.91) 
#> 6       gen          245                
#> 7                 G1         56 (22.86%)
#> 8                 G2         50 (20.41%)
#> 9                 G3          22 (8.98%)
#> 10                G4         42 (17.14%)
#> 11                G5         75 (30.61%)
#> 12      phb          245                
#> 13                P1         63 (25.71%)
#> 14                P2         40 (16.33%)
#> 15                P3          19 (7.76%)
#> 16                P4         32 (13.06%)
#> 17                P5         50 (20.41%)
#> 18                P6         41 (16.73%)
#> 19       tv          226   11.89 (7.99) 
#> 20      reg          745                
#> 21             north         81 (10.87%)
#> 22              east        161 (21.61%)
#> 23              west        239 (32.08%)
#> 24             south        191 (25.64%)
#> 25              city           73 (9.8%)
```
