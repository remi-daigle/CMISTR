
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CMISTR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/CMISTR)](https://CRAN.R-project.org/package=CMISTR)
<!-- badges: end -->

The goal of CMISTR is to allow
[CMIST](https://www.bio.gc.ca/science/monitoring-monitorage/cmist/about-en.php)
assessments in R instead of relying on the MS Excel

## Installation

Until this package is released on [CRAN](https://CRAN.R-project.org),
please install with:

``` r
devtools::install_github("https://github.com/remi-daigle/CMISTR")
```

## Example

This is a basic example which shows you the package workflow:

``` r
library(CMISTR)
set.seed(11)
risks <- sample(x = c(1:3),size = 17,replace = TRUE)
uncertainties <- sample(x = c(1:3),size = 17,replace = TRUE)
score <- CMISTScore(risks,uncertainties)
score
#>   CMIST_Score CMIST_Upper CMIST_Lower Likelihood_Score Likelihood_Upper
#> 1    3.402167    4.166667    2.638889         1.569375            1.875
#>   Likelihood_Lower Impact_Score Impact_Upper Impact_Lower
#> 1             1.25     2.167889     2.444444     1.888889
```
