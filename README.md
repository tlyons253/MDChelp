Last updated Feb 20, 2025

<!-- README.md is generated from README.Rmd. Please edit that file -->

# MDChelp

<!-- badges: start -->
<!-- badges: end -->

The goal of MDChelp is to provide functions and demonstrations of common
statistical methods in fish/ wildlife research.

## Installation

You can install the development version of MDChelp from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("tlyons253/MDChelp")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(MDChelp)
## simulate data and estimate abundance using Chapman's version of a Lincoln-Peterson estimator

LP.sim(500,75,0.3,sample.fixed=TRUE)->sim.dat


chapman(sim.dat$r,
        sim.dat$n,
        sim.dat$m)
#> $N.hat
#> [1] 413.069
#> 
#> $SE
#> [1] 53.71808
```
