Last updated 2024-12-23

# MDChelp

The goal of MDChelp is to provide routine functions and datasets for MDC
biometricians and scientists and provide help/demonstrations of
functions, analysis, or workflows as well as background on analysis
methods that may be useful when developing projects.

## Installation

You can install the MDChelp from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tlyons253/MDChelp")
```

## Examples

Look in the reference section and the articles to see what is currently
contained in the package. The example below uses a simple logistic
exposure analysis simulation (`logexp.sim.simple()`) and the anlysis
using `GLM`. A custom link function, created by Ben Bolker is included
in this package as well.

This is for us plebes who still use MLE to solve models and don’t do
Bayesian everything…

``` r
# using MDChelp:: in front of functions is just a more explicit way to call the 
# function and avoids conflicts with other packages.

library(tidyverse)
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.4     ✔ readr     2.1.5
#> ✔ forcats   1.0.0     ✔ stringr   1.5.1
#> ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
#> ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
#> ✔ purrr     1.0.2     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

``` r


MDChelp::logexp.sim.simple(S.int=0.95,
                           nind=50,
                           ntime=25,
                           p.obs=0.5,
                           obs.start = TRUE,
                           r.censor = FALSE,
                           logexp=TRUE)->sim.dat



glm(obs~1,
    family=binomial(
      link=MDChelp::logexp(exposure=sim.dat$exposure)),
    data=sim.dat)->mod1

broom::tidy(mod1)%>%
  pull(estimate)%>%plogis(.) # should match S.int
#> [1] 0.9379698
```

``` r

glm(obs~time, # not in the data-generating process
    family=binomial(
      link=MDChelp::logexp(exposure=sim.dat$exposure)),
    data=sim.dat)->mod2

broom::tidy(mod2)
#> # A tibble: 2 × 5
#>   term        estimate std.error statistic  p.value
#>   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
#> 1 (Intercept)   3.22      0.349       9.22 3.05e-20
#> 2 time         -0.0411    0.0238     -1.73 8.37e- 2
```

``` r

# can also use it in a mixed model but won't work with the simple simulation data
```
