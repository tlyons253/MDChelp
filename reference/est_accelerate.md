# Jacknife procedure to estimate the acceleration constant

A function called internal in `bca_jacknife` to compute the acceleration
constant for each parameter of interest.

## Usage

``` r
est_accelerate(OG.dat, dat.list, est.fxn)
```

## Arguments

- OG.dat:

  The orignal data set, a data frame.

- dat.list:

  A list of Jacknifed data sets. Generated internally in `bca_jacknife`

- est.fxn:

  A user-supplied function that performs the desired operation on the
  data. Typically a call to `lm`, `lmer`, etc. or any object that can
  then be coerced into a data.frame similar to that returned by
  [`broom::tidy`](https://generics.r-lib.org/reference/tidy.html) (or
  `broom.mixed::tidy`).

  Must return a data.frame with columns "term" and "estimate",
  representing the parameter name, and it's estimate.

## Value

A list comprised of 2 data frames. The first data frame has the first
column (term) indicating each parameter and the second column (a), the
acceleration constant for each term.
