# Chapman Estimator for closed populations

Uses Chapman's modified version of the Lincoln-Petersen estimator for a
two-sample closed population abundance estimator.

## Usage

``` r
chapman(r, n, m)
```

## Arguments

- r:

  the number of individuals marked in the initial sample.

- n:

  the total number of individuals (marked and unmarked) encountered in
  the second sample.

- m:

  the number of marked individuals encountered in the second sample

## Value

A list containing:

- N.hat:

  the abundance estimate

- SE:

  the standard error of the abundance estimate

## Examples

``` r
# Use simulation code to generate data and analyze it
if (FALSE) { # \dontrun{


 LP.sim(N=5E4, mark=300, recap=0.2,sample.fixed=TRUE)->sim.dat


 chapman(r=sim.dat$r,
          n=sim.dat$n,
          m=sim.dat$m)
} # }
```
