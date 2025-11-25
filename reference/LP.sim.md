# Lincoln-Petersen Simulation

Simulate data for a closed, 2-sample mark/recapture study

## Usage

``` r
LP.sim(N, mark, recap, sample.fixed = FALSE)
```

## Arguments

- N:

  Simulated population size

- mark:

  the number of individuals marked or the probability an individual is
  caught and marked during the initial sample period.

- recap:

  the probability an individual is encountered in the second sample
  period

- sample.fixed:

  is the sample size fixed or not. If TRUE, `mark` is a number \>1 and
  represents a number of known marked individuals released, otherwise,
  it's a probability (0,1)

## Value

A list containing:

- r:

  the number of individuals marked in the first sample period

- n:

  the number of individuals in the second sample.

- m:

  the number of previously marked individuals in the second sample.

## Examples

``` r
# Two examples if capture is simulated as a fixed number of individuals, or a probability
if (FALSE) { # \dontrun{


  LP.sim(N=5E4, mark=300, recap=0.2, sample.fixed=TRUE)->sim1

  LP.sim(N=5E4, mark=0.3, recap=0.2, sample.fixed=FALSE)->sim2
} # }
```
