# Generate a ragged observation matrix.

Ragged observation matrix of 1 and 0. Create noise in observations/
visits like nest monitoring, but separate from a detection probability.
With a window looking back up to D days, the probability of an
observation on day j is visitP^(# of visits within previous D days x 3).
This results in a decrease in the probability of a visit if there have
been several visits recently, but increases to maxP, if there have not
been any.

## Usage

``` r
ragged.obs(visitP, maxD, nind, ntime)
```

## Arguments

- visitP:

  the probability of a visit when no other visits have occurred within
  the past D days

- maxD:

  the window to look back over. It will end up being close to the
  average "window" of non-observations in any individuals encounter
  history

- nind:

  the number of individuals

- ntime:

  the length of the encounter histories

## Value

A matrix of 1 and 0 indicating a visit
