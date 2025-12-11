# Resampling helper function

Function called internally in boot_cluster to resample observations
within a cluster. It uses a vector of resampled "clusters: (tmp.name),
pulls just observations from a given cluster in the original data
(tmp.dat) and returns a data frame of the resampled observations from a
cluster. This is done repeatedly within boot_cluster and the results are
combined to produce a single bootstrapped data set.

## Usage

``` r
resamp.fxn(tmp.dat, tmp.id)
```

## Arguments

- tmp.dat:

  the data to be bootstrapped, contains a "cluster" variable created in
  boot_cluster

- tmp.name:

  The cluster value, used to subset data before resampling from
  observations within the specified cluster.

## Value

a data frame of resampled observations from within a single cluster
