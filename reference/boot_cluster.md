# Resample clustered data

This function defines the cluster variables, then resamples from the
unique clusters N times, where N is the number of unique clusters.
Observations within each cluster are also resampled.

## Usage

``` r
boot_cluster(dat, design.vars, cluster.vars, out.folder, i)
```

## Arguments

- dat:

  The original data to be bootstrapped (does not contain a variable
  named "cluster").

- design.vars:

  An unquoted vector of column names used to construct a cluster. An
  unquoted vector of column names.

- cluster.vars:

  An unquoted vector of column names used to construct a cluster.
  Clusters just identify groups of observations that should be sampled
  together. Not all clusters may appear in a bootstrapped data. If it's
  essential that a grouping variable be

- out.folder:

  An optional quoted string that gives the path to an existing folder
  where bootstrapped data objects should be written, if written to file.
  If not provided, an object or list of objects is returned in the
  environement.

- i:

  a counter that will be written to the name of each bootstrapped data
  (e.g bootdat_1.rds, dat_2.rds) if data objects are written to file

## Value

writes file(s) of bootstrapped data to a folder, if specified, or as a
list to an object in the local environment

## Examples

``` r

dat<-data.frame(Y=rnorm(30,0,1),
                A=rep(c('a1','a2','a3'),each=10))

# Run it using purrr. Use walk because this writes to a folder

# purrr::walk(1:5,
# ~boot_cluster(dat,
#           cluster.vars=c(A),
#          out.path="./bootdata/",
#          i=.x))

# Writing to an object in memory

# purrr::map(1:5,
#            ~boot_cluster(dat,
#           cluster.vars=c(A)))->output.out

#Create one bootstrapped data set

# boot_cluster(dat,cluster.vars=c(A))->output.out

```
