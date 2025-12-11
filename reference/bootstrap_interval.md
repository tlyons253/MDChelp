# Compute bias-corrected, accelerated confidence intervals for non-parametric bootstrapped parameter estimates.

Compute bias-corrected, accelerated confidence intervals for
non-parametric bootstrapped parameter estimates.

## Usage

``` r
bootstrap_interval(
  boot.est,
  OG.dat,
  est.fxn,
  alpha,
  multcomp = TRUE,
  type = c("normal", "BC", "BCa")
)
```

## Arguments

- boot.est:

  A data frame with n x p rows (n= \# of bootstrap replicates, p= \# of
  model terms or parameters and two columns: one for the parameter name
  (term), and one for the parameter estimate (estimate))

- OG.dat:

  The orignal data set

- est.fxn:

  The summary function to be performed on the data. See `est_accelerate`
  for a more detailed description of what this function needs to return.

- alpha:

  The desired 2-tail alpha level

- multcomp:

  If true, the resulting table will have adjusted CI's with the
  family-wide error rate at the specified alpha level. If FALSE, the
  error rate is applied to each parameter. Correction using the Sidak
  method is instituted by default. This is only appropriate for when you
  are looking for a table of regression coefficients and their
  bootstrapped CI's.

## Value

A data frame containing the parameter name (term), theta.hat, the
estimate of the parameter from the orignal data, and the lower and upper
BCa confidence intervals

## Examples

``` r
# data.frame(X1=seq(1,10,1),
#            X2=rnorm(10,0,1))%>%
#   mutate(Y=0.2*X1+X2)%>%
#   select(-X2)->tmp
#
# Create bootstrapped data sets
#
# purrr::map(1:10,~sample(1:nrow(tmp),size=nrow(tmp),replace=TRUE)%>%
#               tmp[.,])->boot.dat
#
#
# my.fxn1<-function(X){
#   lm(Y~X1,data=X)%>%
#     broom::tidy(.)%>%
#     select(term,estimate)
# }
#
#
# #this function above could look different every time depending on bootstrap
# #approach  but should return a 2-column dataframe with columns term- the
# #parameter name, and estimate. You do not need to use a function here, I
# #just did for convenience. You just need to get the bootstrapped parameter
# #estimates in the correct format.
#
# est_boot<-function(est.fxn,boot.list){
#
#   map(boot.list,~est.fxn(.x))%>%
#     bind_rows()%>%
#     rename(theta.boot=estimate)->out
#
#   return(out)
#
# }
#
#
# est_boot(my.fxn1,boot.dat)->boot.out
#
#
# bootstrap_interval(boot.out,tmp,my.fxn1,alpha=0.05,multcomp = FALSE)


```
