# Jacknife data helper function

A function to create a list of jacknifed data objects. Used internally
elsewhere but available here in case users wish to write jacknifed data
objects to file

## Usage

``` r
jacknife_data(X)
```

## Arguments

- X:

  A data frame, the original data

## Value

A list of nrow(X) data frames, each being nrow(X)-1 rows; A list of
jacknifed data sets to be passed to est_accelerate()

## Examples

``` r
# data.frame(X1=seq(1,10,1),
# X2=rnorm(10,0,1))%>%
# mutate(Y=0.2*X1+X2)%>%
# select(-X2)->tmp

# jacknife.out<-jacknife_data(tmp)
```
