# logistic exposure link function.

This is the logistic exposure link to use with glm's in R It is sourced
from Ben Bolker's website : https://rpubs.com/bbolker/logregexp

## Usage

``` r
logexp(exposure = 1)
```

## Arguments

- exposure:

  The length of time. defaults to 1 unit.

## Examples

``` r
# Simulate binomial survival data and estimate dsr

if (FALSE) { # \dontrun{

# create dummy data
n.ind<-30 # number of individuals X intervals
dsr<-0.9 # simulated daily survival rate
expose<-sample(c(1,2,3),n.ind,replace=TRUE) # simulate the exposure interval length

Y<-rbinom(n.ind,1,dsr^expose) #observed survival

demo.dat<-data.frame(Y=Y,expose=expose)


mod<-glm(Y~1,
        family=binomial(link=MDChelp::logexp(demo.dat$expose)),
        data=demo.dat)


 predict.dat<-data.frame(Y=1,expose=1)


 predict(mod,predict.dat,type='link',se.fit=TRUE)
 # doesn't work with type='response' and 'newdat'

} # }
```
