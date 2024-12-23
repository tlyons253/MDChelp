#' logistic exposure link function
#'
#' This is the logistic exposure link to use with glm's in R
#' It is sourced from Ben Bolker's website : https://rpubs.com/bbolker/logregexp
#' @param exposure The length of time. defaults to 1 unit.
#' @export

logexp <- function(exposure = 1) {
  ## hack to help with visualization, post-prediction etc etc
  get_exposure <- function() {
    if (exists("..exposure", env=.GlobalEnv))
      return(get("..exposure", envir=.GlobalEnv))
    exposure
  }
  linkfun <- function(mu) qlogis(mu^(1/get_exposure()))
  ## FIXME: is there some trick we can play here to allow
  ##   evaluation in the context of the 'data' argument?
  linkinv <- function(eta) plogis(eta)^get_exposure()
  logit_mu_eta <- function(eta) {
    ifelse(abs(eta)>30,.Machine$double.eps,
           exp(eta)/(1+exp(eta))^2)
  }
  mu.eta <- function(eta) {
    get_exposure() * plogis(eta)^(get_exposure()-1) *
      logit_mu_eta(eta)
  }
  valideta <- function(eta) TRUE
  link <- paste("logexp(", deparse(substitute(exposure)), ")",
                sep="")
  structure(list(linkfun = linkfun, linkinv = linkinv,
                 mu.eta = mu.eta, valideta = valideta,
                 name = link),
            class = "link-glm")
}



#' @examples
#' \dontrun{
#'
#' # create dummy data
#' n.ind<-30 # number of individuals X intervals
#' dsr<-0.9 # simulated daily survival rate
#' expose<-sample(c(1,2,3),n.ind,replace=TRUE) # simulate the exposure interval length
#'
#' Y<-rbinom(n.ind,1,dsr^expose) #observed survival
#'
#' demo.dat<-data.frame(Y=Y,expose=expose)
#'
#'
#' mod<-glm(Y~1,
#'         family=binomial(link=MDChelp::logexp(demo.dat$expose)),
#'         data=demo.dat)
#'
#'
#'  predict.dat<-data.frame(Y=1,expose=1)
#'
#'
#'  predict(mod,predict.dat,type='link',se.fit=TRUE)
#'  # doesn't work with type='response' and 'newdat'
#'           }
'logexp'
