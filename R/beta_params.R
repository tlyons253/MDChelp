#' Convert a probability and SD to beta distribution parameters
#'
#' @param mu
#' @param sd
#'
#' @returns
#' A named vector including alpha and beta parameters
#' @export
#'
#'
get_betadist<-function(mu, sd){

  var<-sd*sd

  alpha<-(((1-mu)/var)-(1/mu)*(mu*mu))

  beta<-alpha*((1/mu)-1)

  out<-c(alpha=alpha,beta=beta)

  if(any(out < 0)){
  warning("invalid negative parameter values returned")
    return(out)
  }
  return(out)
}

