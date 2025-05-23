#' Lincoln-Petersen Simulation
#'
#' Simulate data for a closed, 2-sample mark/recapture study
#'
#' @param N Simulated population size
#' @param mark the number of individuals marked or the probability an individual
#'   is caught and marked during the initial sample period.
#' @param recap the probability an individual is encountered in the second
#'   sample period
#' @param sample.fixed is the sample size fixed or not. If TRUE, `mark` is a
#'   number >1 and represents a number of known marked individuals released, otherwise, it's a probability (0,1)
#'
#' @returns A list containing:
#'
#'  \item{r}{the number of individuals marked in the first sample period}
#'
#'  \item{n}{the number of individuals in the second sample.}
#'
#'  \item{m}{the number of previously marked individuals in the second sample.}
#'
#' @examples
#' # Two examples if capture is simulated as a fixed number of individuals, or a probability
#' \dontrun{
#'
#'
#'   LP.sim(N=5E4, mark=300, recap=0.2, sample.fixed=TRUE)->sim1
#'
#'   LP.sim(N=5E4, mark=0.3, recap=0.2, sample.fixed=FALSE)->sim2
#' }
#' @export
LP.sim<-function(N,mark,recap,sample.fixed=FALSE){

  if(sample.fixed==FALSE){
  r1<-rbinom(N,1,mark)
  n1<-rbinom(N,1,recap)
  r.out<-sum(r1)
  n.out<-sum(n1)
  m.out<-sum(r1*n1)
  }

  if(sample.fixed==TRUE){
    r2<-c(rep(1,mark),rep(0,N-mark))
    n2<-rbinom(N,1,recap)
    m.out<-sum(r2*n2)

    r.out<-mark
    n.out<-sum(n2)
  }

  out<-list(r=r.out,
            n=n.out,
            m=m.out)
return(out)
}
'LP.sim'


#' Chapman Estimator for closed populations
#'
#' Uses Chapman's modified version of the Lincoln-Petersen estimator for a
#' two-sample closed population abundance estimator.
#'
#' @param r the number of individuals marked in the initial sample.
#' @param n the total number of individuals (marked and unmarked) encountered in
#'   the second sample.
#' @param m the number of marked individuals encountered in the second sample
#'
#' @returns A list containing:
#'
#'  \item{N.hat}{the abundance estimate}
#'
#'  \item{SE}{the standard error of the abundance estimate}
#'
#' @examples
#' # Use simulation code to generate data and analyze it
#' \dontrun{
#'
#'
#'  LP.sim(N=5E4, mark=300, recap=0.2,sample.fixed=TRUE)->sim.dat
#'
#'
#'  chapman(r=sim.dat$r,
#'           n=sim.dat$n,
#'           m=sim.dat$m)
#' }
#' @export
chapman<-function(r,n,m){

  SE.chap<-sqrt(((r+1)*(n+1)*(r-m)*(n-m))/(((m+1)^2)*(m+2)))
  N.chap<-(((r+1)*(n+1))/(m+1))-1


  out<-list(N.hat=N.chap,
            SE=SE.chap)

  return(out)
}
'chapman'
