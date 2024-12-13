
#' A simple survival simulation
#'
#' simulate logistic exposure survival data, returns a "long"
#' object with n.individuals X n observations rows or a 'wide' object for
#' a different analysis method. permits right censoring
#'
#' @param S.int the interval survival probability
#' @param nind the number of individuals
#' @param ntime the length of the encounter histories
#' @param p.obs  a probability to create differences in exposure period
#' @param obs.start TRUE or FALSE, if true, all individuals are observed at
#'                time =1, otherwise. all individuals may not be observed until
#'                later in their encounter history, or not at all before failing
#' @param r.censor TRUE or FALSE, the probability an individual is right censored.
#'                The function defaults to having all individuals observed at
#'                the last time interval (as is typical in nest survival studies)
#' @param p.censor the probability an individual is right censored before time=ntime
#' @param t.censor controls when censoring occurs by treating censoring time as a binomial
#'                process where the number of trials is the length of the encounter history
#'                and t.censor is the probability, an the resulting number of successes
#'                is the interval at which censoring occurs
#' @param cjs  if TRUE, the encounter history is output in a matrix typical of CJS matrix, suitable
#'            for use in a bayesian frameowrk
#' @param logexp of TRUE, returns a data frame of rows of individual observation
#'                 intervals for use in glm or similar
#' @export
#'

logexp.sim.simple<-function(S.int=0.95,
                         nind=10,
                         ntime=10,
                         p.obs=0.5,
                         obs.start=TRUE,
                         r.censor=FALSE,
                         p.censor=0.1,
                         t.censor=0.8,
                         cjs=FALSE,
                         logexp=TRUE){
  library(tidyverse)
  S.mat<-matrix(NA,
                nrow=nind,
                ncol=ntime)
  p.mat<-matrix(data=rbinom(n=nind*ntime,1,p.obs),
                nrow=nind,
                ncol=ntime)
  S.mat[,1]<-1

  for(i in 1:nind){
    for(t in 2:ntime){
      S.mat[i,t]<-rbinom(1,1,S.int)*S.mat[i,t-1]
    }
  }
S.mat[S.mat==0]<- -1

if(obs.start==TRUE){
p.mat[,1]<-1

}

p.mat[,ntime]<-1

if(r.censor==TRUE){

is.censor<-rbinom(nind,1,p.censor)
censor.time<-rbinom(nind,ntime,t.censor)

censor.obs<-is.censor*censor.time

for(i in which(censor.obs>0)){
  p.mat[i,censor.time[i]:ntime]<-0
}
}

obs.mat<-p.mat*S.mat


if(logexp==TRUE){
as.data.frame(obs.mat)%>%
  rownames_to_column(var='id')%>%
  pivot_longer(cols=2:(ntime+1),
               names_to='time',
               values_to='obs')%>%
  mutate(time=as.numeric(str_remove(time,'V')))%>%
  filter(obs!=0)%>%
  group_by(id)%>%
  mutate(exposure=time-lag(time),
         flag=obs*lag(obs))%>%
  ungroup()%>%
  filter(!is.na(exposure),
         (obs==1 |(obs==-1 & flag==-1)))%>%
  select(-flag)%>%
  mutate(obs=ifelse(obs==-1,0,1))->sim.dat
  return(sim.dat)
}
if(cjs==TRUE){
  cjs.mat<-obs.mat
  cjs.mat[cjs.mat==0]<-NA
  cjs.mat[cjs.mat==-1]<-0
  return(cjs.mat)
}
}


#' @examples
#' \dontrun{

#' logexp.sim.simple(r.censor=TRUE,p.censor=0.2)->test
#' data.frame(test)
#' }
#'
'logexp.sim.simple'
