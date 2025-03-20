#' Jacknife data helper function
#'
#' A function to create a list of jacknifed data objects. Used internally
#' elsewhere but available here in case users wish to write jacknifed data
#' objects to file
#'
#' @param X
#' A data frame, the original data
#' @returns
#' A list of nrow(X) data frames, each being nrow(X)-1 rows;
#' A list of jacknifed data sets to be passed to est_accelerate()
#' @export
#'
#' @examples
#'
#' # data.frame(X1=seq(1,10,1),
#' # X2=rnorm(10,0,1))%>%
#' # mutate(Y=0.2*X1+X2)%>%
#' # select(-X2)->tmp
#'
#' # jacknife.out<-jacknife_data(tmp)
#'
jacknife_data<-function(X){

  N<-nrow(X)
 out<-list()

  for(i in 1:N){

    out[[i]]<-X[-i,]

  }
  return(out)

}



#' Jacknife procedure to estimate the acceleration constant
#'
#' A function called internal in `bca_jacknife` to compute the acceleration
#' constant for each parameter of interest.
#'
#' @param OG.dat
#' The orignal data set, a data frame.
#' @param dat.list
#' A list of Jacknifed data sets. Generated internally in `bca_jacknife`
#' @param est.fxn
#' A user-supplied function that performs the desired operation on the data.
#' Typically a call to `lm`, `lmer`, etc. or any object that can then be coerced
#' into a data.frame similar to that returned by `broom::tidy` (or
#' `broom.mixed::tidy`).
#'
#' Must return a data.frame with columns "term" and "estimate", representing
#' the parameter name, and it's estimate.
#' @returns
#' A list comprised of 2 data frames. The first data frame has the first column
#' (term) indicating each parameter and the second column (a), the acceleration
#' constant for each term.
#' @export
#'
est_accelerate<-function(OG.dat, dat.list,est.fxn){

  N<-nrow(OG.dat)


  map(dat.list,~est.fxn(.x))%>%
    bind_rows()%>%
    select(term,
           theta.jack=estimate)->jack.est

  est.fxn(OG.dat)%>%
    select(term,
           theta.hat=estimate)->OG.est

  left_join(jack.est,OG.est,
            by='term')%>%
    group_by(term)%>%
    mutate(I=(N-1)*(theta.hat-theta.jack))%>%
    summarize(a=(sum(I^3)/(sum(I^2)^1.5))/6)%>%
    ungroup()->out

return(list(out,OG.est))
}





# package in bca_jacknife -------------------------------------------------


#' Compute bias-corrected, accelerated confidence intervals for non-parametric
#' bootstrapped parameter estimates.
#'
#' @param boot.est
#' A data frame with n x p rows (n= # of bootstrap replicates,
#'   p= # of model terms or parameters and two columns: one for the parameter
#'   name (term), and one for the parameter estimate (estimate))
#' @param OG.dat
#'  The orignal data set
#' @param est.fxn
#' The summary function to be performed on the data. See
#'   `est_accelerate` for a more detailed description of what this function
#'   needs to return.
#' @param alpha
#' The desired 2-tail alpha level
#' @param multcomp
#' If true, the resulting table will have adjusted CI's with the
#'   family-wide error rate at the specified alpha level. If FALSE, the error
#'   rate is applied to each parameter. Correction using the Sidak method is
#'   instituted by default. This is only appropriate for when you are looking
#'   for a table of regression coefficients and their bootstrapped CI's.
#' @returns
#' A data frame containing the parameter name (term), theta.hat, the estimate of
#' the parameter from the orignal data, and the lower and upper BCa confidence
#' intervals
#' @export
#'
#' @examples
#'
#' # data.frame(X1=seq(1,10,1),
#'#            X2=rnorm(10,0,1))%>%
#'#   mutate(Y=0.2*X1+X2)%>%
#'#   select(-X2)->tmp
#'#
#'# Create bootstrapped data sets
#'#
#'# purrr::map(1:10,~sample(1:nrow(tmp),size=nrow(tmp),replace=TRUE)%>%
#'#               tmp[.,])->boot.dat
#'#
#'#
#'# my.fxn1<-function(X){
#'#   lm(Y~X1,data=X)%>%
#'#     broom::tidy(.)%>%
#'#     select(term,estimate)
#'# }
#'#
#'#
#'# #this function above could look different every time depending on bootstrap approach
#'# # but should return a 2-column dataframe with columns term- the parameter name,
#'# # and estimate. You do not need to use a function here, I just did for convenience.
#'# # You just need to get the bootstrapped parameter estimates in the correct format.
#'#
#'#
#'# est_boot<-function(est.fxn,boot.list){
#'#
#'#   map(boot.list,~est.fxn(.x))%>%
#'#     bind_rows()%>%
#'#     rename(theta.boot=estimate)->out
#'#
#'#   return(out)
#'#
#'# }
#'#
#'#
#'# est_boot(my.fxn1,boot.dat)->boot.out
#'#
#'#
#'# bca_jacknife(boot.out,tmp,my.fxn1,alpha=0.05,multcomp = FALSE)

#'
#'
#'
bca_jacknife<-function(boot.est,OG.dat,est.fxn,alpha,multcomp=TRUE){

  if(multcomp==TRUE){
  n_distinct(boot.est$term)->m

    alpha.use<-1-(1-alpha)^(1/m)

    message(paste0("CI's of individual parameters have a family-wide error rate",
                 "of ",alpha, " using the  Sidak method for multiple comparisons"))
  }
   else {

     alpha.use=alpha

message("CI's are unadjusted and DO NOT control the family-wide error rate")
   }





  FXN<-est.fxn

  jacknife_data(OG.dat)->jack.list

  est_accelerate(OG.dat,jack.list,est.fxn=FXN)->est.a





  boot.est%>%
    left_join(.,est.a[[2]],by='term')%>%
    mutate(ind=ifelse(theta.boot<=theta.hat,1,0))%>%
    group_by(term)%>%
    summarize(z0=mean(ind))%>%
    left_join(.,est.a[[1]],by='term')%>%
    mutate(q.lo=qnorm(alpha.use/2),
           q.hi=qnorm(1-alpha.use/2),
           adj.lo=pnorm(z0+(z0+q.lo)/(1-a*(z0+q.lo))),
           adj.hi=pnorm(z0+(z0+q.hi)/(1-a*(z0+q.hi))))%>%
    select(term,adj.hi,adj.lo)%>%
    split(.$term)%>%
    map(.,~select(.x,-term))%>%
    map(.,~matrix(sort((as.numeric(.x))),nrow=1,ncol=2))->adj.quant


  boot.est %>%
    split(.$term) %>%
    map(.,  ~ pull(.x,var=theta.boot)) %>%
    map2(., map(adj.quant,  ~ as.vector(.x)),  ~ quantile(.x, .y)) %>%
    map(.,  ~ unname(.x)) %>%
    map(.,  ~ matrix(., nrow = 1, ncol = 2)) %>%
    map(.,  ~ data.frame(.x)) %>%
    bind_rows(.id = 'term') %>%
    rename(CI.lo = 2, CI.hi = 3)%>%
    right_join(est.a[[2]],by='term')%>%
    select(term, theta.hat,CI.lo,CI.hi)->out

return(out)




}




