


#' tidy unmarked parameter estimates
#'
#' @param X an unmarked fit object
#'
#' @returns A data frame of parameter estimates similar to what broom::tidy()
#'   does for lm/glm objects
#'
#' @export
tidy.unm<-function(X){
  X@estimates@estimates->param.list

  map(param.list,~um.help(.x))%>%bind_rows()%>%as.tibble()->tmp

  row.names(tmp)<-NULL

  return(tmp)}
#'
#' @examples
#' No
'tidy.unm'


#
#'Formatting for a single parameter
#'
#' @param X would be one slot from within unmarkedFit@estimates list
#'
#' @returns a data.frame of the proccess, parameter, SE and approximate Z and P
#'   values (assuming normal distribution)
#'
#' @export
 um.help<-function(X){
data.frame(process=rep(X@name,length(X@estimates)),
           shorty=rep(X@short.name,length(X@estimates)),
           estimate=X@estimates,
           SE=sqrt(diag(X@covMat)))%>%
  mutate(approximate.Z=estimate/SE,
         approximate.P=pnorm(abs(approximate.Z),
                             lower.tail=FALSE)*2)%>%
  rownames_to_column(var='term')%>%
  unite(col='parameter',shorty,term)%>%
  relocate(process,parameter)
}
#'
#' @examples
#' No
'um.help'
