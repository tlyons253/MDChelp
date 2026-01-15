
#' Estimate fish exploitation rate
#'
#' @param release the number of tagged or marked fish releases
#' @param recover the number of tags recovered
#' @param tag.loss the probability a tag or other mark is lost
#' @param report.rate the probability a harvested, marked fish is reported
#'
#' @returns the estimated exploitation rate (harvest probability)
#' @export
#'
#' @examples
#'
#' exploit_fish(release=100,
#'              recover=10,
#'              tag.loss=0.1,
#'              report.rate=0.75)
#'
#'
exploit_fish<-function(release,recover,tag.loss,report.rate){

  recover/(release*(1-tag.loss)*report.rate)->E

  return(E)
}

