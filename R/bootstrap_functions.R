


#' Resampling helper function
#'
#'@description
#' Function called internally in boot_cluster to resample observations within
#' a cluster. It uses a vector of resampled "clusters: (tmp.name), pulls just
#' observations from a given cluster in the original data (tmp.dat) and returns
#' a data frame of the resampled observations from a cluster. This is done
#' repeatedly within boot_cluster and the results are combined to produce a
#' single bootstrapped data set.
#'
#' @param tmp.dat
#' the data to be bootstrapped, contains a "cluster" variable created in
#' boot_cluster
#' @param tmp.name
#' The cluster value, used to subset data before resampling from observations
#' within the specified cluster.
#' @returns
#' a data frame of resampled observations from within a single cluster
#' @export
#'
#'
resamp.fxn<-function(tmp.dat,tmp.id){

  tmp.id%>%
    pull(cluster)%>%
    map_dfr(.,~filter(tmp.dat,cluster==.x)%>%
              slice_sample(.,prop=1,replace=TRUE)
    )

}



#' Resample clustered data
#'
#' This function defines the cluster variables, then resamples from the unique
#' clusters N times, where N is the number of unique clusters. Observations
#' within each cluster are also resampled.
#'
#'
#' @param dat The original data to be bootstrapped (does not contain a variable
#'   named "cluster").
#' @param cluster.vars An unquoted vector of column names used to construct a
#'   cluster. Clusters just identify groups of observations that should be
#'   sampled together. Not all clusters may appear in a bootstrapped data. If
#'   it's essential that a grouping variable be
#' @param design.vars An unquoted vector of column names used to construct a
#'   cluster. An unquoted vector of column names.
#' @param out.folder An optional quoted string that gives the path to an
#'   existing folder where bootstrapped data objects should be written, if
#'   written to file. If not provided, an object or list of objects is returned
#'   in the environement.
#' @param i a counter that will be written to the name of each bootstrapped data
#'   (e.g bootdat_1.rds, dat_2.rds) if data objects are written to file
#'
#' @returns writes file(s) of bootstrapped data to a folder, if specified, or as
#'   a list to an object in the local environment
#' @export
#'
#' @examples
#'
#'
#' dat<-data.frame(Y=rnorm(30,0,1),
#'                 A=rep(c('a1','a2','a3'),each=10))
#'
#' # Run it using purrr. Use walk because this writes to a folder
#'
#' # purrr::walk(1:5,
#' # ~boot_cluster(dat,
#' #           cluster.vars=c(A),
#' #          out.path="./bootdata/",
#' #          i=.x))
#'
#' # Writing to an object in memory
#'
#' # purrr::map(1:5,
#' #            ~boot_cluster(dat,
#' #           cluster.vars=c(A)))->output.out
#'
#' #Create one bootstrapped data set
#'
#' # boot_cluster(dat,cluster.vars=c(A))->output.out
#'
#'
boot_cluster<-function(dat,
                       design.vars,
                       cluster.vars,
                       out.folder,
                       i){

  library(tidyverse)

  if(missing(design.vars)){
    tidyr::unite(dat,col='cluster',
                 {{cluster.vars}},
                 remove=FALSE)->tmp

    tmp%>%
      distinct(cluster)%>%
      list(.)->tmp2


  }else{

    tidyr::unite(dat,col='cluster',
                 {{cluster.vars}},
                 remove=FALSE)%>%
      tidyr::unite(.,col='design',
                   {{design.vars}}, remove=FALSE)->tmp

    tmp%>%
      group_by(design)%>%
      distinct(cluster)%>%
      ungroup()%>%
      split(.$design)->tmp2
  }

  map(tmp2,
      ~slice_sample(.x,n=nrow(.x),replace=TRUE))->tmp3


  map(tmp3,~resamp.fxn(tmp.dat=tmp,tmp.id=.x))%>%
    bind_rows(.)->boot.dat


  if(missing(out.folder)){

    return(boot.dat)

  }else{

    saveRDS(boot.dat,file=paste0(out.folder,'/bootdat_',i,'.rds'))

  }
}
