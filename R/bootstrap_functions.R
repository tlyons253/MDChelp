


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
resamp.fxn<-function(tmp.dat,tmp.name){
  tmp.dat%>%
    filter(cluster==tmp.name)->tmp2

  sample(1:nrow(tmp2),size=nrow(tmp2),replace=TRUE)->idx
  tmp2[idx,]

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
#'   cluster. An unquoted vector of column names.
#' @param out.folder A quoted string that gives the path to an existing folder
#'   where bootstrapped data objects should be written.
#' @param i a counter that will be written to the name of each bootstrapped data
#'   (e.g bootdat_1.rds, dat_2.rds)
#'
#' @returns Writes an RDS file containing one bootstrapped data set.
#' @export
#'
#' @examples
#'
#' # Run it using purrr. Use walk because this writes to a folder
#'
#' dat<-data.frame(Y=rnorm(30,0,1),
#'                 A=rep(c('a1','a2,'a3'),each=10))
#'
#'
#' # tictoc::tic()
#'
#' # purrr::walk(seq(1,5),
#' # ~boot_cluster(dat,
#'            cluster.vars=c(A),
#'            out.path="./bootdata/",
#'            i=.x))
#'
#' # tictoc::toc()
#'
#' # Can also set it up to run in a loop if preferred.
#' #   Use tictoc to compare speed
#'
#'
#'
#' #tictoc::tic()
#'
#' # for (i in 1:5){
#'
#' boot_cluster(dat,
#'         cluster.vars=c(A),
#'         out.path="./bootdata/",
#'         i=i))
#' }
#'
#' #tictoc::toc()
#'
#'
boot_cluster<-function(dat,
                  cluster.vars,
                  out.folder,i){

  tidyr::unite(dat,col='cluster',
               {{cluster.vars}},
               remove=FALSE)->tmp

  distinct(tmp,cluster)%>%
    pull(cluster)->cluster.id

  sample(cluster.id,
         size=length(cluster.id),
         replace=TRUE)->boot.names


  map(boot.names,~resamp.fxn(tmp.dat=tmp,tmp.name=.x))%>%
    bind_rows(.)->boot.dat


  saveRDS(boot.dat,file=paste0(out.path,'/bootdat_',i,'.rds'))
}
