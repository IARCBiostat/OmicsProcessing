#' Register a parallel cluster for parallel processing.
#'
#' This function creates and registers a parallel cluster for parallel processing in R.
#' 
#' @param num_cores The number of CPU cores to be used in the parallel cluster.
#'
#' @return A parallel cluster object registered for parallel processing.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' cluster <- register_cluster(4)
#' # Use the registered cluster for parallel processing
#' }
register_cluster <- function(num_cores) {
    cluster <- parallel::makeCluster(num_cores)
    doParallel::registerDoParallel(cluster)
    return(cluster)
}

#' Unregister a parallel cluster used for parallel processing.
#'
#' This function unregisters a previously registered parallel cluster used for parallel processing in R.
#' 
#' @param cluster The parallel cluster object to be unregistered (default is NULL).
unregister_cluster <- function(cluster = NULL) {
    env <- foreach:::.foreachGlobals
    rm(list = ls(name = env), pos = env)
}
