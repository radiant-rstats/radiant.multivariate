#' Deprecated function(s) in the radiant.multivariate package
#'
#' These functions are provided for compatibility with previous versions of
#' radiant. They will eventually be  removed.
#' @rdname radiant.multivariate-deprecated
#' @name radiant.multivariate-deprecated
#' @param ... Parameters to be passed to the updated functions
#' @docType package
#' @export  kmeans_clus hier_clus pmap
#' @aliases kmeans_clus hier_clus pmap
#' @section Details:
#' \tabular{rl}{
#'   \code{kmeans_clus} is now a synonym for \code{\link{kclus}}\cr
#'   \code{hier_clus} is now a synonym for \code{\link{hclus}}\cr
#'   \code{pmap} is now a synonym for \code{\link{prmap}}\cr
#' }
#'
kmeans_clus <- function(...) {
  .Deprecated("kclus", package = "radiant.multivariate")
  kclus(...)
}
hier_clus <- function(...) {
  .Deprecated("hclus", package = "radiant.multivariate")
  hclus(...)
}
pmap <- function(...) {
  .Deprecated("prmap", package = "radiant.multivariate")
  prmap(...)
}
NULL
