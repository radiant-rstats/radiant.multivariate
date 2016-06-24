#' Deprecated function(s) in the radiant.multivariate package
#'
#' These functions are provided for compatibility with previous versions of
#' radiant. They will eventually be  removed.
#' @rdname radiant.multivariate-deprecated
#' @name radiant.multivariate-deprecated
#' @param ... Parameters to be passed to the updated functions
#' @docType package
#' @export  save_factors save_membership
#' @aliases save_factors save_membership
#' @section Details:
#' \tabular{rl}{
#'   \code{save_factors} is now a synonym for \code{\link{store.full_factor}}\cr
#'   \code{save_membership} is now a synonym for \code{\link{store.kmeans_clus}}\cr
#' }
#'
save_factors <- function(...) {
  .Deprecated("store.full_factor", package = "radiant.multivariate")
  store.full_factor(...)
}
save_membership <- function(...) {
  .Deprecated("store.kmeans_clus", package = "radiant.multivariate")
  store.kmeans_clus(...)
}
NULL
