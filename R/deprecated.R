#' Deprecated function(s) in the radiant.multivariate package
#'
#' These functions are provided for compatibility with previous versions of
#' radiant. They will eventually be  removed.
#' @rdname radiant.multivariate-deprecated
#' @name radiant.multivariate-deprecated
#' @param ... Parameters to be passed to the updated functions
#' @docType package
#' @export  pmap
#' @aliases pmap
#' @section Details:
#' \itemize{
#'   \item Replace \code{pmap} by \code{\link{prmap}}
#' }
#'
pmap <- function(...) {
  # if ("package:purrr" %in% search()) {
  #   message("If you want to create an attribute-based perceptual map use prmap")
  #   purrr::pmap(...)
  # } else {
    .Deprecated("prmap", package = "radiant.multivariate")
    prmap(...)
  # }
}
NULL
