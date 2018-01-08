#' Launch radiant.multivariate in default browser
#'
#' @details See \url{https://radiant-rstats.github.io/docs} for documentation and tutorials
#'
#' @importFrom radiant.data launch
#'
#' @examples
#' \dontrun{
#' radiant.multivariate()
#' }
#' @export
radiant.multivariate <- function() radiant.data::launch(package = "radiant.multivariate", run = "browser")

#' Launch radiant.multivariate in the Rstudio viewer
#'
#' @details See \url{https://radiant-rstats.github.io/docs} for documentation and tutorials
#'
#' @importFrom radiant.data launch
#'
#' @examples
#' \dontrun{
#' radiant.multivariate_viewer()
#' }
#' @export
radiant.multivariate_viewer <- function() radiant.data::launch(package = "radiant.multivariate", run = "viewer")
