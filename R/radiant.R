#' Launch radiant.multivariate in the default browser
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

#' Launch radiant.multivariate in an Rstudio window
#'
#' @details See \url{https://radiant-rstats.github.io/docs} for documentation and tutorials
#'
#' @importFrom radiant.data launch
#'
#' @examples
#' \dontrun{
#' radiant.multivariate_window()
#' }
#' @export
radiant.multivariate_window <- function() radiant.data::launch(package = "radiant.multivariate", run = "window")


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
