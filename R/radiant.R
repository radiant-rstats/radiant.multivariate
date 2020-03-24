#' Launch radiant.multivariate in the default browser
#'
#' @description Launch radiant.multivariate in the default web browser
#' @details See \url{https://radiant-rstats.github.io/docs} for documentation and tutorials
#'
#' @param state Path to state file to load
#' @param ... additional arguments to pass to shiny::runApp (e.g, port = 8080)
#'
#' @importFrom radiant.data launch
#'
#' @examples
#' \dontrun{
#' radiant.multivariate()
#' }
#' @export
radiant.multivariate <- function(state, ...) radiant.data::launch(package = "radiant.multivariate", run = "browser", state, ...)

#' Launch radiant.multivariate in an Rstudio window
#'
#' @details See \url{https://radiant-rstats.github.io/docs} for documentation and tutorials
#'
#' @param state Path to state file to load
#' @param ... additional arguments to pass to shiny::runApp (e.g, port = 8080)
#'
#' @importFrom radiant.data launch
#'
#' @examples
#' \dontrun{
#' radiant.multivariate_window()
#' }
#' @export
radiant.multivariate_window <- function(state, ...) radiant.data::launch(package = "radiant.multivariate", run = "window", state, ...)

#' Launch radiant.multivariate in the Rstudio viewer
#'
#' @details See \url{https://radiant-rstats.github.io/docs} for documentation and tutorials
#'
#' @param state Path to state file to load
#' @param ... additional arguments to pass to shiny::runApp (e.g, port = 8080)
#'
#' @importFrom radiant.data launch
#'
#' @examples
#' \dontrun{
#' radiant.multivariate_viewer()
#' }
#' @export
radiant.multivariate_viewer <- function(state, ...) radiant.data::launch(package = "radiant.multivariate", run = "viewer", state, ...)
