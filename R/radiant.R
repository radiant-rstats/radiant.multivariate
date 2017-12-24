#' Launch radiant.multivariate in default browser or Rstudio Viewer
#'
#' @details See \url{https://radiant-rstats.github.io/docs} for documentation and tutorials
#'
#' @param run Run radiant.multivariate in an external browser ("browser") or in the Rstudio viewer ("viewer")
#'
#' @importFrom rstudioapi viewer
#'
#' @examples
#' \dontrun{
#' radiant.multivariate::radiant.multivariate()
#' radiant.multivariate::radiant.multivariate("viewer")
#' }
#'
#' @export
radiant.multivariate <- function(run = "browser") {
  if (!"package:radiant.multivariate" %in% search()) {
    if (!sshhr(require(radiant.multivariate))) {
      stop("\nCalling radiant.multivariate start function but radiant.multivariate is not installed.")
    }
  }
  run <- if (run == "viewer") {
    message("\nStarting radiant.multivariate in Rstudio Viewer ...")
    rstudioapi::viewer
  } else {
    message("\nStarting radiant.multivariate in default browser ...\n\nUse radiant.multivariate::radiant.multivariate(\"viewer\") to open radiant.multivariate in Rstudio Viewer")
    TRUE
  }
  suppressPackageStartupMessages(
    shiny::runApp(system.file("app", package = "radiant.multivariate"), launch.browser = run)
  )
}
