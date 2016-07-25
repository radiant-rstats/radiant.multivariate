#' Launch Radiant in the default browser
#'
#' @details See \url{https://radiant-rstats.github.io/docs} for documentation and tutorials
#'
#' @export
radiant.multivariate <- function() {
  if (!"package:radiant.multivariate" %in% search())
    if (!require(radiant.multivariate)) stop("Calling radiant.multivariate start function but radiant.multivariate is not installed.")
  runApp(system.file("app", package = "radiant.multivariate"), launch.browser = TRUE)
}
