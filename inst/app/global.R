## sourcing from radiant.data
options(radiant.path.data = system.file(package = "radiant.data"))
source(file.path(getOption("radiant.path.data"), "app/global.R"), encoding = getOption("radiant.encoding", default = "UTF-8"), local = TRUE)

ifelse(grepl("radiant.multivariate", getwd()) && file.exists("../../inst"), "..", system.file(package = "radiant.multivariate")) %>%
  options(radiant.path.multivariate = .)

## setting path for figures in help files
addResourcePath("figures_multivariate", "tools/help/figures/")

## setting path for www resources
addResourcePath("www_multivariate", file.path(getOption("radiant.path.multivariate"), "app/www/"))

if (is.null(getOption("radiant.path.model"))) options(radiant.path.model = system.file(package = "radiant.model"))

## loading urls and ui
source("init.R", encoding = getOption("radiant.encoding", "UTF-8"), local = TRUE)
options(radiant.url.patterns = make_url_patterns())

if (!"package:radiant.multivariate" %in% search() &&
  isTRUE(getOption("radiant.development")) &&
  getOption("radiant.path.multivariate") == "..") {
  options(radiant.from.package = FALSE)
} else {
  options(radiant.from.package = TRUE)
}
