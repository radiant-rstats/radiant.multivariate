# radiant.multivariate 0.9.7.0

## Major changes

* Using [`shinyFiles`](https://github.com/thomasp85/shinyFiles) to provide convenient access to data located on a server

## Minor changes

* Option to add selected labels to the dendogram in hierarchical clustering
* Replace non-ASCII characters in example datasets
* Remove `rstudioapi` as a direct import
* Revert from `svg` to `png` for plots in `_Report > Rmd_ and _Report > R_. `svg` scatter plots with many point get to big for practical use on servers that have to transfer images to a local browser
* Removed dependency on `methods` package

# radiant.multivariate 0.9.5.0

* Fix to accomodate changes in `deparse` in R 3.5

# radiant.multivariate 0.9.3.1

## Major changes

* Various changes to the code to accomodate the use of `shiny::makeReactiveBinding`. The advantage is that the code generated for _Report > Rmd_ and _Report > R_ will no longer have to use `r_data` to store and access data. This means that code generated and used in the Radiant browser interface will be directly usable without the browser interface as well.
* Used `ggplot2` and `ggrepel` for `mds` brand maps
* Used `ggplot2` and `ggrepel` for `prmap` brand maps

## Deprecated

* `pmap` was renamed to `prmap` to avoid conflict with `purrr::pmap`

# radiant.multivariate 0.9.2.0

## Major changes

* Renamed `pmap` function for perceptual maps to `prmap` to avoid conflict with `purrr::pmap`
* `Estimate` buttons indicate when models should be re-estimated based on changes in user input
* Upload and download data using the Rstudio file browser. Allows using relative paths to files (e.g., data or images inside an Rstudio project)
* Long lines of code generated for _Report > Rmd_ or _Report > R_ will be wrapped to enhance readability 

## Minor changes

* Enhanced keyboard shortcuts
* Upgraded tidyr dependency to 0.7

## Bug fixes

* Fix for `pmap` and `mds` when a tibble is passed  

# radiant.multivariate 0.8.7.1

* Upgraded dplyr dependency to 0.7.1

# radiant.multivariate 0.8.2.0

## Minor changes

* Updated output formatting code to make coefficient information more easily accessible
* Add KMO measures for individual variables to _Factor > Pre-factor_
* Code cleanup

## Bug fixes

* Pass plot sizing function to plot_downloader to ensure proper display

# radiant.multivariate VERSION 0.8.0

## Major changes

* Added k-medians, from the Gmedians package, as an option in _Multivariate > K-clustering_
* Added additional rotation options in _Multivariate > Factor_
* Added predict tab for conjoint
* Added option to analyse conjoint data for multiple respondents (`By`) or for a specific respondent (`Show`)
* Store PWs or IWs from conjoint analysis for multiple respondents
* Derive and store predictions based on conjoint analysis for multiple respondents

## Minor changes

* Show dataset name in output if dataframe passed directly to analysis function
* As an alternative to using the Estimate button to run a model you can now also use CTRL-enter or CMD-enter
* Use ALT-enter to put code into _Report > Rmd_ or _Report > R_

## Bug fixes

- Import from `GPArotation`

## Deprecated

* kmeans_clus was replaced by kclus
* hier_clus was replaced by hclus
* Use of *_each is deprecated
