#' Hierarchical cluster analysis
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/hclus.html} for an example in Radiant
#'
#' @param dataset Dataset
#' @param vars Vector of variables to include in the analysis
#' @param distance Distance
#' @param method Method
#' @param max_cases Maximum number of cases allowed (default is 1000). Set to avoid long-running analysis in the radiant web-interface
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#'
#' @return A list of all variables used in hclus as an object of class hclus
#'
#' @examples
#' hclus(shopping, vars = "v1:v6") %>% str()
#'
#' @seealso \code{\link{summary.hclus}} to summarize results
#' @seealso \code{\link{plot.hclus}} to plot results
#'
#' @export
hclus <- function(
  dataset, vars, distance = "sq.euclidian",
  method = "ward.D", max_cases = 5000,
  data_filter = ""
) {

  df_name <- if (is_string(dataset)) dataset else deparse(substitute(dataset))
  dataset <- get_data(dataset, vars, filt = data_filter)
  if (nrow(dataset) > max_cases) {
    return("The number of cases to cluster exceed the maximum set. Change\nthe number of cases allowed using the 'Max cases' input box." %>%
      add_class("hclus"))
  }

  ## in case : is used
  if (length(vars) < ncol(dataset))
    vars <- colnames(dataset)

  dataset %>%
    scale() %>%
    {
      if (distance == "sq.euclidian") {
        dist(., method = "euclidean") ^ 2
      } else {
        dist(., method = distance)
      }
    } %>%
    hclust(d = ., method = method) -> hc_out

  as.list(environment()) %>% add_class("hclus")
}

#' Summary method for the hclus function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/hclus.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{hclus}}
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- hclus(shopping, vars = c("v1:v6"))
#' summary(result)
#'
#' @seealso \code{\link{hclus}} to generate results
#' @seealso \code{\link{plot.hclus}} to plot results
#'
#' @export
summary.hclus <- function(object, ...) {
  if (is.character(object)) return(object)

  cat("Hierarchical cluster analysis\n")
  cat("Data        :", object$df_name, "\n")
  if (object$data_filter %>% gsub("\\s", "", .) != "") {
    cat("Filter      :", gsub("\\n", "", object$data_filter), "\n")
  }
  cat("Variables   :", paste0(object$vars, collapse = ", "), "\n")
  cat("Method      :", object$method, "\n")
  cat("Distance    :", object$distance, "\n")
  cat("Observations:", format_nr(length(object$hc_out$order), dec = 0), "\n")
}

#' Plot method for the hclus function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/hclus.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{hclus}}
#' @param plots Plots to return. "change" shows the percentage change in within-cluster heterogeneity as respondents are grouped into different number of clusters, "dendro" shows the dendrogram, "scree" shows a scree plot of within-cluster heterogeneity
#' @param cutoff For large datasets plots can take time to render and become hard to interpret. By selection a cutoff point (e.g., 0.05 percent) the initial steps in hierarchical cluster analysis are removed from the plot
#' @param shiny Did the function call originate inside a shiny app
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This option can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{http://docs.ggplot2.org} for options.
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- hclus(shopping, vars = c("v1:v6"))
#' plot(result, plots = c("change", "scree"), cutoff = .05)
#' plot(result, plots = "dendro", cutoff = 0)
#'
#' @seealso \code{\link{hclus}} to generate results
#' @seealso \code{\link{summary.hclus}} to summarize results
#'
#' @export
plot.hclus <- function(
  x, plots = c("scree", "change"),
  cutoff = 0.05, shiny = FALSE,
  custom = FALSE, ...
) {

  if (is_empty(plots)) return(invisible())
  if (is.character(x)) return(invisible())
  if (is_not(cutoff)) cutoff <- 0
  x$hc_out$height %<>% {. / max(.)}

  plot_list <- list()
  if ("scree" %in% plots) {
    plot_list[["scree"]] <-
      x$hc_out$height[x$hc_out$height > cutoff] %>%
      data.frame(
        height = .,
        nr_clus = as.integer(length(.):1),
        stringsAsFactors = FALSE
      ) %>%
      ggplot(aes(x = factor(nr_clus, levels = nr_clus), y = height, group = 1)) +
      geom_line(color = "blue", linetype = "dotdash", size = .7) +
      geom_point(color = "blue", size = 4, shape = 21, fill = "white") +
      scale_y_continuous(labels = scales::percent) +
      labs(
        title = "Scree plot",
        x = "# clusters",
        y = "Within-cluster heterogeneity"
      )
  }

  if ("change" %in% plots) {
    plot_list[["change"]] <-
      x$hc_out$height[x$hc_out$height > cutoff] %>%
      {(. - lag(.)) / lag(.)} %>%
      data.frame(
        bump = .,
        nr_clus = paste0((length(.) + 1):2, "-", length(.):1),
        stringsAsFactors = FALSE
      ) %>%
      na.omit() %>%
      ggplot(aes(x = factor(nr_clus, levels = nr_clus), y = bump)) +
      geom_bar(stat = "identity", alpha = 0.5, fill = "blue") +
      scale_y_continuous(labels = scales::percent) +
      labs(
        title = "Change in within-cluster heterogeneity",
        x = "# clusters",
        y = "Change in within-cluster heterogeneity"
      )
  }

  if ("dendro" %in% plots) {
    hc <- as.dendrogram(x$hc_out)
    xlab <- ""
    if (length(plots) > 1) {
      xlab <- "When dendrogram is selected no other plots can be shown.\nCall the plot function separately in Report > Rmd to view different plot types."
    }

    ## can't combine base graphics with grid graphics
    ## https://cran.r-project.org/web/packages/gridExtra/vignettes/grid.arrange.html
    ## ... unless you want to try gridBase https://cran.r-project.org/web/packages/gridBase/index.html

    ## trying out ggraph - looks great but dendrogram very slow for larger datasets
    # install.packages("ggraph")
    # library(ggraph)
    # https://www.r-graph-gallery.com/335-custom-ggraph-dendrogram/
    # plot_list[["dendro"]] <- ggraph(hc, 'dendrogram', circular = FALSE) +
      # geom_edge_elbow()

    if (cutoff == 0) {
      plot(hc, main = "Dendrogram", xlab = xlab, ylab = "Within-cluster heterogeneity")
    } else {
      plot(
        hc, ylim = c(cutoff, 1), leaflab = "none",
        main = "Cutoff dendrogram", xlab = xlab, ylab = "Within-cluster heterogeneity"
      )
    }
    return(invisible())
  }

  if (custom) {
    if (length(plot_list) == 1) {
      return(plot_list[[1]])
    } else {
      return(plot_list)
    }
  }

  sshhr(gridExtra::grid.arrange(grobs = plot_list, ncol = 1)) %>% {
    if (shiny) . else print(.)
  }
}
