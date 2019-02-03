#' K-clustering
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/kclus.html} for an example in Radiant
#'
#' @param dataset Dataset
#' @param vars Vector of variables to include in the analysis
#' @param fun Function to use: "mean" or "median"
#' @param hc_init Use centers from hclus as the starting point
#' @param distance Distance for hclus
#' @param method Method for hclus
#' @param seed Random see to use for k-clustering if hc_init is FALSE
#' @param nr_clus Number of clusters to extract
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#'
#' @return A list of all variables used in kclus as an object of class kclus
#'
#' @examples
#' kclus(shopping, c("v1:v6"), nr_clus = 3) %>% str()
#'
#' @seealso \code{\link{summary.kclus}} to summarize results
#' @seealso \code{\link{plot.kclus}} to plot results
#' @seealso \code{\link{store.kclus}} to add cluster membership to the selected dataset
#'
#' @importFrom Gmedian Gmedian kGmedian
#'
#' @export
kclus <- function(
  dataset, vars, fun = "mean", hc_init = TRUE,
  distance = "sq.euclidian", method = "ward.D",
  seed = 1234, nr_clus = 2, data_filter = ""
) {

  df_name <- if (is_string(dataset)) dataset else deparse(substitute(dataset))
  dataset <- get_data(dataset, vars, filt = data_filter)

  ## in case : is used
  if (length(vars) < ncol(dataset))
    vars <- colnames(dataset)

  if (fun == "median" && length(vars) < 2) {
    stop("K-medians requires at least two variables as input")
  }

  ## converting factors to integer with first level = 1
  convert <- function(x) as.integer(x == levels(x)[1])
  dataset <- mutate_if(dataset, is.factor, convert)

  if (hc_init) {
    init <- hclus(dataset, vars, distance = distance, method = method, max_cases = Inf)

    clus_var <- cutree(init$hc_out, k = nr_clus)
    hc_cent <- c()
    km_out <- dataset %>%
      mutate(clus_var = clus_var) %>%
      mutate_all(~ as.vector(scale(.))) %T>% {
        hc_cent <<-
          group_by(., clus_var) %>%
          summarise_all(mean) %>%
          select(-clus_var) %>%
          as.matrix()
      } %>%
      select(-clus_var) %>%
      {
        if (fun == "median") {
          km_cent <- kmeans(., centers = hc_cent, algorithm = "MacQueen", iter.max = 500)$centers
          Gmedian::kGmedian(., ncenters = km_cent)
        } else {
          kmeans(., centers = hc_cent, iter.max = 500)
        }
      }
    rm(init, hc_cent)
  } else {
    seed %>% gsub("[^0-9]", "", .) %>% {
      if (!is_empty(.)) set.seed(seed)
    }
    km_out <- dataset %>%
      mutate_all(~ as.vector(scale(.))) %>%
      {
        if (fun == "median") {
          km_cent <- kmeans(., centers = nr_clus, algorithm = "MacQueen", iter.max = 500)$centers
          Gmedian::kGmedian(., ncenters = km_cent)
        } else {
          kmeans(., centers = nr_clus, nstart = 10, iter.max = 500)
        }
      }
  }

  ## same calculations of SST etc. as for kmeans (verified)
  if (fun == "median") {
    sdat <- mutate_all(dataset, ~ as.vector(scale(.)))
    km_out$withinss <-
      mutate(sdat, clus_var = km_out$cluster) %>%
      group_by(clus_var) %>%
      summarise_all(~ sum((. - mean(.)) ^ 2)) %>%
      select(-clus_var) %>%
      rowSums()
    km_out$tot.withinss <- sum(km_out$withinss)
    km_out$totss <-
      summarise_all(sdat, ~ sum((. - mean(.)) ^ 2)) %>%
      sum(.)
    km_out$betweenss <- km_out$totss - km_out$tot.withinss
    rm(sdat)
  }

  ## Gmedian / tibble / dplyr issue
  clus_names <- paste("Cluster", 1:nr_clus)
  clus_means <- dataset %>%
    mutate(clus_var = km_out$cluster) %>%
    group_by(clus_var) %>%
    summarise_all(mean) %>%
    select(-clus_var) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    set_rownames(clus_names)

  nr_obs <- length(km_out$cluster)

  as.list(environment()) %>% add_class("kclus")
}

#' Summary method for kclus
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/kclus.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{kclus}}
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- kclus(shopping, vars = "v1:v6", nr_clus = 3)
#' summary(result)
#'
#' @seealso \code{\link{kclus}} to generate results
#' @seealso \code{\link{plot.kclus}} to plot results
#' @seealso \code{\link{store.kclus}} to add cluster membership to the selected dataset
#'
#' @export
summary.kclus <- function(object, dec = 2, ...) {
  cat(paste0("K-", object$fun, "s cluster analysis\n"))
  cat("Data         :", object$df_name, "\n")
  if (!is_empty(object$data_filter)) {
    cat("Filter       :", gsub("\\n", "", object$data_filter), "\n")
  }
  cat("Variables    :", paste0(object$vars, collapse = ", "), "\n")
  cat("Clustering by:", paste0("K-", object$fun, "s\n"))
  if (object$hc_init) {
    cat("HC method    :", object$method, "\n")
    cat("HC distance  :", object$distance, "\n")
  }
  cat("Observations :", format_nr(object$nr_obs, dec = 0), "\n")
  cat("Generated    :", object$nr_clus, "clusters of sizes", paste0(format_nr(object$km_out$size, dec = 0), collapse = " | "), "\n\n")

  # cat(paste0("Cluster ", object$fun,"s:\n"))
  cat(paste0("Cluster means:\n"))
  cm <- object$clus_means
  cm <- cbind(data.frame(" " = paste0("Cluster ", 1:nrow(cm)), check.names = FALSE), cm)
  print(format_df(cm, mark = ",", dec = dec), row.names = FALSE)

  ## percentage of within cluster heterogeneity accounted for by each cluster
  cat("\nPercentage of within cluster heterogeneity accounted for by each cluster:\n")
  data.frame(wcv = object$km_out$withinss / object$km_out$tot.withinss, stringsAsFactors = FALSE) %>%
    format_df(perc = TRUE, dec = dec) %>%
    set_rownames(object$clus_names) %>%
    set_colnames("") %>%
    print()

  ## percentage of between cluster heterogeneity versus the total, higher is better
  format_nr(object$km_out$betweenss / object$km_out$totss, perc = TRUE, dec = dec) %>%
    paste0("\nBetween cluster heterogeneity accounts for ", ., " of the\ntotal heterogeneity in the data (higher is better).") %>%
    cat()
}

#' Plot method for kclus
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/kclus.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{kclus}}
#' @param plots One of "density", "bar", or "scatter")
#' @param shiny Did the function call originate inside a shiny app
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This option can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{http://docs.ggplot2.org} for options.
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- kclus(shopping, vars = "v1:v6", nr_clus = 3)
#' plot(result)
#'
#' @seealso \code{\link{kclus}} to generate results
#' @seealso \code{\link{summary.kclus}} to summarize results
#' @seealso \code{\link{store.kclus}} to add cluster membership to the selected dataset
#'
#' @export
plot.kclus <- function(
  x, plots = "density", shiny = FALSE,
  custom = FALSE, ...
) {

  x$dataset$Cluster <- as.factor(x$km_out$cluster)
  vars <- colnames(x$dataset) %>% .[-length(.)]

  ## what to report?
  # fun <- if (x$fun == "mean") mean else median
  fun <- mean
  x$fun <- "mean"

  plot_list <- list()

  if ("density" %in% plots) {
    for (var in vars) {
      plot_list[[paste0("dens_", var)]] <-
        ggplot(x$dataset, aes_string(x = var, fill = "Cluster")) +
        geom_density(adjust = 2.5, alpha = 0.3) +
        labs(y = "") + theme(axis.text.y = element_blank())
    }
  }
  if ("bar" %in% plots) {
    me_calc <- function(se, n, conf.lev = .95)
      se * qt(conf.lev / 2 + .5, n - 1)

    for (var in vars) {
      dat_summary <-
        select_at(x$dataset, .vars = c(var, "Cluster")) %>%
        group_by_at(.vars = "Cluster") %>%
        summarise_all(
          list(
            cent = ~ fun,
            n = length,
            sd = sd,
            se = se,
            me = ~ me_calc(se, n, .95)
          )
        )

      plot_list[[paste0("bar_", var)]] <-
        ggplot(dat_summary, aes_string(x = "Cluster", y = "cent", fill = "Cluster")) +
        geom_bar(stat = "identity") +
        geom_errorbar(width = .1, aes(ymin = cent - me, ymax = cent + me)) +
        geom_errorbar(width = .05, aes(ymin = cent - se, ymax = cent + se), color = "blue") +
        theme(legend.position = "none") +
        labs(y = paste0(var, " (", x$fun, ")"))
    }
  }
  if ("scatter" %in% plots) {
    for (var in vars) {
      plot_list[[paste0("scatter_", var)]] <-
        visualize(
          x$dataset, xvar = "Cluster", yvar = var,
          check = "jitter",
          type = "scatter",
          linecol = "blue",
          pointcol = "black",
          custom = TRUE
        )
    }
  }

  if (custom) {
    if (length(plot_list) == 1) {
      return(plot_list[[1]])
    } else {
      return(plot_list)
    }
  }

  sshhr(gridExtra::grid.arrange(grobs = plot_list, ncol = min(length(plot_list), 2))) %>% {
    if (shiny) . else print(.)
  }
}

#' Add a cluster membership variable to the active dataset
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/kclus.html} for an example in Radiant
#'
#' @param dataset Dataset to append to cluster membership variable to
#' @param object Return value from \code{\link{kclus}}
#' @param name Name of cluster membership variable
#' @param ... Additional arguments
#'
#' @examples
#' kclus(shopping, vars = "v1:v6", nr_clus = 3) %>%
#'   store(shopping, .) %>%
#'   head()
#'
#' @seealso \code{\link{kclus}} to generate results
#' @seealso \code{\link{summary.kclus}} to summarize results
#' @seealso \code{\link{plot.kclus}} to plot results
#'
#' @export
store.kclus <- function(dataset, object, name = "", ...) {
  if (is_empty(name)) name <- paste0("kclus", object$nr_clus)
  indr <- indexr(dataset, object$vars, object$data_filter)
  km <- rep(NA, indr$nr)
  km[indr$ind] <- object$km_out$cluster
  dataset[[name]] <- as.factor(km)
  dataset
}
