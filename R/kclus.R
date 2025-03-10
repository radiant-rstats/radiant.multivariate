#' K-clustering
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/kclus.html} for an example in Radiant
#'
#' @param dataset Dataset
#' @param vars Vector of variables to include in the analysis
#' @param fun Use either "kmeans" or "kproto" for clustering
#' @param hc_init Use centers from hclus as the starting point
#' @param distance Distance for hclus
#' @param method Method for hclus
#' @param seed Random see to use for k-clustering if hc_init is FALSE
#' @param nr_clus Number of clusters to extract
#' @param standardize Standardize data (TRUE or FALSE)
#' @param lambda Parameter > 0 to trade off between Euclidean distance of numeric variables and simple matching coefficient between categorical variables. Also a vector of variable specific factors is possible where the order must correspond to the order of the variables in the data. In this case all variables' distances will be multiplied by their corresponding lambda value.
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param envir Environment to extract data from
#'
#' @return A list of all variables used in kclus as an object of class kclus
#'
#' @examples
#' kclus(shopping, c("v1:v6"), nr_clus = 3) %>% str()
#' @seealso \code{\link{summary.kclus}} to summarize results
#' @seealso \code{\link{plot.kclus}} to plot results
#' @seealso \code{\link{store.kclus}} to add cluster membership to the selected dataset
#'
#' @importFrom clustMixType kproto
#' @importFrom dplyr across everything summarise
#'
#' @export
kclus <- function(dataset, vars, fun = "kmeans", hc_init = TRUE, distance = "sq.euclidian",
                  method = "ward.D", seed = 1234, nr_clus = 2, standardize = TRUE, lambda = NULL,
                  data_filter = "", envir = parent.frame()) {
  df_name <- if (is_string(dataset)) dataset else deparse(substitute(dataset))
  dataset <- get_data(dataset, vars, filt = data_filter, envir = envir)
  rm(envir)

  if (is.empty(lambda)) lambda <- NULL

  ## in case : is used
  if (length(vars) < ncol(dataset)) {
    vars <- colnames(dataset)
  }

  if (fun == "median") {
    stop("K-medians is deprecated. Use either 'kmeans' or 'kproto' for the 'fun' argument")
  } else if (fun %in% c("mean", "kmeans")) {
    fun <- "kmeans"
    dataset <- select_if(dataset, function(x) !is.factor(x))
    if (ncol(dataset) < length(vars)) {
      cat("** Categorical variables cannot be used with K-means **.\n** Select the K-proto option instead **\n\n")
    }
    vars <- colnames(dataset)
  } else if (fun == "kproto") {
    if (hc_init) distance <- "gower"
    if (!any(sapply(dataset, function(x) is.numeric(x)) == FALSE)) {
      fun <- "kmeans"
      cat("** K-means used when no categorical variables included **\n\n")
    }
  }

  ## check if there is variation in the data
  not_vary <- vars[summarise(dataset, across(everything(), does_vary)) == FALSE]
  if (length(not_vary) > 0) {
    return(paste0("The following variable(s) show no variation. Please select other variables.\n\n** ", paste0(not_vary, collapse = ", "), " **") %>%
      add_class("kclus"))
  }

  max_freq <- function(x) as.factor(names(which.max(table(x))))
  center_calc <- function(x, prop = FALSE) {
    if (is.numeric(x)) {
      mean(x)
    } else {
      mf <- max_freq(x)
      if (prop) {
        ps <- table(x) / length(x)
        as.factor(paste0(mf, " (", 100 * round(ps[as.character(mf)], 2), "%)"))
      } else {
        mf
      }
    }
  }

  if (hc_init) {
    init <- hclus(
      dataset, vars,
      distance = distance, method = method,
      max_cases = Inf, standardize = standardize
    )
    clus_var <- cutree(init$hc_out, k = nr_clus)
    hc_cent <- c()
    km_out <- dataset %>%
      mutate(clus_var = clus_var) %>%
      (function(x) if (standardize) mutate_if(x, is.numeric, ~ as.vector(scale(.))) else x) %T>%
      (function(x) {
        hc_cent <<-
          group_by(x, clus_var) %>%
          summarise_all(center_calc) %>%
          select(-clus_var)
      }) %>%
      select(-clus_var) %>%
      (function(x) {
        if (fun == "kproto") {
          kp <- clustMixType::kproto(as.data.frame(x), k = hc_cent, iter.max = 500, verbose = FALSE, lambda = lambda)
          ## kproto doesn't provide totss or betweenss by default
          kp$totss <- clustMixType::kproto(as.data.frame(x), k = 1, iter.max = 1, verbose = FALSE, lambda = lambda)$tot.withinss
          kp$betweenss <- kp$totss - kp$tot.withinss
          kp
        } else {
          kmeans(x, centers = as.matrix(hc_cent), iter.max = 500)
        }
      })
    rm(init, hc_cent)
  } else {
    seed %>%
      gsub("[^0-9]", "", .) %>%
      (function(x) if (!is.empty(x)) set.seed(seed))
    km_out <- dataset %>%
      (function(x) if (standardize) mutate_if(x, is.numeric, ~ as.vector(scale(.))) else x) %>%
      (function(x) {
        if (fun == "kproto") {
          kp <- clustMixType::kproto(as.data.frame(x), k = nr_clus, iter.max = 500, verbose = FALSE, lambda = lambda)
          ## kproto doesn't provide totss or betweenss by default
          kp$totss <- clustMixType::kproto(as.data.frame(x), k = 1, iter.max = 1, verbose = FALSE, lambda = lambda)$tot.withinss
          kp$betweenss <- kp$totss - kp$tot.withinss
          kp
        } else {
          kmeans(x, centers = nr_clus, nstart = 10, iter.max = 500)
        }
      })
  }

  clus_names <- paste("Cluster", 1:nr_clus)
  clus_means <- dataset %>%
    mutate(clus_var = km_out$cluster) %>%
    group_by(clus_var) %>%
    summarise_all(function(x) center_calc(x, prop = TRUE)) %>%
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
#' @seealso \code{\link{kclus}} to generate results
#' @seealso \code{\link{plot.kclus}} to plot results
#' @seealso \code{\link{store.kclus}} to add cluster membership to the selected dataset
#'
#' @export
summary.kclus <- function(object, dec = 2, ...) {
  if (is.character(object)) {
    return(object)
  }
  cat(paste0("K-", substring(object$fun, 2), " cluster analysis\n"))
  cat("Data         :", object$df_name, "\n")
  if (!is.empty(object$data_filter)) {
    cat("Filter       :", gsub("\\n", "", object$data_filter), "\n")
  }
  cat("Variables    :", paste0(object$vars, collapse = ", "), "\n")
  cat("Clustering by:", paste0("K-", substring(object$fun, 2), "\n"))
  if (object$fun == "kproto") {
    cat("Lambda       :", round(object$km_out$lambda, dec), "\n")
  }
  if (object$hc_init) {
    cat("HC method    :", object$method, "\n")
    cat("HC distance  :", object$distance, "\n")
  }
  cat("Standardize  :", object$standardize, "\n")
  cat("Observations :", format_nr(object$nr_obs, dec = 0), "\n")
  cat("Generated    :", object$nr_clus, "clusters of sizes", paste0(format_nr(object$km_out$size, dec = 0), collapse = " | "), "\n\n")

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
    paste0("\nBetween cluster heterogeneity accounts for ", ., " of the\ntotal heterogeneity in the data (higher is better)") %>%
    cat()
}

#' Plot method for kclus
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/kclus.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{kclus}}
#' @param plots One of "density", "bar", or "scatter")
#' @param shiny Did the function call originate inside a shiny app
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This option can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{https://ggplot2.tidyverse.org/} for options.
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- kclus(shopping, vars = "v1:v6", nr_clus = 3)
#' plot(result)
#' @seealso \code{\link{kclus}} to generate results
#' @seealso \code{\link{summary.kclus}} to summarize results
#' @seealso \code{\link{store.kclus}} to add cluster membership to the selected dataset
#'
#' @importFrom rlang .data
#'
#' @export
plot.kclus <- function(x, plots = "density", shiny = FALSE, custom = FALSE, ...) {
  if (is.character(x)) {
    return(x)
  }

  x$dataset$Cluster <- as.factor(x$km_out$cluster)
  vars <- colnames(x$dataset) %>% .[-length(.)]

  fct_plot <- function(dataset, var1, var2, color = "black", alpha = 0.5) {
    tab <- as.data.frame(table(dataset[[var1]], dataset[[var2]]))
    ggplot(tab, aes(x = .data$Var2, y = .data$Freq, fill = .data$Var1)) +
      geom_bar(stat = "identity", position = "fill", alpha = alpha, color = color) +
      scale_y_continuous(labels = scales::percent) +
      labs(y = "", x = var2, fill = var1)
  }

  plot_list <- list()
  if ("density" %in% plots) {
    for (var in vars) {
      plot_list[[paste0("dens_", var)]] <- if (is.numeric(x$dataset[[var]])) {
        ggplot(x$dataset, aes(x = .data[[var]], fill = .data$Cluster)) +
          geom_density(adjust = 2.5, alpha = 0.3) +
          labs(y = "") +
          theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
      } else {
        fct_plot(x$dataset, "Cluster", var, color = "black", alpha = 0.3)
      }
    }
  }
  if ("bar" %in% plots) {
    me_calc <- function(se, n, conf.lev = .95) {
      se * qt(conf.lev / 2 + .5, n - 1)
    }

    for (var in vars) {
      plot_list[[paste0("bar_", var)]] <- if (is.numeric(x$dataset[[var]])) {
        dat_summary <-
          select_at(x$dataset, .vars = c(var, "Cluster")) %>%
          group_by_at(.vars = "Cluster") %>%
          summarise_all(
            list(
              cent = mean,
              n = length,
              sd = sd,
              se = se,
              me = ~ me_calc(se, n, .95)
            )
          )

        ggplot(dat_summary, aes(x = .data$Cluster, y = .data$cent, fill = .data$Cluster)) +
          geom_bar(stat = "identity", alpha = 0.5) +
          geom_errorbar(width = .1, aes(ymin = cent - me, ymax = cent + me)) +
          geom_errorbar(width = .05, aes(ymin = cent - se, ymax = cent + se), color = "blue") +
          theme(legend.position = "none") +
          labs(y = paste0(var, " (mean)"))
      } else {
        fct_plot(x$dataset, var, "Cluster")
      }
    }
  }
  if ("scatter" %in% plots) {
    for (var in vars) {
      plot_list[[paste0("scatter_", var)]] <- if (is.numeric(x$dataset[[var]])) {
        visualize(
          x$dataset,
          xvar = "Cluster", yvar = var,
          check = "jitter",
          type = "scatter",
          linecol = "blue",
          pointcol = "black",
          custom = TRUE
        )
      } else {
        fct_plot(x$dataset, var, "Cluster")
      }
    }
  }

  if (length(plot_list) > 0) {
    if (custom) {
      if (length(plot_list) == 1) plot_list[[1]] else plot_list
    } else {
      patchwork::wrap_plots(plot_list, ncol = min(length(plot_list), 2)) %>%
        (function(x) if (isTRUE(shiny)) x else print(x))
    }
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
#' @seealso \code{\link{kclus}} to generate results
#' @seealso \code{\link{summary.kclus}} to summarize results
#' @seealso \code{\link{plot.kclus}} to plot results
#'
#' @export
store.kclus <- function(dataset, object, name = "", ...) {
  if (is.empty(name)) name <- paste0("kclus", object$nr_clus)
  indr <- indexr(dataset, object$vars, object$data_filter)
  km <- rep(NA, indr$nr)
  km[indr$ind] <- object$km_out$cluster
  dataset[[name]] <- as.factor(km)
  dataset
}
