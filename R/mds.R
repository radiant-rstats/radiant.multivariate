#' (Dis)similarity based brand maps (MDS)
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/mds.html} for an example in Radiant
#'
#' @param dataset Dataset
#' @param id1 A character variable or factor with unique entries
#' @param id2 A character variable or factor with unique entries
#' @param dis A numeric measure of brand dissimilarity
#' @param method Apply metric or non-metric MDS
#' @param nr_dim Number of dimensions
#' @param seed Random seed
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#'
#' @return A list of all variables defined in the function as an object of class mds
#'
#' @examples
#' mds(city, "from", "to", "distance") %>% str()
#' mds(diamonds, "clarity", "cut", "price") %>% str()
#'
#' @seealso \code{\link{summary.mds}} to summarize results
#' @seealso \code{\link{plot.mds}} to plot results
#'
#' @importFrom MASS isoMDS
#'
#' @export
mds <- function(
  dataset, id1, id2, dis, method = "metric",
  nr_dim = 2, seed = 1234, data_filter = ""
) {

  nr_dim <- as.numeric(nr_dim)
  df_name <- if (is_string(dataset)) dataset else deparse(substitute(dataset))
  dataset <- get_data(dataset, c(id1, id2, dis), filt = data_filter)

  d <- dataset[[dis]]
  id1_dat <- as.character(dataset[[id1]])
  id2_dat <- as.character(dataset[[id2]])
  rm(dataset)

  ## ids
  lab <- unique(c(id1_dat, id2_dat))
  nrLev <- length(lab)

  lower <- (nrLev * (nrLev - 1)) / 2
  nrObs <- length(d)

  ## setup the distance matrix
  mds_dis_mat <- diag(nrLev)
  if (lower == nrObs) {
    mds_dis_mat[lower.tri(mds_dis_mat, diag = FALSE)] <- d
  } else if ((lower + nrLev) == nrObs) {
    mds_dis_mat[lower.tri(mds_dis_mat, diag = TRUE)] <- d
  } else {
    return("Number of observations and unique IDs for the brand variable do not match.\nPlease choose another brand variable or another dataset.\n\nFor an example dataset go to Data > Manage, select 'examples' from the\n'Load data of type' dropdown, and press the 'Load examples' button. Then\nselect the \'city' dataset." %>%
      add_class("mds"))
  }

  mds_dis_mat %<>% set_rownames(lab) %>%
    set_colnames(lab) %>%
    as.dist()

  ## Alternative method, metaMDS - requires vegan
  # res <- suppressWarnings(metaMDS(mds_dis_mat, k = nr_dim, trymax = 500))
  # if (res$converged == FALSE) return("The MDS algorithm did not converge. Please try again.")

  seed %>% gsub("[^0-9]", "", .) %>% {if (!is_empty(.)) set.seed(seed)}
  res <- MASS::isoMDS(mds_dis_mat, k = nr_dim, trace = FALSE)
  res$stress <- res$stress / 100

  if (method == "metric") {
    res$points <- cmdscale(mds_dis_mat, k = nr_dim)
    ## Using R^2
    # res$stress <- sqrt(1 - cor(dist(res$points),mds_dis_mat)^2) * 100
    # Using standard Kruskal formula for metric MDS
    res$stress <- {sum((dist(res$points) - mds_dis_mat) ^ 2) / sum(mds_dis_mat ^ 2)} %>%
      sqrt(.)
  }

  as.list(environment()) %>% add_class("mds")
}

#' Summary method for the mds function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/mds.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{mds}}
#' @param dec Rounding to use for output (default = 2). +1 used for stress measure
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- mds(city, "from", "to", "distance")
#' summary(result, dec = 1)
#'
#' @seealso \code{\link{mds}} to calculate results
#' @seealso \code{\link{plot.mds}} to plot results
#'
#' @export
summary.mds <- function(object, dec = 2, ...) {

  if (is.character(object)) return(cat(object))

  cat("(Dis)similarity based brand map (MDS)\n")
  cat("Data        :", object$df_name, "\n")
  if (!is_empty(object$data_filter)) {
    cat("Filter      :", gsub("\\n", "", object$data_filter), "\n")
  }
  cat("Variables   :", paste0(c(object$id1, object$id2, object$dis), collapse = ", "), "\n")
  cat("Dimensions:", object$nr_dim, "\n")
  meth <- if (object$method == "non-metric") "Non-metric" else "Metric"
  cat("Method      :", meth, "\n")
  cat("Observations:", object$nrObs, "\n")

  cat("\nOriginal distance data:\n")
  object$mds_dis_mat %>% round(dec) %>% print()

  cat("\nRecovered distance data:\n")
  object$res$points %>% dist() %>% round(dec) %>% print()

  cat("\nCoordinates:\n")
  object$res$points %>%
    round(dec) %>%
    set_colnames({
      paste("Dimension ", 1:ncol(.))
    }) %>%
    print()

  cat("\nStress:", round(object$res$stress, dec + 1))
}

#' Plot method for the mds function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/mds.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{mds}}
#' @param rev_dim Flip the axes in plots
#' @param fontsz Font size to use in plots
#' @param shiny Did the function call originate inside a shiny app
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This option can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{http://docs.ggplot2.org} for options.
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- mds(city, "from", "to", "distance")
#' plot(result, fontsz = 7)
#' plot(result, rev_dim = 1:2)
#'
#' @seealso \code{\link{mds}} to calculate results
#' @seealso \code{\link{summary.mds}} to plot results
#'
#' @importFrom ggrepel geom_text_repel
#'
#' @export
plot.mds <- function(x, rev_dim = NULL, fontsz = 5, shiny = FALSE, custom = FALSE, ...) {
  if (is.character(x)) return(cat(x))

  ## set extremes for plot
  lim <- max(abs(x$res$points))

  ## set seed for ggrepel label positioning
  set.seed(x$seed)

  tbl <- as.data.frame(x$res$points) %>%
    set_colnames(paste0("dim", seq_len(ncol(.))))
  tbl$rnames <- rownames(tbl)
  ## reverse selected dimensions
  if (!is_empty(rev_dim)) {
    rev_dim <- as.integer(rev_dim)
    tbl[, rev_dim] <- -1 * tbl[, rev_dim]
  }

  plot_list <- list()
  for (i in 1:(x$nr_dim - 1)) {
    for (j in (i + 1):x$nr_dim) {
      i_name <- paste0("dim", i)
      j_name <- paste0("dim", j)
      plot_list[[paste0("dim", i, "_dim", j)]] <- ggplot(tbl, aes_string(x = i_name, y = j_name, color = "rnames", label = "rnames")) +
        geom_point() +
        ggrepel::geom_text_repel(size = fontsz) +
        theme(legend.position = "none") +
        coord_cartesian(xlim = c(-lim, lim), ylim = c(-lim, lim)) +
        geom_vline(xintercept = 0, size = 0.3) +
        geom_hline(yintercept = 0, size = 0.3) +
        labs(
          x = paste("Dimension", i),
          y = paste("Dimension", j)
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

  sshhr(gridExtra::grid.arrange(grobs = plot_list, ncol = 1)) %>%
  {if (shiny) . else print(.)}
}
