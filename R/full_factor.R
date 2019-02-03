#' Factor analysis (PCA)
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/full_factor.html} for an example in Radiant
#'
#' @param dataset Dataset
#' @param vars Variables to include in the analysis
#' @param method Factor extraction method to use
#' @param nr_fact Number of factors to extract
#' @param rotation Apply varimax rotation or no rotation ("varimax" or "none")
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#'
#' @return A list with all variables defined in the function as an object of class full_factor
#'
#' @examples
#' full_factor(shopping, "v1:v6") %>% str()
#'
#' @seealso \code{\link{summary.full_factor}} to summarize results
#' @seealso \code{\link{plot.full_factor}} to plot results
#'
#' @importFrom psych principal fa
#' @importFrom GPArotation quartimax oblimin simplimax
#'
#' @export
full_factor <- function(
  dataset, vars, method = "PCA", nr_fact = 1,
  rotation = "varimax", data_filter = ""
) {

  df_name <- if (is_string(dataset)) dataset else deparse(substitute(dataset))
  dataset <- get_data(dataset, vars, filt = data_filter)

  ## in case : is used
  if (length(vars) < ncol(dataset))
    vars <- colnames(dataset)

  nrObs <- nrow(dataset)
  if (nrObs <= ncol(dataset)) {
    return("Data should have more observations than variables.\nPlease reduce the number of variables." %>%
      add_class("full_factor"))
  }

  nrFac <- max(1, as.numeric(nr_fact))
  if (nrFac > ncol(dataset)) {
    return("The number of factors cannot exceed the number of variables" %>%
        add_class("full_factor"))
    nrFac <- ncol(dataset)
  }

  if (method == "PCA") {
    fres <- psych::principal(
      dataset, nfactors = nrFac, rotate = rotation, scores = TRUE,
      oblique.scores = FALSE
    )
  } else {
    fres <- try(psych::fa(
      dataset, nfactors = nrFac, rotate = rotation, scores = TRUE,
      oblique.scores = FALSE, fm = "ml"
    ), silent = TRUE)
    if (inherits(fres, "try-error")) {
      return(
        "An error occured. Increase the number of variables or reduce the number of factors" %>%
          add_class("full_factor")
      )
    }
  }

  ## convert loadings object to data.frame
  floadings <-
    fres$loadings %>%
    {
      dn <- dimnames(.)
      matrix(., nrow = length(dn[[1]])) %>%
        set_colnames(., dn[[2]]) %>%
        set_rownames(., dn[[1]]) %>%
        data.frame(stringsAsFactors = FALSE)
    }

  as.list(environment()) %>% add_class("full_factor")
}

#' Summary method for the full_factor function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/full_factor.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{full_factor}}
#' @param cutoff Show only loadings with (absolute) values above cutoff (default = 0)
#' @param fsort Sort factor loadings
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- full_factor(shopping , "v1:v6", nr_fact = 2)
#' summary(result)
#' summary(result, cutoff = .5, fsort = TRUE)
#'
#' @seealso \code{\link{full_factor}} to calculate results
#' @seealso \code{\link{plot.full_factor}} to plot results
#'
#' @importFrom psych fa.sort
#'
#' @export
summary.full_factor <- function(
  object, cutoff = 0, fsort = FALSE,
  dec = 2, ...
) {

  if (is.character(object)) return(cat(object))

  cat("Factor analysis\n")
  cat("Data        :", object$df_name, "\n")
  if (!is_empty(object$data_filter)) {
    cat("Filter      :", gsub("\\n", "", object$data_filter), "\n")
  }
  cat("Variables   :", paste0(object$vars, collapse = ", "), "\n")
  cat("Factors     :", object$nr_fact, "\n")
  cat("Method      :", object$method, "\n")
  cat("Rotation    :", object$rotation, "\n")
  cat("Observations:", format_nr(object$nrObs, dec = 0), "\n")

  cat("\nFactor loadings:\n")

  ## show only the loadings > cutoff
  clean_loadings(object$floadings, cutoff = cutoff, fsort = fsort, dec = dec, repl = "") %>%
    print()

  ## fit measures
  cat("\nFit measures:\n")
  colSums(object$floadings ^ 2) %>%
    rbind(., . / nrow(object$floadings)) %>%
    rbind(., cumsum(.[2, ])) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    format_df(dec = dec) %>%
    set_rownames(c("Eigenvalues", "Variance %", "Cumulative %")) %>%
    print()

  # results from psych - uncomment to validate results
  # object$fres$loadings %>%
  # { if (fsort) psych::fa.sort(.) else . } %>%
  # print(cutoff = cutoff, digits = 2)

  cat("\nAttribute communalities:")
  data.frame(1 - object$fres$uniqueness, stringsAsFactors = FALSE) %>%
    format_df(dec = dec, perc = TRUE) %>%
    set_rownames(object$vars) %>%
    set_colnames("") %>%
    print()

  cat("\nFactor scores (max 10 shown):\n")
  as.data.frame(object$fres$scores, stringsAsFactors = FALSE) %>%
    .[1:min(nrow(.), 10), , drop = FALSE] %>%
    format_df(dec = dec) %>%
    print(row.names = FALSE)
}

#' Plot method for the full_factor function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/full_factor.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{full_factor}}
#' @param plots Include attribute ("attr"), respondents ("resp") or both in the plot
#' @param shiny Did the function call originate inside a shiny app
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This option can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{http://docs.ggplot2.org} for options.
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- full_factor(shopping , "v1:v6", nr_fact = 2)
#' plot(result)
#'
#' @seealso \code{\link{full_factor}} to calculate results
#' @seealso \code{\link{plot.full_factor}} to plot results
#'
#' @importFrom ggrepel geom_text_repel
#'
#' @export
plot.full_factor <- function(x, plots = "attr", shiny = FALSE, custom = FALSE, ...) {

  ## when no analysis was conducted (e.g., no variables selected)
  if (is.character(x)) {
    return(plot(x = 1, type = "n", main = x, axes = FALSE, xlab = "", ylab = ""))
  } else if (x$fres$factors < 2) {
    x <- "Plots require two or more factors"
    return(plot(x = 1, type = "n", main = x, axes = FALSE, xlab = "", ylab = ""))
  }

  df <- x$floadings
  scores <- as.data.frame(x$fres$scores)
  plot_scale <- if ("resp" %in% plots) max(scores) else 1
  rnames <- rownames(df)
  cnames <- colnames(df)
  plot_list <- list()
  for (i in 1:(length(cnames) - 1)) {
    for (j in (i + 1):length(cnames)) {
      i_name <- cnames[i]
      j_name <- cnames[j]
      df2 <- cbind(df[, c(i_name, j_name)], rnames)

      p <- ggplot(df2, aes_string(x = i_name, y = j_name)) +
        theme(legend.position = "none") +
        coord_cartesian(xlim = c(-plot_scale, plot_scale), ylim = c(-plot_scale, plot_scale)) +
        geom_vline(xintercept = 0) +
        geom_hline(yintercept = 0)

      if ("resp" %in% plots) {
        p <- p + geom_point(data = scores, aes_string(x = i_name, y = j_name), alpha = 0.5)
      }

      if ("attr" %in% plots) {
        p <- p + geom_point(aes_string(color = "rnames")) +
          ggrepel::geom_text_repel(aes_string(color = "rnames", label = "rnames")) +
          geom_segment(
            aes_string(x = 0, y = 0, xend = i_name, yend = j_name, color = "rnames"),
            size = 0.5, linetype = "dashed", alpha = 0.5
          )
      }

      plot_list[[paste0(i_name, "_", j_name)]] <- p
    }
  }

  if (custom) {
    if (length(plot_list) == 1) {
      return(plot_list[[1]])
    } else {
      return(plot_list)
    }
  }

  sshhr(gridExtra::grid.arrange(grobs = plot_list, ncol = min(length(plot_list), 2))) %>%
    {if (shiny) . else print(.)}
}

#' Store factor scores to active dataset
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/full_factor.html} for an example in Radiant
#'
#' @param dataset Dataset to append to factor scores to
#' @param object Return value from \code{\link{full_factor}}
#' @param name Name of factor score variables
#' @param ... Additional arguments
#'
#' @examples
#' full_factor(shopping, "v1:v6", nr_fact = 3) %>%
#'   store(shopping, .) %>%
#'   head()
#'
#' @seealso \code{\link{full_factor}} to generate results
#' @seealso \code{\link{summary.full_factor}} to summarize results
#' @seealso \code{\link{plot.full_factor}} to plot results
#'
#' @export
store.full_factor <- function(dataset, object, name = "", ...) {
  if (is_empty(name)) name <- "factor"
  fscores <- as.data.frame(object$fres$scores, stringsAsFactors = FALSE)
  indr <- indexr(dataset, object$vars, object$data_filter)
  fs <- data.frame(matrix(NA, nrow = indr$nr, ncol = ncol(fscores)), stringsAsFactors = FALSE)
  fs[indr$ind, ] <- fscores
  dataset[, paste0(name, 1:ncol(fscores))] <- fs
  dataset
}

#' Sort and clean loadings
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/full_factor.html} for an example in Radiant
#'
#' @param floadings Data frame with loadings
#' @param fsort Sort factor loadings
#' @param cutoff Show only loadings with (absolute) values above cutoff (default = 0)
#' @param dec Number of decimals to show
#' @param repl Replace loadings below the cutoff by NA (or "")
#'
#' @examples
#' result <- full_factor(shopping, "v1:v6", nr_fact = 2)
#' clean_loadings(result$floadings, fsort = TRUE, cutoff = .5, dec = 2)
#'
#' @importFrom psych fa.sort
#'
#' @export
clean_loadings <- function(
  floadings, cutoff = 0, fsort = FALSE, dec = 8, repl = NA
) {

  if (fsort) {
    floadings <- select(psych::fa.sort(floadings), -order)
  }

  if (cutoff == 0) {
    floadings %<>% round(dec)
  } else {
    ind <- abs(floadings) < cutoff
    floadings %<>% round(dec)
    floadings[ind] <- repl
  }
  floadings
}
