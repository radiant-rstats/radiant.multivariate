#' Evaluate if data are appropriate for PCA / Factor analysis
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/pre_factor.html} for an example in Radiant
#'
#' @param dataset Dataset
#' @param vars Variables to include in the analysis
#' @param hcor Use polycor::hetcor to calculate the correlation matrix
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param envir Environment to extract data from
#'
#' @return A list with all variables defined in the function as an object of class pre_factor
#'
#' @examples
#' pre_factor(shopping, "v1:v6") %>% str()
#'
#' @seealso \code{\link{summary.pre_factor}} to summarize results
#' @seealso \code{\link{plot.pre_factor}} to plot results
#'
#' @importFrom psych KMO cortest.bartlett
#' @importFrom lubridate is.Date
#' @importFrom polycor hetcor
#'
#' @export
pre_factor <- function(dataset, vars, hcor = FALSE, data_filter = "", envir = parent.frame()) {
  df_name <- if (is_string(dataset)) dataset else deparse(substitute(dataset))
  dataset <- get_data(dataset, vars, filt = data_filter, envir = envir)
  nrObs <- nrow(dataset)

  ## in case : is used
  if (length(vars) < ncol(dataset)) {
    vars <- colnames(dataset)
  }

  anyCategorical <- sapply(dataset, function(x) is.numeric(x) || is.Date(x)) == FALSE

  if (hcor) {
    dataset <- mutate_if(dataset, is.Date, as.numeric)
    cmat <- try(sshhr(polycor::hetcor(dataset, ML = FALSE, std.err = FALSE)$correlations), silent = TRUE)
    dataset <- mutate_all(dataset, radiant.data::as_numeric)
    if (inherits(cmat, "try-error")) {
      message("Calculating the heterogenous correlation matrix produced an error.\nUsing standard correlation matrix instead")
      hcor <- "Calculation failed"
      cmat <- cor(dataset)
    }
  } else {
    dataset <- mutate_all(dataset, radiant.data::as_numeric)
    cmat <- cor(dataset)
  }

  btest <- psych::cortest.bartlett(cmat, nrow(dataset))
  pre_kmo <- psych::KMO(cmat)
  pre_eigen <- eigen(cmat)$values

  if (det(cmat) > 0) {
    scmat <- try(solve(cmat), silent = TRUE)
    if (inherits(scmat, "try-error")) {
      pre_r2 <- matrix(NA, nrow = nrow(cmat), ncol = 1)
      rownames(pre_r2) <- rownames(cmat)
    } else {
      pre_r2 <- (1 - (1 / diag(scmat))) %>%
        data.frame(stringsAsFactors = FALSE) %>%
        set_colnames("Rsq")
    }
  } else {
    pre_r2 <- matrix(NA, nrow = nrow(cmat), ncol = 1)
    rownames(pre_r2) <- rownames(cmat)
  }

  rm(dataset, envir)

  as.list(environment()) %>% add_class("pre_factor")
}

## Notes:
# KMO is a measure of the relative size of (1) correlations between variables
# and (2) the partial correlations between those variables. This makes sense
# if you are interested in factors that have links to > 2 variables.
# But what if your factors are based on 2 indicators/variables? In that case
# your KMO would be horrible but a simple factor analysis would still cut the
# dimensionality of your data in halve. Use VIF or tolerance to indicate if
# factor analysis makes sense?

#' Summary method for the pre_factor function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/pre_factor.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{pre_factor}}
#' @param dec Rounding to use for output
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- pre_factor(shopping, "v1:v6")
#' summary(result)
#' pre_factor(computer, "high_end:business") %>% summary()
#' @seealso \code{\link{pre_factor}} to calculate results
#' @seealso \code{\link{plot.pre_factor}} to plot results
#'
#' @export
summary.pre_factor <- function(object, dec = 2, ...) {
  if (is.character(object)) {
    return(cat(object))
  }

  if (is.character(object$pre_r2)) {
    cat(object$pre_r2)
    return(invisible())
  }

  cat("Pre-factor analysis diagnostics\n")
  cat("Data        :", object$df_name, "\n")
  if (!is.empty(object$data_filter)) {
    cat("Filter      :", gsub("\\n", "", object$data_filter), "\n")
  }
  cat("Variables   :", paste0(object$vars, collapse = ", "), "\n")
  cat("Observations:", format_nr(object$nrObs, dec = 0), "\n")
  if (is.character(object$hcor)) {
    cat(paste0("Correlation : Pearson (adjustment using polycor::hetcor failed)\n"))
  } else if (isTRUE(object$hcor)) {
    if (sum(object$anyCategorical) > 0) {
      cat(paste0("Correlation : Heterogeneous correlations using polycor::hetcor\n"))
    } else {
      cat(paste0("Correlation : Pearson\n"))
    }
  } else {
    cat("Correlation : Pearson\n")
  }
  if (sum(object$anyCategorical) > 0) {
    if (isTRUE(object$hcor)) {
      cat("** Variables of type {factor} are assumed to be ordinal **\n\n")
    } else {
      cat("** Variables of type {factor} included without adjustment **\n\n")
    }
  } else if (isTRUE(object$hcor)) {
    cat("** No variables of type {factor} selected. No adjustment applied **\n\n")
  } else {
    cat("\n")
  }

  btest <- object$btest
  cat("Bartlett test\n")
  cat("Null hyp. : variables are not correlated\n")
  cat("Alt. hyp. : variables are correlated\n")
  bt <- object$btest$p.value
  bt <- if (!is.empty(bt) && bt < .001) "< .001" else round(bt, dec + 1)
  cat(paste0(
    "Chi-square: ", round(object$btest$chisq, 2), " df(",
    object$btest$df, "), p.value ", bt, "\n"
  ))

  cat("\nKMO test: ", round(object$pre_kmo$MSA, dec), "\n")
  # cat("\nMeasures of sampling adequacy:\n")
  # print(object$pre_kmo$MSAi, digits = dec)

  cat("\nVariable collinearity:\n")
  if (all(is.na(object$pre_r2))) {
    cat("\nRsq measures could not be calculated because the selected variables\nare perfectly collinear. Please check the correlations and remove\nany variable with a correlation of 1 or -1 from the analysis\n\n")
  }
  data.frame(Rsq = object$pre_r2, KMO = object$pre_kmo$MSAi, stringsAsFactors = FALSE) %>%
    format_df(dec = dec) %>%
    set_rownames(rownames(object$pre_r2)) %>%
    print()

  ## fit measures,  using transposed format because there could be many factors
  cat("\nFit measures:\n")
  object$pre_eigen %>%
    {
      data.frame(
        ` ` = paste0("PC", 1:length(.)),
        Eigenvalues = .,
        `Variance %` = 100 * (. / sum(.)),
        `Cumulative %` = 100 * (cumsum(. / sum(.))),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    } %>%
    format_df(dec = dec) %>%
    print(row.names = FALSE)
}

#' Plot method for the pre_factor function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/pre_factor.html} for an example in Radiant
#' @param x Return value from \code{\link{pre_factor}}
#' @param plots Plots to return. "change" shows the change in eigenvalues as variables are grouped into different number of factors, "scree" shows a scree plot of eigenvalues
#' @param cutoff For large datasets plots can take time to render and become hard to interpret. By selection a cutoff point (e.g., eigenvalues of .8 or higher) factors with the least explanatory power are removed from the plot
#' @param shiny Did the function call originate inside a shiny app
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This option can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{https://ggplot2.tidyverse.org/} for options.
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- pre_factor(shopping, "v1:v6")
#' plot(result, plots = c("change", "scree"), cutoff = .05)
#' @seealso \code{\link{pre_factor}} to calculate results
#' @seealso \code{\link{summary.pre_factor}} to summarize results
#'
#' @export
plot.pre_factor <- function(x, plots = c("scree", "change"), cutoff = 0.2,
                            shiny = FALSE, custom = FALSE, ...) {
  if (is.character(x) || is.character(x$pre_r2) || length(plots) == 0) {
    return("")
  }

  cutoff <- ifelse(is_not(cutoff), .2, cutoff)
  pre_eigen <- with(x, pre_eigen[pre_eigen > cutoff])
  dat <- data.frame(y = pre_eigen, x = as.integer(1:length(pre_eigen)), stringsAsFactors = FALSE)

  plot_list <- list()
  if ("scree" %in% plots) {
    plot_list[[which("scree" == plots)]] <-
      ggplot(dat, aes(x = x, y = y, group = 1)) +
      geom_line(color = "blue", linetype = "dotdash", linewidth = .7) +
      geom_point(color = "blue", size = 4, shape = 21, fill = "white") +
      geom_hline(yintercept = 1, color = "black", linetype = "solid", linewidth = .5) +
      labs(title = "Screeplot", x = "# factors", y = "Eigenvalues") +
      scale_x_continuous(breaks = dat[["x"]])
  }

  if ("change" %in% plots) {
    plot_list[[which("change" == plots)]] <- pre_eigen %>%
      (function(x) (x - lag(x)) / lag(x)) %>%
      (function(x) x / min(x, na.rm = TRUE)) %>%
      data.frame(
        bump = .,
        nr_fact = paste0(0:(length(.) - 1), "-", 1:length(.)),
        stringsAsFactors = FALSE
      ) %>%
      na.omit() %>%
      ggplot(aes(x = factor(nr_fact, levels = nr_fact), y = bump)) +
      geom_bar(stat = "identity", alpha = 0.5, fill = "blue") +
      labs(
        title = paste("Change in Eigenvalues"),
        x = "# factors", y = "Rate of change index"
      )
  }

  if (length(plot_list) > 0) {
    if (custom) {
      if (length(plot_list) == 1) plot_list[[1]] else plot_list
    } else {
      patchwork::wrap_plots(plot_list, ncol = 1) %>%
        (function(x) if (shiny) x else print(x))
    }
  }
}
