#' Attribute based brand maps
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/prmap.html} for an example in Radiant
#'
#' @param dataset Dataset
#' @param brand A character variable with brand names
#' @param attr Names of numeric variables
#' @param pref Names of numeric brand preference measures
#' @param nr_dim Number of dimensions
#' @param hcor Use polycor::hetcor to calculate the correlation matrix
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param envir Environment to extract data from
#'
#' @return A list of all variables defined in the function as an object of class prmap
#'
#' @examples
#' prmap(computer, brand = "brand", attr = "high_end:business") %>% str()
#'
#' @seealso \code{\link{summary.prmap}} to summarize results
#' @seealso \code{\link{plot.prmap}} to plot results
#'
#' @importFrom psych principal
#' @importFrom lubridate is.Date
#' @importFrom polycor hetcor
#'
#' @export
prmap <- function(dataset, brand, attr, pref = "", nr_dim = 2, hcor = FALSE,
                  data_filter = "", envir = parent.frame()) {
  nr_dim <- as.numeric(nr_dim)
  vars <- c(brand, attr)
  if (!is.empty(pref)) vars <- c(vars, pref)
  df_name <- if (is_string(dataset)) dataset else deparse(substitute(dataset))
  dataset <- get_data(dataset, vars, filt = data_filter, envir = envir)

  brands <- dataset[[brand]] %>%
    as.character() %>%
    gsub("^\\s+|\\s+$", "", .)
  f_data <- get_data(dataset, attr, envir = envir)
  anyCategorical <- sapply(f_data, function(x) is.numeric(x) || is.Date(x)) == FALSE
  nrObs <- nrow(dataset)

  # in case : is used
  if (length(attr) < ncol(f_data)) attr <- colnames(f_data)
  if (nr_dim > length(attr)) {
    return("The number of dimensions cannot exceed the number of attributes" %>%
      add_class("prmap"))
  }

  if (hcor) {
    f_data <- mutate_if(f_data, is.Date, as.numeric)
    cmat <- try(sshhr(polycor::hetcor(f_data, ML = FALSE, std.err = FALSE)$correlations), silent = TRUE)
    f_data <- mutate_all(f_data, radiant.data::as_numeric)
    if (inherits(cmat, "try-error")) {
      message("Calculating the heterogeneous correlation matrix produced an error.\nUsing standard correlation matrix instead")
      hcor <- "Calculation failed"
      cmat <- cor(f_data)
    }
  } else {
    f_data <- mutate_all(f_data, radiant.data::as_numeric)
    cmat <- cor(f_data)
  }

  fres <- sshhr(psych::principal(
    cmat,
    nfactors = nr_dim, rotate = "varimax",
    scores = FALSE, oblique.scores = FALSE
  ))

  m <- fres$loadings[, colnames(fres$loadings)]
  cscm <- m %*% solve(crossprod(m))
  ## store in fres so you can re-use save_factors
  fres$scores <- scale(as.matrix(f_data), center = TRUE, scale = TRUE) %*% cscm
  rownames(fres$scores) <- brands

  scores <- data.frame(fres$scores) %>%
    mutate(brands = brands) %>%
    group_by_at("brands") %>%
    summarise_all(mean) %>%
    as.data.frame() %>%
    set_rownames(.[["brands"]]) %>%
    select(-1)

  if (!is.empty(pref)) {
    p_data <- get_data(dataset, pref, envir = envir) %>%
      mutate_if(is.Date, as.numeric)
    anyPrefCat <- sapply(p_data, function(x) is.numeric(x)) == FALSE
    if (sum(anyPrefCat) > 0) {
      pref_cor <- sshhr(polycor::hetcor(cbind(p_data, fres$scores), ML = FALSE, std.err = FALSE)$correlations)
      pref_cor <- as.data.frame(pref_cor[-((length(pref) + 1):nrow(pref_cor)), -(1:length(pref))], stringsAsFactor = FALSE)
    } else {
      pref_cor <- p_data %>%
        cor(fres$scores) %>%
        data.frame(stringsAsFactors = FALSE)
    }
    pref <- colnames(pref_cor)
    pref_cor$communalities <- rowSums(pref_cor^2)
    rm(p_data, anyPrefCat)
  }

  rm(f_data, m, cscm, envir)
  as.list(environment()) %>% add_class(c("prmap", "full_factor"))
}

#' Summary method for the prmap function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/prmap.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{prmap}}
#' @param cutoff Show only loadings with (absolute) values above cutoff (default = 0)
#' @param dec Rounding to use for output
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- prmap(computer, brand = "brand", attr = "high_end:business")
#' summary(result)
#' summary(result, cutoff = .3)
#' prmap(
#'   computer,
#'   brand = "brand", attr = "high_end:dated",
#'   pref = c("innovative", "business")
#' ) %>% summary()
#'
#' @seealso \code{\link{prmap}} to calculate results
#' @seealso \code{\link{plot.prmap}} to plot results
#'
#' @export
summary.prmap <- function(object, cutoff = 0, dec = 2, ...) {
  if (is.character(object)) {
    return(object)
  }

  cat("Attribute based brand map\n")
  cat("Data        :", object$df_name, "\n")
  if (!is.empty(object$data_filter)) {
    cat("Filter      :", gsub("\\n", "", object$data_filter), "\n")
  }
  cat("Attributes  :", paste0(object$attr, collapse = ", "), "\n")
  if (!is.empty(object$pref)) {
    cat("Preferences :", paste0(object$pref, collapse = ", "), "\n")
  }
  cat("Dimensions  :", object$nr_dim, "\n")
  cat("Rotation    : varimax\n")
  cat("Observations:", object$nrObs, "\n")
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

  cat("Brand - Factor scores:\n")
  round(object$scores, dec) %>% print()

  cat("\nAttribute - Factor loadings:\n")

  ## convert loadings object to data.frame
  lds <- object$fres$loadings
  dn <- dimnames(lds)
  lds %<>% matrix(nrow = length(dn[[1]])) %>%
    set_colnames(dn[[2]]) %>%
    set_rownames(dn[[1]]) %>%
    data.frame(stringsAsFactors = FALSE)

  ## show only the loadings > ff_cutoff
  ind <- abs(lds) < cutoff
  print_lds <- round(lds, dec)
  print_lds[ind] <- ""
  print(print_lds)

  if (!is.empty(object$pref)) {
    cat("\nPreference correlations:\n")
    print(round(object$pref_cor, dec), digits = dec)
  }

  ## fit measures
  cat("\nFit measures:\n")
  colSums(lds^2) %>%
    rbind(., 100 * (. / length(dn[[1]]))) %>%
    rbind(., cumsum(.[2, ])) %>%
    round(dec) %>%
    set_rownames(c("Eigenvalues", "Variance %", "Cumulative %")) %>%
    print()

  cat("\nAttribute communalities:")
  data.frame(1 - object$fres$uniqueness, stringsAsFactors = FALSE) %>%
    set_colnames("") %>%
    round(dec) %>%
    print()
}

#' Plot method for the prmap function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/prmap.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{prmap}}
#' @param plots Components to include in the plot ("brand", "attr"). If data on preferences is available use "pref" to add preference arrows to the plot
#' @param scaling Arrow scaling in the brand map
#' @param fontsz Font size to use in plots
#' @param seed Random seed
#' @param shiny Did the function call originate inside a shiny app
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This option can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{https://ggplot2.tidyverse.org/} for options.
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- prmap(computer, brand = "brand", attr = "high_end:business")
#' plot(result, plots = "brand")
#' plot(result, plots = c("brand", "attr"))
#' plot(result, scaling = 1, plots = c("brand", "attr"))
#' prmap(
#'   retailers,
#'   brand = "retailer",
#'   attr = "good_value:cluttered",
#'   pref = c("segment1", "segment2")
#' ) %>% plot(plots = c("brand", "attr", "pref"))
#'
#' @seealso \code{\link{prmap}} to calculate results
#' @seealso \code{\link{summary.prmap}} to plot results
#'
#' @importFrom ggrepel geom_text_repel
#' @importFrom rlang .data
#'
#' @export
plot.prmap <- function(x, plots = "", scaling = 2, fontsz = 5, seed = 1234,
                       shiny = FALSE, custom = FALSE, ...) {
  if (is.character(x)) {
    return(x)
  }

  ## set seed for ggrepel label positioning
  set.seed(seed)

  ## need for dplyr as.symbol
  type <- rnames <- NULL

  pm_dat <- list()
  ## brand coordinates
  pm_dat$brand <- as.data.frame(x$scores) %>%
    set_colnames(paste0("dim", seq_len(ncol(.)))) %>%
    mutate(rnames = rownames(.), type = "brand")

  ## preference coordinates
  if (!is.empty(x$pref_cor)) {
    pm_dat$pref <- x$pref_cor %>%
      select(-ncol(.)) %>%
      set_colnames(paste0("dim", seq_len(ncol(.)))) %>%
      (function(x) x * scaling) %>%
      mutate(rnames = rownames(.), type = "pref")
  } else {
    plots <- base::setdiff(plots, "pref")
  }

  ## attribute coordinates
  std_m <- x$fres$loadings
  dn <- dimnames(std_m)
  pm_dat$attr <- std_m %>%
    matrix(nrow = length(dn[[1]])) %>%
    set_colnames(paste0("dim", seq_len(ncol(.)))) %>%
    set_rownames(dn[[1]]) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    (function(x) x * scaling) %>%
    mutate(rnames = rownames(.), type = "attr")

  ## combining data
  pm_dat <- bind_rows(pm_dat)

  ## set plot limits
  isNum <- sapply(pm_dat, is.numeric)
  lim <- max(abs(select(pm_dat, which(isNum))))

  label_colors <- c(brand = "black", attr = "darkblue", pref = "red")
  plot_list <- list()
  for (i in 1:(x$nr_dim - 1)) {
    for (j in (i + 1):x$nr_dim) {
      i_name <- paste0("dim", i)
      j_name <- paste0("dim", j)
      p <- ggplot() +
        theme(legend.position = "none") +
        coord_cartesian(xlim = c(-lim, lim), ylim = c(-lim, lim)) +
        geom_vline(xintercept = 0, linewidth = 0.3) +
        geom_hline(yintercept = 0, linewidth = 0.3) +
        labs(
          x = paste("Dimension", i),
          y = paste("Dimension", j)
        )

      if (!is.empty(plots)) {
        p <- p + ggrepel::geom_text_repel(
          data = filter(pm_dat, !!as.symbol("type") %in% plots),
          aes(x = .data[[i_name]], y = .data[[j_name]], label = .data$rnames, color = .data$type),
          size = fontsz
        ) +
          scale_color_manual(values = label_colors)

        if ("brand" %in% plots) {
          p <- p + geom_point(data = filter(pm_dat, !!as.symbol("type") == "brand"), aes(x = .data[[i_name]], y = .data[[j_name]]))
        }

        if (any(c("attr", "pref") %in% plots)) {
          pm_arrows <- filter(pm_dat, !!as.symbol("type") %in% base::setdiff(plots, "brand"))
          pm_arrows[, isNum] <- pm_arrows[, isNum] * 0.9
          p <- p + geom_segment(
            data = pm_arrows, aes(x = 0, y = 0, xend = .data[[i_name]], yend = .data[[j_name]], color = .data$type),
            arrow = arrow(length = unit(0.01, "npc"), type = "closed"), linewidth = 0.3, linetype = "dashed"
          )
        }
      }
      plot_list[[paste0("dim", i, "_dim", j)]] <- p
    }
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
