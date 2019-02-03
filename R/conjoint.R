#' Conjoint analysis
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/conjoint.html} for an example in Radiant
#'
#' @param dataset Dataset
#' @param rvar The response variable (e.g., profile ratings)
#' @param evar Explanatory variables in the regression
#' @param int Interaction terms to include in the model
#' @param by Variable to group data by before analysis (e.g., a respondent id)
#' @param reverse Reverse the values of the response variable (`rvar`)
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#'
#' @return A list with all variables defined in the function as an object of class conjoint
#'
#' @examples
#' conjoint(mp3, rvar = "Rating", evar = "Memory:Shape") %>% str()
#'
#' @seealso \code{\link{summary.conjoint}} to summarize results
#' @seealso \code{\link{plot.conjoint}} to plot results
#'
#' @export
conjoint <- function(
  dataset, rvar, evar,
  int = "", by = "none",
  reverse = FALSE, data_filter = ""
) {

  vars <- c(rvar, evar)
  if (by != "none") vars <- c(vars, by)
  df_name <- if (is_string(dataset)) dataset else deparse(substitute(dataset))
  dataset <- get_data(dataset, vars, filt = data_filter)
  radiant.model::var_check(evar, colnames(dataset)[-1], int) %>%
    {vars <<- .$vars; evar <<- .$ev; int <<- .$intv}

  ## in case : was used to select a range of variables
  # evar <- colnames(dataset)[-1]
  if (!is_empty(by, "none")) {
    evar <- base::setdiff(evar, by)
    vars <- base::setdiff(vars, by)
    bylevs <- dataset[[by]] %>%
      as_factor() %>%
      levels()
    model_list <- vector("list", length(bylevs)) %>% set_names(bylevs)
  } else {
    bylevs <- "full"
    model_list <- list(full = list(model = NA, coeff = NA, tab = NA))
  }

  formula <- paste(rvar, "~", paste(vars, collapse = " + ")) %>% as.formula()

  for (i in seq_along(bylevs)) {
    if (!by == "none") {
      cdat <- filter(dataset, .data[[by]] == bylevs[i]) %>%
        select_at(.vars = base::setdiff(colnames(dataset), by))
    } else {
      cdat <- dataset
    }

    if (reverse) {
      cdat[[rvar]] <- cdat[[rvar]] %>% {(max(.) + 1) - .}
    }

    model <- sshhr(lm(formula, data = cdat))
    coeff <- tidy(model) %>% as.data.frame()
    tab <- the_table(coeff, cdat, evar)

    coeff$sig_star <- sig_stars(coeff$p.value) %>%
      format(justify = "left")
    colnames(coeff) <- c("label", "coefficient", "std.error", "t.value", "p.value", "sig_star")
    hasLevs <- sapply(select(dataset, -1), function(x) is.factor(x) || is.logical(x) || is.character(x))
    if (sum(hasLevs) > 0) {
      for (j in names(hasLevs[hasLevs])) {
        coeff$label %<>% gsub(paste0("^", j), paste0(j, "|"), .) %>%
          gsub(paste0(":", j), paste0(":", j, "|"), .)
      }
      rm(j, hasLevs)
    }
    model_list[[bylevs[i]]] <- list(model = model, coeff = coeff, tab = tab)
  }

  ## creating PW and IW data.frames
  if (!is_empty(by, "none")) {
    cn <- gsub("\\|", "_", model_list[[1]]$coeff$label) %>%
      gsub("[^A-z0-9_\\.]", "", .)

    PW <- matrix(NA, nrow = length(bylevs), ncol = length(cn) + 1) %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      set_colnames(c(by, cn))
    PW[[by]] <- bylevs

    for (i in seq_along(bylevs)) {
      PW[i, 2:ncol(PW)] <- model_list[[bylevs[i]]]$coeff$coefficient
    }

    ## creating IW data.frame
    cn <- model_list[[1]]$tab$IW$Attribute %>%
      gsub("[^A-z0-9_\\.]", "", .)

    IW <- matrix(NA, nrow = length(bylevs), ncol = length(cn) + 1) %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      set_colnames(c(by, cn))
    IW[[by]] <- bylevs

    for (i in seq_along(bylevs)) {
      IW[i, 2:ncol(IW)] <- model_list[[bylevs[i]]]$tab$IW$IW
    }
    rm(cn)
  }

  rm(model, coeff, tab)

  as.list(environment()) %>% add_class("conjoint")
}

#' Summary method for the conjoint function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/conjoint.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{conjoint}}
#' @param show Level in by variable to analyze (e.g., a specific respondent)
#' @param mc_diag Shows multicollinearity diagnostics.
#' @param additional Show additional regression results
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- conjoint(mp3, rvar = "Rating", evar = "Memory:Shape")
#' summary(result, mc_diag = TRUE)
#'
#' @seealso \code{\link{conjoint}} to generate results
#' @seealso \code{\link{plot.conjoint}} to plot results
#'
#' @importFrom car vif
#'
#' @export
summary.conjoint <- function(
  object, show = "", mc_diag = FALSE,
  additional = FALSE, dec = 3, ...
) {

  cat("Conjoint analysis\n")
  cat("Data                 :", object$df_name, "\n")
  if (!is_empty(object$data_filter)) {
    cat("Filter               :", gsub("\\n", "", object$data_filter), "\n")
  }
  if (object$by != "none") {
    cat("Show                 :", object$by, "==", show, "\n")
  }
  rvar <- if (object$reverse) paste0(object$rvar, " (reversed)") else object$rvar
  cat("Response variable    :", rvar, "\n")
  cat("Explanatory variables:", paste0(object$evar, collapse = ", "), "\n\n")

  if (object$by == "none" || is_empty(show) || !show %in% names(object$model_list)) {
    show <- names(object$model_list)[1]
  }

  tab <- object$model_list[[show]]$tab
  cat("Conjoint part-worths:\n")
  tab$PW[,1:2] %<>% format(justify = "left")
  print(format_df(tab$PW, dec), row.names = FALSE)
  cat("\nConjoint importance weights:\n")
  tab$IW[,1:2] %<>% format(justify = "left")
  print(format_df(tab$IW, dec), row.names = FALSE)
  cat("\nConjoint regression results:\n\n")

  coeff <- object$model_list[[show]]$coeff
  coeff$label %<>% format(justify = "left")
  if (!additional) {
    coeff[, 2] %<>% {sprintf(paste0("%.", dec, "f"), .)}
    print(rename(coeff[, 1:2], `  ` = "label"), row.names = FALSE)
    cat("\n")
  } else {
    if (all(coeff$p.value == "NaN")) {
      coeff[, 2] %<>% {sprintf(paste0("%.", dec, "f"), .)}
      print(rename(coeff[, 1:2], `  ` = "label"), row.names = FALSE)
      cat("\nInsufficient variation in explanatory variable(s) to report additional statistics")
      return()
    } else {
      p.small <- coeff$p.value < .001
      coeff[, 2:5] %<>% format_df(dec)
      coeff$p.value[p.small] <- "< .001"
      print(rename(coeff, `  ` = "label", ` ` = "sig_star"), row.names = FALSE)
    }

    model <- object$model_list[[show]]$model

    if (nrow(model$model) <= (length(object$evar) + 1)) {
      return("\nInsufficient observations to estimate model")
    }

    ## adjusting df for included intercept term
    df_int <- if (attr(model$terms, "intercept")) 1L else 0L

    reg_fit <- glance(model) %>% round(dec)
    if (reg_fit["p.value"] < .001) reg_fit["p.value"] <- "< .001"
    cat("\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")
    cat("R-squared:", paste0(reg_fit$r.squared, ", "), "Adjusted R-squared:", reg_fit$adj.r.squared, "\n")
    cat("F-statistic:", reg_fit$statistic, paste0("df(", reg_fit$df - df_int, ",", reg_fit$df.residual, "), p.value"), reg_fit$p.value)
    cat("\nNr obs:", format_nr(reg_fit$df + reg_fit$df.residual, dec = 0), "\n\n")

    if (anyNA(model$coeff)) {
      cat("The set of explanatory variables exhibit perfect multicollinearity.\nOne or more variables were dropped from the estimation.\n")
    }
  }

  if (mc_diag) {
    if (length(object$evar) > 1) {
      cat("Multicollinearity diagnostics:\n")
      car::vif(object$model_list[[show]]$model) %>%
        {if (!dim(.) %>% is.null()) .[, "GVIF"] else .} %>% # needed when factors are included
        data.frame(
          VIF = .,
          Rsq = 1 - 1 / .,
          stringsAsFactors = FALSE
        ) %>%
        round(dec) %>%
        .[order(.$VIF, decreasing = T), ] %>%
        {if (nrow(.) < 8) t(.) else .} %>%
        print()
    } else {
      cat("Insufficient number of attributes selected to calculate\nmulticollinearity diagnostics")
    }
  }
}

#' Predict method for the conjoint function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/conjoint.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{conjoint}}
#' @param pred_data Provide the dataframe to generate predictions. The dataset must contain all columns used in the estimation
#' @param pred_cmd Command used to generate data for prediction
#' @param conf_lev Confidence level used to estimate confidence intervals (.95 is the default)
#' @param se Logical that indicates if prediction standard errors should be calculated (default = FALSE)
#' @param interval Type of interval calculation ("confidence" or "prediction"). Set to "none" if se is FALSE
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{conjoint}} to generate the result
#' @seealso \code{\link{summary.conjoint}} to summarize results
#' @seealso \code{\link{plot.conjoint}} to plot results
#'
#' @examples
#' result <- conjoint(mp3, rvar = "Rating", evar = "Memory:Shape")
#' predict(result, pred_data = mp3)
#'
#' @importFrom radiant.model predict_model
#'
#' @export
predict.conjoint <- function(
  object, pred_data = NULL, pred_cmd = "",
  conf_lev = 0.95, se = FALSE,
  interval = "confidence", dec = 3,
  ...
) {

  if (is.character(object)) return(object)
  if (!isTRUE(se)) {
    interval <- "none"
  } else if (isTRUE(interval == "none")) {
    se <- FALSE
  }

  ## ensure you have a name for the prediction dataset
  if (is.data.frame(pred_data)) {
    df_name <- deparse(substitute(pred_data))
  } else {
    df_name <- pred_data
  }

  pfun <- function(model, pred, se, conf_lev) {
    pred_val <-
      try(
        sshhr(
          predict(model, pred, interval = ifelse(se, interval, "none"), level = conf_lev)
        ),
        silent = TRUE
      )

    if (!inherits(pred_val, "try-error")) {
      if (se) {
        pred_val %<>% data.frame(stringsAsFactors = FALSE) %>% mutate(diff = .[, 3] - .[, 1])
        ci_perc <- ci_label(cl = conf_lev)
        colnames(pred_val) <- c("Prediction", ci_perc[1], ci_perc[2], "+/-")
      } else {
        pred_val %<>% data.frame(stringsAsFactors = FALSE) %>% select(1)
        colnames(pred_val) <- "Prediction"
      }
    }

    pred_val
  }

  if (is_empty(object$by, "none")) {
    object$model <- object$model_list[["full"]]$model
    predict_model(object, pfun, "conjoint.predict", pred_data, pred_cmd, conf_lev, se, dec) %>%
      set_attr("radiant_interval", interval) %>%
      set_attr("radiant_pred_data", df_name)

  } else {
    predict_conjoint_by(object, pfun, pred_data, pred_cmd, conf_lev, se, dec) %>%
      set_attr("radiant_interval", interval) %>%
      set_attr("radiant_pred_data", df_name)
  }
}

#' Predict method for the conjoint function when a by variables is used
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/conjoint.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{conjoint}}
#' @param pfun Function to use for prediction
#' @param pred_data Name of the dataset to use for prediction
#' @param pred_cmd Command used to generate data for prediction
#' @param conf_lev Confidence level used to estimate confidence intervals (.95 is the default)
#' @param se Logical that indicates if prediction standard errors should be calculated (default = FALSE)
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{conjoint}} to generate the result
#' @seealso \code{\link{summary.conjoint}} to summarize results
#' @seealso \code{\link{plot.conjoint}} to plot results
#'
#' @importFrom radiant.model predict_model
#'
#' @export
predict_conjoint_by <- function(
  object, pfun, pred_data = NULL, pred_cmd = "",
  conf_lev = 0.95, se = FALSE, dec = 3,
  ...
) {

  if (is.character(object)) return(object)
  ## ensure you have a name for the prediction dataset
  if (is.data.frame(pred_data)) {
    attr(pred_data, "radiant_pred_data") <- deparse(substitute(pred_data))
  }

  pred <- list()
  bylevs <- object$bylevs

  for (i in seq_along(bylevs)) {
    object$model <- object$model_list[[bylevs[i]]]$model
    pred[[i]] <- predict_model(object, pfun, "conjoint.predict", pred_data, pred_cmd, conf_lev, se, dec)

    ## when se is true reordering the columns removes attributes for some reason
    if (i == 1) att <- attributes(pred[[1]])

    if (is.character(pred[[i]])) return(pred[[i]])
    pred[[i]] %<>% {.[[object$by]] <- bylevs[i]; .} %>%
      {.[, c(object$by, head(colnames(.), -1))]}
  }

  pred <- bind_rows(pred)
  att$row.names <- 1:nrow(pred)
  att$vars <- att$names <- colnames(pred)
  attributes(pred) <- att
  add_class(pred, "conjoint.predict.by") %>%
    add_class("conjoint.predict")
}

#' Print method for predict.conjoint
#'
#' @param x Return value from prediction method
#' @param ... further arguments passed to or from other methods
#' @param n Number of lines of prediction results to print. Use -1 to print all lines
#'
#' @importFrom radiant.model print_predict_model
#'
#' @export
print.conjoint.predict <- function(x, ..., n = 20)
  print_predict_model(x, ..., n = n, header = "Conjoint Analysis")

#' Plot method for the conjoint function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/conjoint.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{conjoint}}
#' @param plots Show either the part-worth ("pw") or importance-weights ("iw") plot
#' @param show Level in by variable to analyze (e.g., a specific respondent)
#' @param scale_plot Scale the axes of the part-worth plots to the same range
#' @param shiny Did the function call originate inside a shiny app
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This option can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{http://docs.ggplot2.org} for options.
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- conjoint(mp3, rvar = "Rating", evar = "Memory:Shape")
#' plot(result, scale_plot = TRUE)
#' plot(result, plots = "iw")
#'
#' @seealso \code{\link{conjoint}} to generate results
#' @seealso \code{\link{summary.conjoint}} to summarize results
#'
#' @export
plot.conjoint <- function(
  x, plots = "pw", show = "", scale_plot = FALSE,
  shiny = FALSE, custom = FALSE, ...
) {

  if (x$by == "none" || is_empty(show) || !show %in% names(x$model_list)) {
    show <- names(x$model_list)[1]
  }

  the_table <- x$model_list[[show]]$tab
  plot_ylim <- the_table$plot_ylim
  plot_list <- list()

  if ("pw" %in% plots) {
    PW.df <- the_table[["PW"]]

    lab <- if (x$by == "none") "" else paste0("(", show, ")")

    for (var in x$evar) {
      PW.var <- PW.df[PW.df[["Attributes"]] == var, ]

      # setting the levels in the same order as in the_table. Without this
      # ggplot would change the ordering of the price levels
      PW.var$Levels <- factor(PW.var$Levels, levels = PW.var$Levels, ordered = FALSE)

      p <- ggplot(PW.var, aes_string(x = "Levels", y = "PW", group = 1)) +
        geom_line(color = "blue", linetype = "dotdash", size = .7) +
        geom_point(color = "blue", size = 4, shape = 21, fill = "white") +
        labs(title = paste("Part-worths for", var, lab), x = "") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      if (scale_plot) {
        p <- p + ylim(plot_ylim[var, "Min"], plot_ylim[var, "Max"])
      }
      plot_list[[var]] <- p
    }
  }

  if ("iw" %in% plots) {
    IW.df <- the_table[["IW"]]
    lab <- if (x$by == "none") "" else paste0(" (", show, ")")
    plot_list[["iw"]] <- ggplot(IW.df, aes_string(x = "Attributes", y = "IW", fill = "Attributes")) +
      geom_bar(stat = "identity", alpha = 0.5) +
      theme(legend.position = "none") +
      labs(title = paste0("Importance weights", lab))
  }

  if (length(plot_list) == 0) return(invisible())

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

#' Function to calculate the PW and IW table for conjoint
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/conjoint.html} for an example in Radiant
#'
#' @param model Tidied model results (broom) output from \code{\link{conjoint}} passed on by summary.conjoint
#' @param dataset Conjoint data
#' @param evar Explanatory variables used in the conjoint regression
#'
#' @examples
#' result <- conjoint(mp3, rvar = "Rating", evar = "Memory:Shape")
#' the_table(tidy(result$model_list[[1]][["model"]]), result$dataset, result$evar)
#'
#' @seealso \code{\link{conjoint}} to generate results
#' @seealso \code{\link{summary.conjoint}} to summarize results
#' @seealso \code{\link{plot.conjoint}} to plot results
#'
#' @export
the_table <- function(model, dataset, evar) {
  if (is.character(model)) return(list("PW" = "No attributes selected."))

  attr <- select_at(dataset, .vars = evar) %>%
    mutate_if(is.logical, as.factor) %>%
    mutate_if(is.character, as.factor)

  isFct <- sapply(attr, is.factor)
  if (sum(isFct) < ncol(attr)) return(list("PW" = "Only factors can be used.", "IW" = "Only factors can be used."))
  bylevs <- lapply(attr[, isFct, drop = FALSE], levels)
  vars <- colnames(attr)[isFct]

  nlevs <- sapply(bylevs, length)
  PW.df <- data.frame(rep(vars, nlevs), unlist(bylevs), stringsAsFactors = FALSE)
  colnames(PW.df) <- c("Attributes", "Levels")
  PW.df$PW <- 0

  ## Calculate PW and IW's when interactions are present
  ## http://www.slideshare.net/SunnyBose/conjoint-analysis-12090511
  rownames(PW.df) <- paste(PW.df[["Attributes"]], PW.df[["Levels"]], sep = "")

  coeff <- model$estimate
  PW.df[model$term[-1], "PW"] <- coeff[-1]

  minPW <- PW.df[tapply(1:nrow(PW.df), PW.df$Attributes, function(i) i[which.min(PW.df$PW[i])]), ]
  maxPW <- PW.df[tapply(1:nrow(PW.df), PW.df$Attributes, function(i) i[which.max(PW.df$PW[i])]), ]
  rownames(minPW) <- minPW$Attributes
  rownames(maxPW) <- maxPW$Attributes

  rangePW <- data.frame(cbind(maxPW[vars, "PW"], minPW[vars, "PW"]), stringsAsFactors = FALSE)
  rangePW$Range <- rangePW[[1]] - rangePW[[2]]
  colnames(rangePW) <- c("Max", "Min", "Range")
  rownames(rangePW) <- vars

  ## for plot range if standardized
  maxlim <- rangePW[["Max"]] > abs(rangePW[["Min"]])
  maxrange <- max(rangePW[["Range"]])
  plot_ylim <- rangePW[c("Min", "Max")]

  plot_ylim[maxlim, "Max"] <- plot_ylim[maxlim, "Max"] + maxrange - rangePW$Range[maxlim]
  plot_ylim[!maxlim, "Min"] <- plot_ylim[!maxlim, "Min"] - (maxrange - rangePW$Range[!maxlim])
  plot_ylim <- plot_ylim * 1.01 ## expanded max to avoid hiding max points in plot

  IW <- data.frame(vars, stringsAsFactors = FALSE)
  IW$IW <- rangePW$Range / sum(rangePW$Range)
  colnames(IW) <- c("Attributes", "IW")

  PW.df[["Attributes"]] <- as.character(PW.df[["Attributes"]])
  PW.df[["Levels"]] <- as.character(PW.df[["Levels"]])
  PW.df <- rbind(PW.df, c("Base utility", "~", coeff[1]))
  PW.df[["PW"]] <- as.numeric(PW.df[["PW"]])

  PW.df[["PW"]] <- round(PW.df[["PW"]], 3)
  IW[["IW"]] <- round(IW[["IW"]], 3)

  list("PW" = PW.df, "IW" = IW, "plot_ylim" = plot_ylim)
}

#' Store method for the Multivariate > Conjoint tab
#'
#' @details Store data frame with PWs or IWs in Radiant r_data list if available
#'
#' @param dataset Dataset
#' @param object Return value from conjoint
#' @param name Variable name(s) assigned to predicted values
#' @param ... further arguments passed to or from other methods
#'
#' @export
store.conjoint <- function(dataset, object, name, ...) {
  if (missing(name)) {
    object$tab
  } else {
    print(list(...))
    stop(
      paste0(
        "This function is deprecated. Use the code below for part worths instead:\n\n",
        name, " <- ", deparse(substitute(object)), "$PW\nregister(\"",
        name, ")\n\n",
        "This function is deprecated. Use the code below for importance weights instead:\n\n",
        name, " <- ", deparse(substitute(object)), "$IW\nregister(\"",
        name, ")"
      ),
      call. = FALSE
    )
  }
}

##' Store predicted values generated in predict.conjoint
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/conjoint.html} for an example in Radiant
#'
#' @param dataset Dataset to add predictions to
#' @param object Return value from model predict function
#' @param name Variable name(s) assigned to predicted values
#' @param ... Additional arguments
#'
#' @examples
#' conjoint(mp3, rvar = "Rating", evar = "Memory:Shape") %>%
#'   predict(mp3) %>%
#'   store(mp3, ., name = "pred_pref")
#'
#' @export
store.conjoint.predict <- function(dataset, object, name = "prediction", ...) {
  radiant.model:::store.model.predict(dataset, object, name = name, ...)
}
