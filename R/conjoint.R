#' Conjoint analysis
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/conjoint.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param rvar The response variable (e.g., profile ratings)
#' @param evar Explanatory variables in the regression
#' @param by Variable to group data by before analysis (e.g., a respondent id)
#' @param show Level in by variable to analyse (e.g., a specific respondent)
#' @param reverse Reverse the values of the response variable (`rvar`)
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#'
#' @return A list with all variables defined in the function as an object of class conjoint
#'
#' @examples
#' result <- conjoint("mp3", rvar = "Rating", evar = "Memory:Shape")
#' result <- mp3 %>% conjoint(rvar = "Rating", evar = "Memory:Shape")
#'
#' @seealso \code{\link{summary.conjoint}} to summarize results
#' @seealso \code{\link{plot.conjoint}} to plot results
#'
#' @export
conjoint <- function(dataset, rvar, evar,
                     by = "none",
                     show = NULL,
                     reverse = FALSE,
                     data_filter = "") {

	vars <- c(rvar, evar)
	if (by != "none") vars <- c(vars, by)
	dat <- getdata(dataset, vars, filt = data_filter)
	if (!is_string(dataset)) dataset <- "-----"

	## in case : was used to select a range of variables
	evar <- colnames(dat)[-1]
	if (by != "none") {
		evar <- setdiff(evar, by)
		if (is.null(show)) show <- dat[[by]] %>% as_factor %>% levels %>% .[1]
		## in case a data.frame was passed to conjoint(...)
    if(dataset == "-----") store_dat <- dat
		dat <- filter_(dat, paste0(by, " == ", show))
	}

	formula <- paste(rvar, "~", paste(evar, collapse = " + ")) %>% as.formula

	if (reverse) {
		ca_dep <- dat[[rvar]]
		dat[[rvar]] <- (max(ca_dep) + 1) - ca_dep
	}

	model <- lm(formula, data = dat)
	coeff <- tidy(model)
	the_table <- the_table(coeff, dat, evar)

  coeff$` ` <- sig_stars(coeff$p.value) %>% format(justify = "left")
  colnames(coeff) <- c("  ","coefficient","std.error","t.value","p.value"," ")
  isFct <- sapply(select(dat,-1), function(x) is.factor(x) || is.logical(x))
  if (sum(isFct) > 0) {
    for (i in names(isFct[isFct]))
      coeff$`  ` %<>% gsub(i, paste0(i,"|"), .) %>% gsub("\\|\\|","\\|",.)

    rm(i, isFct)
  }
  coeff$`  ` %<>% format(justify = "left")

	as.list(environment()) %>% add_class("conjoint")
}

#' Summary method for the conjoint function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/conjoint.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{conjoint}}
#' @param mc_diag Shows multicollinearity diagnostics.
#' @param additional Show additional regression results
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- conjoint("mp3", rvar = "Rating", evar = "Memory:Shape")
#' summary(result, mc_diag = TRUE)
#' mp3 %>% conjoint(rvar = "Rating", evar = "Memory:Shape") %>% summary(., mc_diag = TRUE)
#'
#' @seealso \code{\link{conjoint}} to generate results
#' @seealso \code{\link{plot.conjoint}} to plot results
#'
#' @importFrom car vif
#'
#' @export
summary.conjoint <- function(object,
                             mc_diag = FALSE,
                             additional = FALSE,
                             dec = 3,
                             ...) {

	cat("Conjoint analysis\n")
  cat("Data                 :", object$dataset, "\n")
	if (object$data_filter %>% gsub("\\s","",.) != "")
		cat("Filter               :", gsub("\\n","", object$data_filter), "\n")
	if (object$by != "none")
		cat("Show                 :", object$by, "==", object$show, "\n")
	rvar <- if (object$reverse) paste0(object$rvar, " (reversed)") else object$rvar
  cat("Response variable    :", rvar, "\n")
  cat("Explanatory variables:", paste0(object$evar, collapse=", "), "\n\n")

	object$the_table %>%
	{ cat("Conjoint part-worths:\n")
		print(formatdf(.$PW, dec), row.names = FALSE)
		cat("\nConjoint importance weights:\n")
		print(formatdf(.$IW, dec), row.names = FALSE)
	}

	cat("\nConjoint regression results:\n\n")

	coeff <- object$coeff
  if (!additional) {
	  coeff[,2] %<>% {sprintf(paste0("%.",dec,"f"),.)}
	  print(coeff[,1:2], row.names=FALSE)
		cat("\n")
	} else {
	  if (all(coeff$p.value == "NaN")) {
	    coeff[,2] %<>% {sprintf(paste0("%.",dec,"f"),.)}
	    print(coeff[,1:2], row.names=FALSE)
	    cat("\nInsufficient variation in explanatory variable(s) to report additional statistics")
	    return()
	  } else {
	    p.small <- coeff$p.value < .001
	    coeff[,2:5] %<>% formatdf(dec)
	    coeff$p.value[p.small] <- "< .001"
	    print(coeff, row.names=FALSE)
	  }

	  if (nrow(object$model$model) <= (length(object$evar) + 1))
	    return("\nInsufficient observations to estimate model")

	  ## adjusting df for included intercept term
	  df_int <- if (attr(object$model$terms, "intercept")) 1L else 0L

	  ## if stepwise returns only an intercept
	  if (nrow(coeff) == 1) return("\nModel contains only an intercept. No additional output shown")

	  reg_fit <- glance(object$model) %>% round(dec)
	  if (reg_fit['p.value'] < .001) reg_fit['p.value'] <- "< .001"
	  cat("\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")
	  cat("R-squared:", paste0(reg_fit$r.squared, ", "), "Adjusted R-squared:", reg_fit$adj.r.squared, "\n")
	  cat("F-statistic:", reg_fit$statistic, paste0("df(", reg_fit$df - df_int, ",", reg_fit$df.residual, "), p.value"), reg_fit$p.value)
	  cat("\nNr obs:", formatnr(reg_fit$df + reg_fit$df.residual, dec = 0), "\n\n")

	  if (anyNA(object$model$coeff))
	    cat("The set of explanatory variables exhibit perfect multicollinearity.\nOne or more variables were dropped from the estimation.\n")
	}

	if (mc_diag) {
    if (length(object$evar) > 1) {
      cat("Multicollinearity diagnostics:\n")
      car::vif(object$model) %>%
        { if (!dim(.) %>% is.null) .[,"GVIF"] else . } %>% # needed when factors are included
        data.frame("VIF" = ., "Rsq" = 1 - 1/.) %>%
        round(dec) %>%
        .[order(.$VIF, decreasing=T),] %>%
        { if (nrow(.) < 8) t(.) else . } %>% print
    } else {
      cat("Insufficient number of attributes selected to calculate\nmulticollinearity diagnostics")
    }
	}
}

#' Predict method for the conjoint function
#'
#' @details See \url{http://radiant-rstats.github.io/docs/model/conjoint.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{conjoint}}
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
#' @examples
#' result <- conjoint("mp3", rvar = "Rating", evar = "Memory:Shape")
#' predict(result, pred_data = "mp3")
#'
#' @importFrom radiant.model predict_model
#'
#' @export
predict.conjoint <- function(object,
                             pred_data = "",
                             pred_cmd = "",
                             conf_lev = 0.95,
                             se = FALSE,
                             dec = 3,
                             ...) {

 pfun <- function(model, pred, se, conf_lev) {

    pred_val <-
      try(sshhr(
        predict(model, pred, interval = ifelse (se, "prediction", "none"), level = conf_lev)),
        silent = TRUE
      )

    if (!is(pred_val, 'try-error')) {
      if (se) {
        pred_val %<>% data.frame %>% mutate(diff = .[,3] - .[,1])
        ci_perc <- ci_label(cl = conf_lev)
        colnames(pred_val) <- c("Prediction",ci_perc[1],ci_perc[2],"+/-")
      } else {
        pred_val %<>% data.frame %>% select(1)
        colnames(pred_val) <- "Prediction"
      }
    }

    pred_val
  }

  # radiant.model::predict_model(object, pfun, "conjoint.predict", pred_data, pred_cmd, conf_lev, se, dec)
  if (object$by == "none") {
    predict_model(object, pfun, "conjoint.predict", pred_data, pred_cmd, conf_lev, se, dec)
  } else {
    predict_conjoint_by(object, pfun, pred_data, pred_cmd, conf_lev, se, dec)
  }
}


# result <- conjoint("mp2", rvar = "Rating", evar = "Memory:Shape", by = "ID", show = "2")
# object <- result
# x <- predict(result, "mp3")
# str(x)
# x

#' Predict method for the conjoint function when a by variables is used
#'
#' @details See \url{http://radiant-rstats.github.io/docs/model/conjoint.html} for an example in Radiant
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
predict_conjoint_by <- function(object, pfun,
                                pred_data = "",
                                pred_cmd = "",
                                conf_lev = 0.95,
                                se = FALSE,
                                dec = 3,
                                ...) {

	# library(radiant.multivariate)
	# load("~/Desktop/mp2.rda")
	# c1 <- filter(mp2, ID == 1)
	# c2 <- filter(mp2, ID == 2)

	# res <- list()
	# res[[1]] <- c1
	# for (i in 2:10) {
	# 	tmp <- c1
	# 	tmp$Rating <- round(c1$Rating + rnorm(18, 0, 2), 0)
	# 	tmp$ID <- i
	# 	res[[i]] <- tmp
	# }

	# c2$ID <- 11
	# res[[11]] <- c2
	# for (i in 12:20) {
	# 	tmp <- c2
	# 	tmp$Rating <- round(c2$Rating + rnorm(18, 0, 2), 0)
	# 	tmp$ID <- i
	# 	res[[i]] <- tmp
	# }

	# mp3_segment <- bind_rows(res)
	# mp3_segment$ID <- as_integer(mp3_segment$ID)
	# mp3_segment$Rating <- as_integer(mp3_segment$Rating)
	# save(mp3_segment, file = "~/Desktop/mp3_segment.rda")

	# object <- conjoint(dataset = "mp2", rvar = "Rating", evar = c("Memory", "Radio", "Size", "Price", "Shape"), by = "ID", show = "2")
	# pred_data <- "mp3"
	# pred_cmd <- ""
 #  conf_lev = 0.95
 #  se = FALSE
 #  dec = 3
	# envir <- parent.frame()
	# se <- TRUE

  if (is.character(object)) return(object)

	vars <- c(object$rvar, object$evar, object$by)
	dataset <- if (object$dataset == "-----") object$store_data else object$dataset
	dat <- getdata(dataset, vars, filt = object$data_filter)

  levs <- dat[[object$by]] %>% as_factor %>% levels
  pred <- list()

	for (i in seq_along(levs)) {
		# i <- 1
		cdat <- filter_(dat, paste0(object$by, " == ", levs[i])) %>% select_(.dots = setdiff(colnames(dat), object$by))
    pred[[i]] <- conjoint(cdat, object$rvar, object$evar, reverse = object$reverse) %>%
    	predict_model(., pfun, "conjoint.predict", pred_data, pred_cmd, conf_lev, se, dec)

    ## when se is true reordering the columns removes attributes for some reason
    if (i == 1) att <- attributes(pred[[1]])

  	if (is.character(pred[[i]])) return(pred[[i]])
    pred[[i]]	%<>% {.[[object$by]] <- levs[i]; .} %>%
      {.[,c(object$by, head(colnames(.),-1))]}
  }

  pred <- bind_rows(pred)
  att$row.names <- 1:nrow(pred)
  att$vars <- att$names <- colnames(pred)
	if (is_string(object$dataset)) att$dataset <- object$dataset
  attributes(pred) <- att
  add_class(pred,"conjoint.predict.by")
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
print.conjoint.predict <- function(x, ..., n = 50) {
  # radiant.model::print_predict_model(x, ..., n = n, header = "Conjoint Analysis")
  print_predict_model(x, ..., n = n, header = "Conjoint Analysis")
}

#' Plot method for the conjoint function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/conjoint.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{conjoint}}
#' @param plots Show either the part-worth ("pw") or importance-weights ("iw") plot
#' @param scale_plot Scale the axes of the part-worth plots to the same range
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- conjoint(dataset = "mp3", rvar = "Rating", evar = "Memory:Shape")
#' plot(result, scale_plot = TRUE)
#' plot(result, plots = "iw")
#'
#' @seealso \code{\link{conjoint}} to generate results
#' @seealso \code{\link{summary.conjoint}} to summarize results
#'
#' @export
plot.conjoint <- function(x, plots = "pw",
                          scale_plot = FALSE,
                          shiny = FALSE,
                          ...) {

	object <- x; rm(x)

	the_table <- object$the_table
	plot_ylim <- the_table$plot_ylim
	plot_list <- list()

	if ("pw" %in% plots) {
		PW.df <- the_table[["PW"]]

		for (var in object$evar) {
			PW.var <- PW.df[PW.df[["Attributes"]] == var,]

			# setting the levels in the same order as in the_table. Without this
			# ggplot would change the ordering of the price levels
			PW.var$Levels <- factor(PW.var$Levels,levels=PW.var$Levels,ordered=FALSE)

			p <- ggplot(PW.var, aes_string(x="Levels", y="PW", group = 1)) +
				  geom_line(colour="blue", linetype = 'dotdash', size=.7) +
	  		  geom_point(colour="blue", size=4, shape=21, fill="white") +
		  	  labs(list(title = paste("Part-worths for", var), x = ""))
		  	  # theme(axis.text.x = element_text(angle = 45, hjust = 1))

		  if (scale_plot) p <- p + ylim(plot_ylim[var,"Min"],plot_ylim[var,"Max"])
			plot_list[[var]] <- p
		}
	}

	if ("iw" %in% plots) {
		IW.df <- the_table[['IW']]
		plot_list[["iw"]] <- ggplot(IW.df, aes_string(x="Attributes", y="IW", fill = "Attributes")) +
		                   geom_bar(stat = "identity", alpha = .5) +
		                   theme(legend.position = "none") +
		                   labs(list(title = "Importance weights"))
	}

	sshhr( do.call(gridExtra::arrangeGrob, c(plot_list, list(ncol = min(length(plot_list),2)))) ) %>%
	 	{ if (shiny) . else print(.) }
}

#' Function to calculate the PW and IW table for conjoint
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/conjoint.html} for an example in Radiant
#'
#' @param model Tidied model results (broom) output from \code{\link{conjoint}} passed on by summary.conjoint
#' @param dat Conjoint data
#' @param evar Explanatory variables used in the conjoint regression
#'
#' @examples
#' result <- conjoint(dataset = "mp3", rvar = "Rating", evar = "Memory:Shape")
#' the_table(tidy(result$model), result$dat, result$evar)
#'
#' @seealso \code{\link{conjoint}} to generate results
#' @seealso \code{\link{summary.conjoint}} to summarize results
#' @seealso \code{\link{plot.conjoint}} to plot results
#'
#' @export
the_table <- function(model, dat, evar) {
	if (is.character(model)) return(list("PW" = "No attributes selected."))

	attr <- select_(dat, .dots = evar)
	isFct <- sapply(attr, is.factor)
	if (sum(isFct) < ncol(attr)) return(list("PW" = "Only factors can be used.", "IW" = "Only factors can be used."))
	levs <- lapply(attr[,isFct, drop = FALSE],levels)
	vars <- colnames(attr)[isFct]

	nlevs <- sapply(levs,length)
	PW.df <- data.frame(rep(vars,nlevs), unlist(levs))
	colnames(PW.df) <- c("Attributes","Levels")
	PW.df$PW <- 0

	## Calculate PW and IW's when interactions are present
	## http://www.slideshare.net/SunnyBose/conjoint-analysis-12090511
	rownames(PW.df) <- paste(PW.df[['Attributes']], PW.df[['Levels']], sep = "")

	coeff <- model$estimate
	PW.df[model$term[-1],'PW'] <- coeff[-1]

	minPW <- PW.df[tapply(1:nrow(PW.df),PW.df$Attributes,function(i) i[which.min(PW.df$PW[i])]),]
	maxPW <- PW.df[tapply(1:nrow(PW.df),PW.df$Attributes,function(i) i[which.max(PW.df$PW[i])]),]
	rownames(minPW) <- minPW$Attributes
	rownames(maxPW) <- maxPW$Attributes

	rangePW <- data.frame(cbind(maxPW[vars,'PW'],minPW[vars,'PW']))
	rangePW$Range <- rangePW[[1]] - rangePW[[2]]
	colnames(rangePW) <- c("Max","Min","Range")
	rownames(rangePW) <- vars

	## for plot range if standardized
	maxlim <- rangePW[['Max']] > abs(rangePW[['Min']])
	maxrange <- max(rangePW[['Range']])
	plot_ylim <- rangePW[c('Min','Max')]

	plot_ylim[maxlim,'Max'] <- plot_ylim[maxlim,'Max'] + maxrange - rangePW$Range[maxlim]
	plot_ylim[!maxlim,'Min'] <- plot_ylim[!maxlim,'Min'] - (maxrange - rangePW$Range[!maxlim])
	plot_ylim <- plot_ylim * 1.01 		## expanded max to avoid hiding max points in plot

	IW <- data.frame(vars)
	IW$IW <- rangePW$Range / sum(rangePW$Range)
	colnames(IW) <- c("Attributes","IW")

	PW.df[['Attributes']] <- as.character(PW.df[['Attributes']])
	PW.df[['Levels']] <- as.character(PW.df[['Levels']])
	PW.df <- rbind(PW.df, c("Base utility","~",coeff[1]))
	PW.df[['PW']] <- as.numeric(PW.df[['PW']])

	PW.df[['PW']] <- round(PW.df[['PW']],3)
	IW[['IW']] <- round(IW[['IW']],3)

	list('PW' = PW.df, 'IW' = IW, 'plot_ylim' = plot_ylim)
}

#' Store method for the Multivariate > Conjoint tab
#'
#' @details Store data frame with PWs or IWs in Radiant r_data list if available
#'
#' @param object Return value from conjoint
#' @param name Name of the dataset to store
#' @param type Type of output to store
#' @param envir Environment to assign 'new' dataset (optional). Used when an r_data list is not available
#' @param ... further arguments passed to or from other methods
#'
#' @importFrom pryr where
#'
#' @export
store.conjoint <- function(object, name = "PWs", type = "PW", envir = parent.frame(), ...) {

	# library(radiant.multivariate)
	# load("~/Desktop/mp2.rda")
	# object <- conjoint(dataset = "mp2", rvar = "Rating", evar = c("Memory", "Radio", "Size", "Price", "Shape"), by = "ID", show = "2")
	# type <- "PW"
	# name <- "PWs"
	# envir <- parent.frame()

	vars <- c(object$rvar, object$evar, object$by)
	dataset <- if (object$dataset == "-----") object$store_data else object$dataset
	dat <- getdata(dataset, vars, filt = object$data_filter)

  levs <- dat[[object$by]] %>% as_factor %>% levels

  if (type == "PW") {
    cn <- tidy(object$model)$term[-1]

    for (i in object$evar)
    	cn %<>% gsub(i, paste0(i, "_"), .) %>% gsub("\\_\\_","\\_",.)

    cn <- gsub("[^A-z0-9_\\.]", "", cn)
  } else {
    cn <- object$the_table$IW$Attribute %>%
      gsub("[^A-z0-9_\\.]", "", .)
  }

  res <- matrix(NA, nrow = length(levs), ncol = length(cn) + 1)
  colnames(res) <- c(object$by, cn)
  res <- as.data.frame(res)
  res[[object$by]] <- levs

	for (i in seq_along(levs)) {
		cdat <- filter_(dat, paste0(object$by, " == ", levs[i])) %>% select_(.dots = setdiff(colnames(dat), object$by))
    cres <- conjoint(cdat, object$rvar, object$evar, reverse = object$reverse)
    cres
		if (type == "IW") {
      res[i, 2:ncol(res)] <- cres$the_table[[type]][[type]]
		} else {
      res[i, 2:ncol(res)] <- cres$coef$coefficient[-1]
		}
	}

  if (exists("r_environment")) {
    env <- r_environment
  } else if (exists("r_data")) {
    env <- pryr::where("r_data")
  } else {
    assign(name, res, envir = envir)
    message("Dataset ", name, " created in ", environmentName(envir), " environment")
    return(invisible())
  }

  # ## use data description from the original if available
  if (is_empty(env$r_data[[paste0(name, "_descr")]])) {
  	s <- ifelse(type == "PW", "PWs", "IWs")
    attr(res, "description") <- paste0("## Conjoint ", s, "\n\nThis dataset contains ", s, " derived from dataset ", object$dataset)
  } else {
    attr(res, "description") <- env$r_data[[paste0(name, "_descr")]]
  }

  env$r_data[[name]] <- res
  env$r_data[[paste0(name,"_descr")]] <- attr(res, "description")
  env$r_data[["datasetlist"]] <- c(name, env$r_data[["datasetlist"]]) %>% unique
}

#' Store predicted values generated in predict.conjoint
#'
#' @details See \url{http://radiant-rstats.github.io/docs/multivariate/conjoint.html} for an example in Radiant
#'
#' @param object Return value from model predict function
#' @param ... Additional arguments
#' @param data Data or dataset name (e.g., data = mtcars or data = "mtcars")
#' @param name Variable name(s) assigned to predicted values
#'
#' @export
store.conjoint.predict <- function(object, ..., data = attr(object,"pred_data"), name = "prediction") {
  if (is_empty(name)) name <- "prediction"

  ## gsub needed because trailing/leading spaces may be added to the variable name
  ind <- which(colnames(object) == "Prediction")

  ## if se was calculated
  name <- unlist(strsplit(name, ",")) %>% gsub("\\s","",.)
  if (length(name) > 1) {
    name <- name[1:min(3, length(name))]
    ind_mult <- ind:(ind + length(name[-1]))
    df <- object[,ind_mult, drop = FALSE]
  } else {
    df <- object[,"Prediction", drop = FALSE]
  }

  vars <- colnames(object)[1:(ind-1)]
  indr <- indexr(data, vars, "")
  pred <- as_data_frame(matrix(NA, nrow = indr$nr, ncol = ncol(df)))
  pred[indr$ind, ] <- as.vector(df) ## as.vector removes all attributes from df

  changedata(data, vars = pred, var_names = name)
}

#' Store method for the Multivariate > Conjoint > Predict
#'
#' @details Store data frame with predictions in Radiant r_data list if available
#'
#' @param object Return value from predict.conjoint
#' @param name Name of the dataset to store
#' @param envir Environment to assign 'new' dataset (optional). Used when an r_data list is not available
#' @param ... further arguments passed to or from other methods
#'
#' @importFrom pryr where
#'
#' @export
store.conjoint.predict.by <- function(object, name = "predict_by", envir = parent.frame(), ...) {

  if (exists("r_environment")) {
    env <- r_environment
  } else if (exists("r_data")) {
    env <- pryr::where("r_data")
  } else {
    assign(name, object, envir = envir)
    message("Dataset ", name, " created in ", environmentName(envir), " environment")
    return(invisible())
  }

  # ## use data description from the original if available
  if (is_empty(env$r_data[[paste0(name, "_descr")]])) {
  	if(attr(object, "pred_type") == "data") {
  		s <- paste("The prediction dataset used was", attr(object, "pred_data"))
  	} else if(attr(object, "pred_type") == "cmd") {
      pred_cmd <- gsub("([\\=\\+\\*-])", " \\1 ", attr(object, "pred_cmd")) %>% gsub("([;,])", "\\1 ", .)
  		s <- paste("The prediction command used was ", pred_cmd)
  	} else {
      pred_cmd <- gsub("([\\=\\+\\*-])", " \\1 ", attr(object, "pred_cmd")) %>% gsub("([;,])", "\\1 ", .)
  		s <- paste("The prediction dataset used was ", attr(object, "pred_data"), " and the dataset was adapted using the following prediction command:", pred_cmd)
  	}

    attr(object, "description") <- paste0("## Conjoint predictions\n\nThis dataset contains predictions from a conjoint analysis on ", attr(object,"dataset"), ". ", s, ". Note that a separate regression model was estimated for each level of ", colnames(object)[1], ".")
  } else {
    attr(object, "description") <- env$r_data[[paste0(name, "_descr")]]
  }

  env$r_data[[name]] <- object
  env$r_data[[paste0(name,"_descr")]] <- attr(object, "description")
  env$r_data[["datasetlist"]] <- c(name, env$r_data[["datasetlist"]]) %>% unique
}



## code for 'exploded logistic regression' when ranking data is provided
# library(MASS)
# library(dplyr)
# library(magrittr)
#
# load("~/gh/radiant/inst/base/data/carpet.rda")
# # carpet %<>% {bind_rows(list(.,.,.))}
# carpet$rev_ranking <- ordered(as.factor(19 - carpet$ranking))
# carpet$price <- factor(carpet$price, levels = c("1.59","1.39","1.19"))
# carpet$brand <- factor(carpet$brand, levels = c("Glory","K2R","Bissell"))
#
# m <- polr(rev_ranking ~ design + brand + price + seal + money_back, data = carpet, Hess = FALSE)
# # coef(m) %>% {. / max(abs(.))}
# coef(m) %>% {. / abs(.["designB"])}
#
# carpet$ranking_lm <- 19 - carpet$ranking
# l <- lm(ranking_lm ~ design + brand + price + seal + money_back, data = carpet)
# # coef(l) %>% {./ max(abs(.))}
# coef(l) %>% {. / abs(.["designB"])} %>% .[-1]
