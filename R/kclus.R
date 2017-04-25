#' K-clustering
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/kclus.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
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
#' result <- kclus("shopping", c("v1:v6"))
#'
#' @seealso \code{\link{summary.kclus}} to summarize results
#' @seealso \code{\link{plot.kclus}} to plot results
#' @seealso \code{\link{store.kclus}} to add cluster membership to the selected dataset
#'
#' @importFrom Gmedian Gmedian kGmedian
#'
#' @export
kclus <- function(dataset, vars,
                  fun = "mean",
                  hc_init = TRUE,
                  distance = "sq.euclidian",
                  method = "ward.D",
                  seed = 1234,
                  nr_clus = 2,
                  data_filter = "") {

	dat <- getdata(dataset, vars, filt = data_filter)
	if (!is_string(dataset)) dataset <- deparse(substitute(dataset)) %>% set_attr("df", TRUE)

	if (fun == "median" && length(vars) < 2)
		stop("K-medians requires at least two variables as input")

	# converting factors to integer with first level = 1
	convert <- function(x) {
    as.integer(x == levels(x)[1])
	}
	dat <- mutate_if_tmp(dat, is.factor, convert)

	if (hc_init) {
		init <- hclus(dat, vars, distance = distance, method = method, max_cases = Inf)

		clus_var <- cutree(init$hc_out, k = nr_clus)
		hc_cent <- c()
		km_out <- dat %>%
			mutate(clus_var = clus_var) %>%
			mutate_all(funs(as.vector(scale(.)))) %T>%
			{ hc_cent <<-
			   group_by(., clus_var) %>%
				 summarise_all(funs(mean)) %>%
				 select(-clus_var) %>%
				 as.matrix
			} %>% select(-clus_var) %>%
			{ if (fun == "median") {
  		    km_cent <- kmeans(., centers = hc_cent, algorithm = "MacQueen", iter.max = 500)$centers
  		    Gmedian::kGmedian(., ncenters = km_cent)
  		  } else {
  		    kmeans(., centers = hc_cent, iter.max = 500)
  		  }
		  }
		rm(init, hc_cent)
	} else {
    seed %>% gsub("[^0-9]","",.) %>% { if (!is_empty(.)) set.seed(seed) }
		km_out <- dat %>%
			mutate_all(funs(as.vector(scale(.)))) %>%
			{ if (fun == "median") {
  		    km_cent <- kmeans(., centers = nr_clus, algorithm = "MacQueen", iter.max = 500)$centers
  		    Gmedian::kGmedian(., ncenters = km_cent)
  		  } else {
			    kmeans(., centers = nr_clus, nstart = 10, iter.max = 500)
  		  }
		  }
	}

	## same calculations of SST etc. as for kmeans (verified)
	if (fun == "median") {
		sdat <- mutate_all(dat, funs(as.vector(scale(.))))
	  km_out$withinss <-
			mutate(sdat, clus_var = km_out$cluster) %>%
			group_by(clus_var) %>%
			# summarise_each(funs(sum((. - as.vector(Gmedian::Gmedian(.)))^2))) %>%
			summarise_all(funs(sum((. - mean(.))^2))) %>%
			select(-clus_var) %>%
			rowSums
	  km_out$tot.withinss <- sum(km_out$withinss)
	  km_out$totss <-
			# summarise_each(sdat, funs(sum((. - as.vector(Gmedian::Gmedian(.)))^2))) %>%
			summarise_all(sdat, funs(sum((. - mean(.))^2))) %>%
			sum(.)
	  km_out$betweenss <- km_out$totss - km_out$tot.withinss
	  rm(sdat)
	}

	## Gmedian / tibble / dplyr issue
	clus_names <- paste("Cluster",1:nr_clus)
	clus_means <- dat %>%
		mutate(clus_var = km_out$cluster) %>%
		group_by(clus_var) %>%
		## need as.vector - see https://github.com/hadley/tibble/issues/175
		# summarise_each(funs(if (fun == "median") as.vector(Gmedian::Gmedian(.)) else mean(.))) %>%
		summarise_all(funs(mean(.))) %>%
		select(-clus_var) %>%
		as.data.frame %>%
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
#' result <- kclus("shopping", vars = c("v1:v6"))
#' summary(result)
#' shopping %>% kclus(vars = c("v1:v6"), nr_clus = 3) %>% summary
#'
#' @seealso \code{\link{kclus}} to generate results
#' @seealso \code{\link{plot.kclus}} to plot results
#' @seealso \code{\link{store.kclus}} to add cluster membership to the selected dataset
#'
#' @export
summary.kclus <- function(object, dec = 2, ...) {

	cat(paste0("K-", object$fun, "s cluster analysis\n"))
	cat("Data         :", object$dataset, "\n")
	if (object$data_filter %>% gsub("\\s","",.) != "")
		cat("Filter       :", gsub("\\n","", object$data_filter), "\n")
	cat("Variables    :", paste0(object$vars, collapse=", "), "\n")
	cat("Clustering by:", paste0("K-",object$fun, "s\n"))
	if (object$hc_init) {
		cat("HC method    :", object$method, "\n")
		cat("HC distance  :", object$distance, "\n")
	}
	cat("Observations :", object$nr_obs, "\n")
	cat("Generated    :", object$nr_clus, "clusters of sizes", paste0(object$km_out$size, collapse=", "),"\n\n")

	# cat(paste0("Cluster ", object$fun,"s:\n"))
	cat(paste0("Cluster means:\n"))
	cm <- object$clus_means
	cm <- cbind(data.frame(" " = paste0("Cluster ", 1:nrow(cm)), check.names = FALSE), cm)
	print(formatdf(cm, dec = dec), row.names = FALSE)

	## percentage of within cluster heterogeneity accounted for by each cluster
	cat("\nPercentage of within cluster heterogeneity accounted for by each cluster:\n")
	data.frame(wcv = object$km_out$withinss / object$km_out$tot.withinss) %>%
		formatdf(perc = TRUE, dec = dec) %>%
		set_rownames(object$clus_names) %>%
		set_colnames("") %>%
		print

	## percentage of between cluster heterogeneity versus the total, higher is better
	formatnr(object$km_out$betweenss / object$km_out$totss, perc = TRUE, dec = dec) %>%
		paste0("\nBetween cluster heterogeneity accounts for ", . , " of the\ntotal heterogeneity in the data (higher is better).") %>%
		cat
}

#' Plot method for kclus
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/kclus.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{kclus}}
#' @param plots One of "density", "bar", or "scatter")
#' @param shiny Did the function call originate inside a shiny app
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This opion can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{http://docs.ggplot2.org/} for options.
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- kclus("shopping", vars = c("v1:v6"))
#' plot(result)
#' shopping %>% kclus(, vars = c("v1:v6")) %>% plot
#'
#' @seealso \code{\link{kclus}} to generate results
#' @seealso \code{\link{summary.kclus}} to summarize results
#' @seealso \code{\link{store.kclus}} to add cluster membership to the selected dataset
#'
#' @export
plot.kclus <- function(x, plots = "density",
                       shiny = FALSE, custom = FALSE, ...) {

	x$dat <- mutate(x$dat, Cluster = as.factor(x$km_out$cluster))
	vars <- colnames(x$dat) %>% .[-length(.)]

	## what to report?
	# fun <- if (x$fun == "mean") mean else median
	fun <- mean
	x$fun <- "mean"

	plot_list <- list()

	if ("density" %in% plots) {
		for (var in vars) {
			plot_list[[paste0("dens_", var)]] <- ggplot(x$dat, aes_string(x=var, fill = "Cluster")) +
					geom_density(adjust=2.5, alpha=.3) +
					labs(y = "") + theme(axis.text.y = element_blank())
		}
	} 
	if ("bar" %in% plots) {

	  ci_calc <- function(se, n, conf.lev = .95)
	 	  se * qt(conf.lev/2 + .5, n - 1)

		for (var in vars) {

  	  dat_summary <-
		    select_(x$dat, .dots = c(var, "Cluster"))  %>%
		    group_by_("Cluster") %>%
		    summarise_all(funs(cent = fun(.), n = length(.), sd,
		                        se = sd/sqrt(n),
		                        ci = ci_calc(se,n,.95)))

		  plot_list[[paste0("bar_", var)]] <-
		    ggplot(dat_summary,
		           aes_string(x = "Cluster", y = "cent", fill = "Cluster")) +
		    geom_bar(stat = "identity")  +
		    geom_errorbar(width = .1, aes(ymin = cent - ci, ymax = cent + ci)) +
		    geom_errorbar(width = .05, aes(ymin = cent - se, ymax = cent + se), colour = "blue") +
		    theme(legend.position = "none") +
		    labs(y = paste0(var, " (", x$fun, ")"))
		 }
	} 
	if ("scatter" %in% plots) {
		for (var in vars) {
		  plot_list[[paste0("scatter_", var)]] <-
		     visualize(x$dat, xvar = "Cluster", yvar = var, type = "scatter",
		               check = "jitter", custom = TRUE)
		}
	}

  if (custom)
    if (length(plot_list) == 1) return(plot_list[[1]]) else return(plot_list)

	sshhr(gridExtra::grid.arrange(grobs = plot_list, ncol = min(length(plot_list),2))) %>%
	 	{if (shiny) . else print(.)}
}

#' Add a cluster membership variable to the active dataset
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/kclus.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{kclus}}
#' @param ... Additional arguments
#' @param name Name of cluster membership variable
#'
#' @examples
#' kclus(shopping, vars = "v1:v6") %>% store %>% head
#'
#' @seealso \code{\link{kclus}} to generate results
#' @seealso \code{\link{summary.kclus}} to summarize results
#' @seealso \code{\link{plot.kclus}} to plot results
#'
#' @export
store.kclus <- function(object, ..., name = "") {
	## membership variable name
  if (is_empty(name)) name <- paste0("kclus",object$nr_clus)
  # dat <- {if (object$dataset == "-----") object$dat else object$dataset}
  dat <- if (length(attr(object$dataset, "df") > 0)) object$dat else object$dataset
	indr <- indexr(dat, object$vars, object$data_filter)
	km <- rep(NA,indr$nr)
	km[indr$ind] <- object$km_out$cluster
	changedata(dat, vars = as.factor(km), var_names = name)
}
