#' K-means cluster analysis
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/kmeans_clus.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param vars Vector of variables to include in the analysis
#' @param hc_init Use centers from hier_clus as the starting point
#' @param distance Distance for hier_clus
#' @param method Method for hier_clus
#' @param seed Random see to use for kmeans if hc_init is FALSE
#' @param nr_clus Number of clusters to extract
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#'
#' @return A list of all variables used in kmeans_clus as an object of class kmeans_clus
#'
#' @examples
#' result <- kmeans_clus("shopping", c("v1:v6"))
#'
#' @seealso \code{\link{summary.kmeans_clus}} to summarize results
#' @seealso \code{\link{plot.kmeans_clus}} to plot results
#' @seealso \code{\link{store.kmeans_clus}} to add cluster membership to the selected dataset
#'
#' @export
kmeans_clus <- function(dataset, vars,
                        hc_init = TRUE,
                        distance = "sq.euclidian",
                        method = "ward.D",
                        seed = 1234,
                        nr_clus = 2,
                        data_filter = "") {

	dat <- getdata(dataset, vars, filt = data_filter)
	if (!is_string(dataset)) dataset <- "-----"

	if (hc_init) {
		init <- hier_clus(dat, vars, distance = distance, method = method,
		                  max_cases = Inf)

		clus_var <- cutree(init$hc_out, k = nr_clus)
		hc_cent <- c()
		# getdata(dataset, vars, filt = data_filter) %>%
		km_out <- dat %>%
			mutate(clus_var = clus_var) %>%
			mutate_each(funs(scale)) %T>%
			{ hc_cent <<-
			   group_by(., clus_var) %>%
				 summarise_each(funs(mean)) %>%
				 select(-clus_var) %>%
				 as.matrix
			} %>% select(-clus_var) %>%
		  kmeans(centers = hc_cent, iter.max = 500)
		rm(init, hc_cent)
	} else {
		set.seed(seed)
		# getdata(dataset, vars, filt = data_filter) %>%
		km_out <- dat %>%
			mutate_each(funs(scale)) %>%
			kmeans(centers = nr_clus, nstart = 10, iter.max = 500)
	}

	clus_names <- paste("Cluster",1:nr_clus)
	clus_means <- dat %>%
		mutate(clus_var = km_out$cluster) %>%
		group_by(clus_var) %>%
		summarise_each(funs(mean)) %>%
		select(-clus_var) %>%
		as.data.frame %>%
		set_rownames(clus_names)

	nr_obs <- length(km_out$cluster)

	as.list(environment()) %>% add_class("kmeans_clus")
}

#' Summary method for kmeans_clus
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/kmeans_clus.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{kmeans_clus}}
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- kmeans_clus("shopping", vars = c("v1:v6"))
#' summary(result)
#' shopping %>% kmeans_clus(vars = c("v1:v6"), nr_clus = 3) %>% summary
#'
#' @seealso \code{\link{kmeans_clus}} to generate results
#' @seealso \code{\link{plot.kmeans_clus}} to plot results
#' @seealso \code{\link{store.kmeans_clus}} to add cluster membership to the selected dataset
#'
#' @export
summary.kmeans_clus <- function(object, dec = 2, ...) {

	cat("K-means cluster analysis\n")
	cat("Data        :", object$dataset, "\n")
	if (object$data_filter %>% gsub("\\s","",.) != "")
		cat("Filter      :", gsub("\\n","", object$data_filter), "\n")
	cat("Variables   :", paste0(object$vars, collapse=", "), "\n")
	if (object$hc_init) {
		cat("Method      :", object$method, "\n")
		cat("Distance    :", object$distance, "\n")
	}
	cat("Observations:", object$nr_obs, "\n")
	cat("Generated   :", object$nr_clus, "clusters of sizes", paste0(object$km_out$size, collapse=", "),"\n\n")

	cat("Cluster means:\n")
	cm <- object$clus_means
	cm <- cbind(data.frame(" " = paste0("Cluster ", 1:nrow(cm)), check.names = FALSE), cm)
	print(formatdf(cm, dec = dec), row.names = FALSE)

	## percentage of within cluster variance accounted for by each cluster
	cat("\nPercentage of within cluster variance accounted for by each cluster:\n")
	data.frame(wcv = object$km_out$withinss / object$km_out$tot.withinss) %>%
		formatdf(perc = TRUE, dec = dec) %>%
		set_rownames(object$clus_names) %>%
		set_colnames("") %>%
		print

	## percentage of between cluster variance versus the total higher is better
	formatnr(object$km_out$betweenss / object$km_out$totss, perc = TRUE, dec = dec) %>%
		paste0("\nBetween cluster variance accounts for ", . , " of the\ntotal variance in the data (higher is better).") %>%
		cat
}

#' Plot method for kmeans_clus
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/kmeans_clus.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{kmeans_clus}}
#' @param plots One of "density", "bar", or "scatter")
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- kmeans_clus("shopping", vars = c("v1:v6"))
#' plot(result)
#' shopping %>% kmeans_clus(, vars = c("v1:v6")) %>% plot
#'
#' @seealso \code{\link{kmeans_clus}} to generate results
#' @seealso \code{\link{summary.kmeans_clus}} to summarize results
#' @seealso \code{\link{store.kmeans_clus}} to add cluster membership to the selected dataset
#'
#' @export
plot.kmeans_clus <- function(x,
                             plots = "density",
                             shiny = FALSE,
                             ...) {

	x$dat <- mutate(x$dat, Cluster = as.factor(x$km_out$cluster))
	vars <- colnames(x$dat) %>% .[-length(.)]

	plot_list <- list()

	if ("density" %in% plots) {
		for (var in vars) {
			plot_list[[var]] <- ggplot(x$dat, aes_string(x=var, fill = "Cluster")) +
					geom_density(adjust=2.5, alpha=.3) +
					labs(y = "") + theme(axis.text.y = element_blank())
		}
	} else if ("bar" %in% plots) {

	  ci_calc <- function(se, n, conf.lev = .95)
	 	  se * qt(conf.lev/2 + .5, n - 1)

		for (var in vars) {

  	  dat_summary <-
		    select_(x$dat, .dots = c(var, "Cluster"))  %>%
		    group_by_("Cluster") %>%
		    summarise_each(funs(mean, n = length(.), sd,
		                        se = sd/sqrt(n),
		                        ci = ci_calc(se,n,.95)))

		  plot_list[[var]] <-
		    ggplot(dat_summary,
		           aes_string(x = "Cluster", y = "mean", fill = "Cluster")) +
		    geom_bar(stat = "identity")  +
		    geom_errorbar(width = .1, aes(ymin = mean - ci, ymax = mean + ci)) +
		    geom_errorbar(width = .05, aes(ymin = mean - se, ymax = mean + se), colour = "blue") +
		    theme(legend.position = "none") +
		    ylab(paste0(var, " (mean)"))
		 }

	} else {
		 plot_list <-
		   visualize(x$dat, xvar = "Cluster", yvar = vars, type = "scatter",
		             check = "jitter", custom = TRUE)
	}

	sshhr( do.call(gridExtra::arrangeGrob, c(plot_list, list(ncol = min(length(plot_list),2)))) ) %>%
	 	{ if (shiny) . else print(.) }

}

#' Add a cluster membership variable to the active dataset
#'
#' @details See \url{https://radiant-rstats.github.io/docs/multivariate/kmeans_clus.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{kmeans_clus}}
#' @param ... Additional arguments
#' @param name Name of cluster membership variable
#'
#' @examples
#' kmeans_clus(shopping, vars = c("v1:v6")) %>% store %>% head
#'
#' @seealso \code{\link{kmeans_clus}} to generate results
#' @seealso \code{\link{summary.kmeans_clus}} to summarize results
#' @seealso \code{\link{plot.kmeans_clus}} to plot results
#'
#' @export
store.kmeans_clus <- function(object, ..., name = "") {
	## membership variable name
  if (is_empty(name)) name <- paste0("kclus",object$nr_clus)
  dat <- {if (object$dataset == "-----") object$dat else object$dataset}
	indr <- indexr(dat, object$vars, object$data_filter)
	km <- rep(NA,indr$nr)
	km[indr$ind] <- object$km_out$cluster
	changedata(dat, vars = as.factor(km), var_names = name)
}
