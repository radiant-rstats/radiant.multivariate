## urls for menu
r_url_list <- getOption("radiant.url.list")
r_url_list[["(Dis)similarity"]] <-
  list("tabs_mds" = list("Summary" = "multivariate/mds/", "Plot" = "multivariate/mds/plot/"))
r_url_list[["Attributes"]] <-
  list("tabs_pmap" = list("Summary" = "multivariate/pmap/", "Plot" = "multivariate/pmap/plot/"))
r_url_list[["Hierarchical"]] <-
  list("tabs_hier_clus" = list("Summary" = "multivariate/hier_clus/", "Plot" = "multivariate/hier_clus/plot/"))
r_url_list[["K-means"]] <-
  list("tabs_kmeans_clus" = list("Summary" = "multivariate/kmeans/", "Plot" = "multivariate/kmeans/plot/"))
r_url_list[["Conjoint"]] <-
  list("tabs_conjoint" = list("Summary" = "multivariate/conjoint/", "Plot" = "multivariate/plot/"))
options(radiant.url.list = r_url_list); rm(r_url_list)

## design menu
options(radiant.multivariate_ui =
	tagList(
	  navbarMenu("Multivariate",
	    "Maps",
	    tabPanel("(Dis)similarity", uiOutput("mds")),
      tabPanel("Attributes", uiOutput("pmap")),
	    "----", "Factor",
      tabPanel("Pre-factor", uiOutput("pre_factor")),
      tabPanel("Factor", uiOutput("full_factor")),
	    "----", "Cluster",
    	tabPanel("Hierarchical", uiOutput("hier_clus")),
    	tabPanel("K-means", uiOutput("kmeans_clus")),
	    "----", "Conjoint",
	    tabPanel("Conjoint", uiOutput("conjoint"))
    )
  )
)
