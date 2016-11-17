trim <- function(x) gsub("^\\s+|\\s+$", "", x)

######### tests ########

# library(radiant.multivariate)
# library(testthat)

context("Maps")

test_that("City MDS points", {
  result <- mds("city", "from", "to", "distance")
	# str(result)
  res1 <- result$res$points
  # dput(result$res$points)
  res2 <- structure(c(-1348.66832957982, -1198.87410814714, -1076.98554040122,
-1226.93901099845, -428.454832718783, 1596.1594018405, 1697.22828135996,
1464.04701004452, 522.48712860043, -462.400598146569, -306.546900234988,
-136.432035420421, 1013.62838366558, -174.603164807742, -639.307768963489,
131.685862779591, 560.580459896188, 13.3957612318459), .Dim = c(9L,
2L), .Dimnames = list(c("Boston", "NY", "DC", "Miami", "Chicago",
"Seattle", "SF", "LA", "Denver"), NULL))
  expect_equal(res1,res2)
})

test_that("Computer perceptual map", {
	result <- pmap("computer","brand","high_end:business")
	# str(result)
	res1 <- result$fres$scores
	# dput(result$res$points)
	res2 <- structure(c(1.2990975042645, -0.318156927318684, -1.18661978839803,
	-0.522421680770708, 0.728100892222923, 0.0936804393886441, -0.208948184854464,
	-0.934302935231416, 1.64813821225715, -0.598567531559918), .Dim = c(5L,
	2L), .Dimnames = list(c("Apple", "Dell", "Gateway", "HP", "Sony"
	), c("RC1", "RC2")))
  expect_equal(res1,res2)
})

context("Factor/PCA analysis")

test_that("Pre nalysis for diamonds", {
	result <- pre_factor("diamonds",c("price","carat","table"))
	# str(result)
	res1 <- result$pre_r2
	# dput(result$pre_r2)
	res2 <- structure(list(Rsq = c(0.861258211951766, 0.86356619173057, 0.0450708598611924
)), .Names = "Rsq", row.names = c("price", "carat", "table"), class = "data.frame")
  expect_equal(res1,res2)
})

test_that("Factor/PCA analysis for diamonds", {
	result <- full_factor("diamonds",c("price","carat","table"))
	# str(result)
	res1 <- result$floadings
	# dput(result$floadings)
	res2 <- structure(list(PC1 = c(0.964483176117948, 0.972902482025944,
0.325710945731448)), .Names = "PC1", row.names = c("price", "carat",
"table"), class = "data.frame")
  expect_equal(res1,res2)
})

context("Cluster analysis")

test_that("Hierarchical cluster analysis", {
	result <- hclus("shopping", vars = "v1:v6")
	# str(result)
	res1 <- result$hc_out$height
	# dput(result$hc_out$height)
	res2 <- c(0.693447070258665, 0.77981545158788, 1.19609257290417, 1.20263048421394,
1.20263048421394, 1.25874249684769, 1.59728591646143, 1.76984887051771,
1.88396035104441, 2.06113619040031, 3.37654118004185, 3.5167211043475,
3.77286952167201, 5.26961961999936, 7.6948927428698, 9.4541210015406,
12.7002828285666, 76.1882734993453, 92.3810886131668)
  expect_equal(res1,res2)
})

test_that("K-clustering", {
	result <- kclus("shopping", vars = "v1:v6")
	# str(result)
	res1 <- result$clus_means
	# dput(result$clus_means)
	res2 <- structure(list(v1 = c(5.75, 2.58333333333333), v2 = c(3.625,
4.41666666666667), v3 = c(6, 2.58333333333333), v4 = c(3.125,
4.75), v5 = c(1.875, 4.5), v6 = c(3.875, 4.66666666666667)), class = "data.frame", row.names = c("Cluster 1",
"Cluster 2"), .Names = c("v1", "v2", "v3", "v4", "v5", "v6"))
  expect_equal(res1,res2)
})

context("Conjoint analysis")

test_that("Conjoint on mp3 data", {
  result <- conjoint("mp3", rvar = "Rating", evar = "Memory:Shape")
	# str(result)
	res1 <- result$model_list[[1]]$tab
	res2 <- structure(list(PW = structure(list(Attributes = c("Memory", "Memory",
"Memory", "Radio", "Radio", "Size", "Size", "Size", "Price",
"Price", "Price", "Shape", "Shape", "Shape", "Base utility"),
    Levels = c("4GB", "6GB", "8GB", "No", "Yes", "Large", "Medium",
    "Small", "$50", "$100", "$150", "Circular", "Rectangular",
    "Square", "~"), PW = c(0, 7.667, 29.667, 0, 6.111, 0, 6.333,
    8.5, 0, -6.833, -33.833, 0, -27.833, -13.333, 58.111)), .Names = c("Attributes",
"Levels", "PW"), row.names = c("Memory4GB", "Memory6GB", "Memory8GB",
"RadioNo", "RadioYes", "SizeLarge", "SizeMedium", "SizeSmall",
"Price$50", "Price$100", "Price$150", "ShapeCircular", "ShapeRectangular",
"ShapeSquare", "15"), class = "data.frame"), IW = structure(list(
    Attributes = c("Memory", "Radio", "Size", "Price", "Shape"
    ), IW = c(0.28, 0.058, 0.08, 0.319, 0.263)), .Names = c("Attributes",
"IW"), row.names = c(NA, -5L), class = "data.frame"), plot_ylim = structure(list(
    Min = c(0, 0, 0, -34.1716666666667, -34.1716666666667), Max = c(34.1716666666667,
    34.1716666666667, 34.1716666666667, 0, 0)), .Names = c("Min",
"Max"), row.names = c("Memory", "Radio", "Size", "Price", "Shape"
), class = "data.frame")), .Names = c("PW", "IW", "plot_ylim"
))
  expect_equal(res1,res2)
})








