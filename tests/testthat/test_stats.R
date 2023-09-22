# library(radiant.multivariate)
# library(testthat)
trim <- function(x) gsub("^\\s+|\\s+$", "", x)

context("Maps")

test_that("City MDS points", {
  result <- mds(city, "from", "to", "distance")
  # str(result)
  res1 <- result$res$points
  # dput(result$res$points)
  res2 <- structure(c(
    -1348.66832957982, -1198.87410814714, -1076.98554040122,
    -1226.93901099845, -428.454832718783, 1596.1594018405, 1697.22828135996,
    1464.04701004452, 522.48712860043, -462.400598146569, -306.546900234988,
    -136.432035420421, 1013.62838366558, -174.603164807742, -639.307768963489,
    131.685862779591, 560.580459896188, 13.3957612318459
  ), .Dim = c(
    9L,
    2L
  ), .Dimnames = list(c(
    "Boston", "NY", "DC", "Miami", "Chicago",
    "Seattle", "SF", "LA", "Denver"
  ), NULL))
  expect_equal(abs(res1), abs(res2))
})

test_that("Computer perceptual map", {
  result <- prmap(computer, "brand", "high_end:business")
  # str(result)
  res1 <- result$fres$scores
  # dput(result$res$points)
  res2 <- structure(c(
    1.2990975042645, -0.318156927318684, -1.18661978839803,
    -0.522421680770708, 0.728100892222923, 0.0936804393886441, -0.208948184854464,
    -0.934302935231416, 1.64813821225715, -0.598567531559918
  ), .Dim = c(
    5L,
    2L
  ), .Dimnames = list(c("Apple", "Dell", "Gateway", "HP", "Sony"), c("RC1", "RC2")))
  expect_equal(res1, res2)
})

context("Factor/PCA analysis")

test_that("Pre nalysis for diamonds", {
  result <- pre_factor(diamonds, c("price", "carat", "table"))
  # str(result)
  res1 <- result$pre_r2
  # dput(result$pre_r2)
  res2 <- structure(list(Rsq = c(0.861258211951766, 0.86356619173057, 0.0450708598611924)), .Names = "Rsq", row.names = c("price", "carat", "table"), class = "data.frame")
  expect_equal(res1, res2)
})

test_that("Factor/PCA analysis for diamonds", {
  result <- full_factor(diamonds, c("price", "carat", "table"))
  # str(result)
  res1 <- result$floadings
  # dput(result$floadings)
  res2 <- structure(list(PC1 = c(
    0.964483176117948, 0.972902482025944,
    0.325710945731448
  )), .Names = "PC1", row.names = c(
    "price", "carat",
    "table"
  ), class = "data.frame")
  expect_equal(res1, res2)
})

context("Cluster analysis")

test_that("Hierarchical cluster analysis", {
  result <- hclus(shopping, vars = "v1:v6")
  # str(result)
  res1 <- result$hc_out$height
  # dput(result$hc_out$height)
  res2 <- c(
    0.693447070258665, 0.77981545158788, 1.19609257290417, 1.20263048421394,
    1.20263048421394, 1.25874249684769, 1.59728591646143, 1.76984887051771,
    1.88396035104441, 2.06113619040031, 3.37654118004185, 3.5167211043475,
    3.77286952167201, 5.26961961999936, 7.6948927428698, 9.4541210015406,
    12.7002828285666, 76.1882734993453, 92.3810886131668
  )
  expect_equal(res1, res2)
})

test_that("K-clustering", {
  result <- kclus(shopping, vars = "v1:v6")
  # str(result)
  res1 <- result$clus_means
  # dput(result$clus_means)
  res2 <- structure(list(v1 = c(5.75, 2.58333333333333), v2 = c(
    3.625,
    4.41666666666667
  ), v3 = c(6, 2.58333333333333), v4 = c(
    3.125,
    4.75
  ), v5 = c(1.875, 4.5), v6 = c(3.875, 4.66666666666667)), class = "data.frame", row.names = c(
    "Cluster 1",
    "Cluster 2"
  ), .Names = c("v1", "v2", "v3", "v4", "v5", "v6"))
  expect_equal(res1, res2)
})

context("Conjoint analysis")

test_that("Conjoint on mp3 data", {
  result <- conjoint(mp3, rvar = "Rating", evar = "Memory:Shape")
  res1 <- capture_output(summary(result))
  res2 <- "Conjoint analysis\nData                 : mp3 \nResponse variable    : Rating \nExplanatory variables: Memory, Radio, Size, Price, Shape \n\nConjoint part-worths:\n   Attributes      Levels      PW\n Memory       4GB           0.000\n Memory       6GB           7.667\n Memory       8GB          29.667\n Radio        No            0.000\n Radio        Yes           6.111\n Size         Large         0.000\n Size         Medium        6.333\n Size         Small         8.500\n Price        $50           0.000\n Price        $100         -6.833\n Price        $150        -33.833\n Shape        Circular      0.000\n Shape        Rectangular -27.833\n Shape        Square      -13.333\n Base utility ~            58.111\n\nConjoint importance weights:\n Attributes    IW\n     Memory 0.280\n     Radio  0.058\n     Size   0.080\n     Price  0.319\n     Shape  0.263\n\nConjoint regression results:\n\n                   coefficient\n (Intercept)            58.111\n Memory|6GB              7.667\n Memory|8GB             29.667\n Radio|Yes               6.111\n Size|Medium             6.333\n Size|Small              8.500\n Price|$100             -6.833\n Price|$150            -33.833\n Shape|Rectangular     -27.833\n Shape|Square          -13.333\n"
  expect_equal(res1, res2)
})
