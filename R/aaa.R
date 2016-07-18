# to avoid 'no visible binding for global variable' NOTE
globalVariables(c(".","y","nr_clus","nr_fact","height","bump","n","se","ci"))

#' radiant.multivariate
#'
#' @name radiant.multivariate
#' @docType package
#' @import radiant.data shiny ggplot2
#' @importFrom dplyr select select_ filter mutate funs group_by group_by_ summarise_each lag slice
#' @importFrom magrittr %>% %<>% %T>% set_colnames set_rownames
#' @importFrom gridExtra arrangeGrob
#' @importFrom scales percent
#' @importFrom import from
#' @importFrom grDevices rainbow
#' @importFrom graphics abline arrows par plot points text title
#' @importFrom methods is
#' @importFrom stats as.dendrogram as.dist cmdscale cor cov cutree dist factanal hclust kmeans lm na.omit qt sd
NULL

#' Conjoint data for MP3 players
#' @details Ratings reflect the evaluation of 18 alternative MP3 players by one respondent. Description provided in attr(mp3,"description")
#' @docType data
#' @keywords datasets
#' @name mp3
#' @usage data(mp3)
#' @format A data frame with 18 rows and 6 variables
NULL

#' Conjoint data for Movie theaters
#' @details Rankings reflect the evaluation of 18 alternative movie theaters by one respondent. Description provided in attr(movie,"description")
#' @docType data
#' @keywords datasets
#' @name movie
#' @usage data(movie)
#' @format A data frame with 18 rows and 6 variables
NULL

#' Carpet cleaners
#' @details Rankings reflect the evaluation of 18 alternative carpet cleaners by one respondent. Description provided in attr(carpet,"description")
#' @docType data
#' @keywords datasets
#' @name carpet
#' @usage data(carpet)
#' @format A data frame with 18 rows and 5 variables
NULL

#' Shopping attitudes
#' @details Attitudinal data on shopping for 20 consumers. Description provided in attr(shopping,"description")
#' @docType data
#' @keywords datasets
#' @name shopping
#' @usage data(shopping)
#' @format A data frame with 20 rows and 7 variables
NULL

#' Toothpaste attitudes
#' @details Attitudinal data on toothpaste for 60 consumers. Description provided in attr(toothpaste,"description")
#' @docType data
#' @keywords datasets
#' @name toothpaste
#' @usage data(toothpaste)
#' @format A data frame with 60 rows and 10 variables
NULL

#' City distances
#' @details Distance in miles between nine cities in the USA. The dataset is used to illustrate multi-dimensional scaling (MDS). Description provided in attr(city,"description")
#' @docType data
#' @keywords datasets
#' @name city
#' @usage data(city)
#' @format A data frame with 45 rows and 3 variables
NULL

#' City distances 2
#' @details Distance in miles between 12 cities in the USA. The dataset is used to illustrate multi-dimensional scaling (MDS). Description provided in attr(city2,"description")
#' @docType data
#' @keywords datasets
#' @name city2
#' @usage data(city2)
#' @format A data frame with 78 rows and 3 variables
NULL

#' Toothpaste brands
#' @details Perceived (dis)similarity of a set of toothpaste brands. The dataset is used to illustrate multi-dimensional scaling (MDS). Description provided in attr(tpbrands,"description")
#' @docType data
#' @keywords datasets
#' @name tpbrands
#' @usage data(tpbrands)
#' @format A data frame with 45 rows and 4 variables
NULL

#' Perceptions of computer (re)sellers
#' @details Perceptions of computer (re)sellers. The dataset is used to illustrate perceptual maps. Description provided in attr(computer,"description")
#' @docType data
#' @keywords datasets
#' @name computer
#' @usage data(computer)
#' @format A data frame with 5 rows and 8 variables
NULL

#' Perceptions of retailers
#' @details Consumer evaluations for a set of retailers in the Chicago area on 7 attributes. The dataset is used to illustrate perceptual maps. Description provided in attr(retailers,"description")
#' @docType data
#' @keywords datasets
#' @name retailers
#' @usage data(retailers)
#' @format A data frame with 6 rows and 10 variables
NULL
