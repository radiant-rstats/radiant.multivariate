trim_trailing <- function(x) sub("\\s+$", "", x)
trim_leading <- function(x) sub("^\\s+", "", x)
trim <- function(x) gsub("^\\s+|\\s+$", "", x)

compare_output <- function(res1, res2) {
  for (i in 1:length(res2)) {
    if (res1[i] != res2[i]) {
      print(i)
      print(res1[i])
      print(res2[i])
    }
  }
}

######### tests ########
