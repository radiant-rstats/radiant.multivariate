## build for mac
curr <- setwd("../")
devtools::install(app, upgrade = "never")
f <- devtools::build(app)
system(paste0("R CMD INSTALL --build ", f))
setwd(curr)


?devtools::install
