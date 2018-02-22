# For upgrading the site I want to have the latest versions of all relevant packages

try(remove.packages("magick"))
install.packages("magick")
devtools::install_github("klutometis/roxygen")
devtools::install_github("r-lib/pkgdown")
devtools::install_github("tidyverse/ggplot2")
devtools::install_github("ropensci/plotly")
devtools::document()
devtools::install()
devtools::test()
pkgdown::clean_site()
pkgdown::build_site()
