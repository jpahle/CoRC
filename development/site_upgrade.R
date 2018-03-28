# For upgrading the site I want to have the latest versions of all relevant packages

try(remove.packages("magick"))
install.packages("magick")
devtools::install_github("klutometis/roxygen")
devtools::install_github("r-lib/pkgdown")
devtools::install_github("tidyverse/ggplot2")
# for now, use fixed plotly version, as, more recent, fail to scale ggplotly() properly
devtools::install_github("ropensci/plotly", ref = "d7eaf38e915462db75ae09790fc1b7067e10091a")
devtools::document()
devtools::install()
devtools::test()
pkgdown::clean_site()
pkgdown::build_site()
