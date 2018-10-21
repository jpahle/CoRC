# For upgrading the site I want to have the latest versions of all relevant packages

# reinstall magick to keep up with system imagemagick
try(remove.packages("magick"))
install.packages("magick")
# use latest pkgdown for latest features for now
devtools::install_github("r-lib/pkgdown")
# use fixed plotly versions to be able to keep the 8mb or so of plots frozen when updating the rest of the website
remotes::install_version("ggplot2", "3.0.0")
remotes::install_version("plotly", "4.8.0")
devtools::document()
devtools::install()
devtools::test()
devtools::build_readme()
pkgdown::clean_site()
pkgdown::build_site()
