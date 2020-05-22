# For upgrading the site I want to have specific versions of relevant packages

# use specific pkgdown to keep page stable for now
remotes::install_version("pkgdown", "1.4.1", upgrade = "never")
# use fixed plotly versions to be able to keep the 8mb or so of plots frozen when updating the rest of the website
remotes::install_version("ggplot2", "3.2.1", upgrade = "never")
remotes::install_version("plotly", "4.9.0", upgrade = "never")
devtools::document()
devtools::install(upgrade = "never")
devtools::test()
devtools::build_readme()
pkgdown::clean_site()
pkgdown::build_site()
