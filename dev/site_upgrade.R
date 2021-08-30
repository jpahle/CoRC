# For upgrading the site I want to have specific versions of relevant packages
Sys.setenv(COPASI_LIB_PATH = normalizePath(file.path("libs", .Platform$r_arch, paste0("COPASI", .Platform$dynlib.ext))))

# use specific pkgdown to keep page stable for now
remotes::install_version("pkgdown", "1.6.1", upgrade = "never")
# use fixed plotly versions to be able to keep the 8mb or so of plots frozen when updating the rest of the website
remotes::install_version("ggplot2", "3.3.5", upgrade = "never")
remotes::install_version("plotly", "4.9.4.1", upgrade = "never")
devtools::document()
# Pkgdown and the parallel cluster used in the examples vignette needs to have CoRC installed
devtools::install(upgrade = "never")
devtools::test()
devtools::build_readme()
pkgdown::clean_site()
pkgdown::build_site()
