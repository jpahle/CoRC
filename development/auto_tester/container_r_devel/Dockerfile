FROM rocker/tidyverse:devel

# tidyverse should required packages installed
# potential misses: remotes, testthat, devtools

# install corc
RUN R -e 'remotes::install_github("jpahle/CoRC", INSTALL_opts = "--install-tests"); CoRC::getCopasi()'
