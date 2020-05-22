FROM r-base:latest

# tidyverse should have required packages installed
# potential misses: remotes, testthat, devtools

RUN R -e 'install.packages("remotes")'

# install corc
RUN R -e 'remotes::install_github("jpahle/CoRC", INSTALL_opts = "--install-tests"); CoRC::getCopasi()'

RUN R -e 'remotes::install_cran(c("sessioninfo", "testthat"))'

