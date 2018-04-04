#!/bin/bash
set -x

# relocate for easy access
cp CoRC/development/copasi-source/libs/COPASI_unix_x86_64.so COPASI.so
cp CoRC/R/swig_wrapper.R .
cp CoRC/inst/extdata/test_names.cps .

# build package
/opt/R/latest/bin/R CMD build --no-resave-data --no-build-vignettes --no-manual CoRC/
mv CoRC_*.tar.gz CoRC.tar.gz

# install CoRC and further dependencies
Rscript                   -e "$R_OPTS remotes::install_local('CoRC.tar.gz', INSTALL_opts = '--install-tests')"
/opt/R/latest/bin/Rscript -e "$R_OPTS remotes::install_local('CoRC.tar.gz', INSTALL_opts = '--install-tests')"
/opt/R/3.4/bin/Rscript    -e "$R_OPTS remotes::install_local('CoRC.tar.gz', INSTALL_opts = '--install-tests')"
/opt/R/3.3/bin/Rscript    -e "$R_OPTS remotes::install_local('CoRC.tar.gz', INSTALL_opts = '--install-tests')"
/opt/R/3.2/bin/Rscript    -e "$R_32_OPTS remotes::install_local('CoRC.tar.gz', INSTALL_opts = '--install-tests')"
/opt/R/3.1/bin/Rscript    -e "$R_31_OPTS remotes::install_local('CoRC.tar.gz', INSTALL_opts = '--install-tests')"

# use the most recent COPASI_unix_x64.so for CoRC
Rscript                   -e "CoRC::getCopasi(path = 'COPASI.so')"
/opt/R/latest/bin/Rscript -e "CoRC::getCopasi(path = 'COPASI.so')"
/opt/R/3.4/bin/Rscript    -e "CoRC::getCopasi(path = 'COPASI.so')"
/opt/R/3.3/bin/Rscript    -e "CoRC::getCopasi(path = 'COPASI.so')"
/opt/R/3.2/bin/Rscript    -e "CoRC::getCopasi(path = 'COPASI.so')"
/opt/R/3.1/bin/Rscript    -e "CoRC::getCopasi(path = 'COPASI.so')"

# Debug info
Rscript                   -e "sessioninfo::session_info('CoRC')"
/opt/R/latest/bin/Rscript -e "sessioninfo::session_info('CoRC')"
/opt/R/3.4/bin/Rscript    -e "sessioninfo::session_info('CoRC')"
/opt/R/3.3/bin/Rscript    -e "sessioninfo::session_info('CoRC')"
/opt/R/3.2/bin/Rscript    -e "sessioninfo::session_info('CoRC')"
/opt/R/3.1/bin/Rscript    -e "sessioninfo::session_info('CoRC')"

# do a simple test with just the swig wrapper and test_names.cps
# this file seems to be prone to crashing on bad api
Rscript                   -e 'R.version.string; library(methods); library(isnullptr); dyn.load("COPASI.so"); source("swig_wrapper.R"); m <- CRootContainer_addDatamodel(); m$loadModel("test_names.cps"); m$getModel()$getObjectName()'
/opt/R/latest/bin/Rscript -e 'R.version.string; library(methods); library(isnullptr); dyn.load("COPASI.so"); source("swig_wrapper.R"); m <- CRootContainer_addDatamodel(); m$loadModel("test_names.cps"); m$getModel()$getObjectName()'
/opt/R/3.4/bin/Rscript    -e 'R.version.string; library(methods); library(isnullptr); dyn.load("COPASI.so"); source("swig_wrapper.R"); m <- CRootContainer_addDatamodel(); m$loadModel("test_names.cps"); m$getModel()$getObjectName()'
/opt/R/3.3/bin/Rscript    -e 'R.version.string; library(methods); library(isnullptr); dyn.load("COPASI.so"); source("swig_wrapper.R"); m <- CRootContainer_addDatamodel(); m$loadModel("test_names.cps"); m$getModel()$getObjectName()'
/opt/R/3.2/bin/Rscript    -e 'R.version.string; library(methods); library(isnullptr); dyn.load("COPASI.so"); source("swig_wrapper.R"); m <- CRootContainer_addDatamodel(); m$loadModel("test_names.cps"); m$getModel()$getObjectName()'
/opt/R/3.1/bin/Rscript    -e 'R.version.string; library(methods); library(isnullptr); dyn.load("COPASI.so"); source("swig_wrapper.R"); m <- CRootContainer_addDatamodel(); m$loadModel("test_names.cps"); m$getModel()$getObjectName()'

# CoRC testset
Rscript                   -e "R.version.string; testthat::test_package('CoRC')"
/opt/R/latest/bin/Rscript -e "R.version.string; testthat::test_package('CoRC')"
/opt/R/3.4/bin/Rscript    -e "R.version.string; testthat::test_package('CoRC')"
# Older Rscript causes crashes
/opt/R/3.3/bin/R       -q -e "R.version.string; testthat::test_package('CoRC')"
/opt/R/3.2/bin/R       -q -e "R.version.string; testthat::test_package('CoRC')"
/opt/R/3.1/bin/R       -q -e "R.version.string; testthat::test_package('CoRC')"
