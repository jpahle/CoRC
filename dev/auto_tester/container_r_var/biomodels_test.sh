#!/bin/bash
set -x

# relocate for easy access
cp CoRC/libs/COPASI.so .
cp CoRC/R/swig_wrapper.R .
cp CoRC/inst/extdata/test_names.cps .

# install CoRC and further dependencies
COPASI_LIB_PATH=/work/COPASI.so Rscript -e "$R_OPTS remotes::install_local('CoRC/', upgrade = 'never', INSTALL_opts = '--install-tests');"

# Debug info
Rscript -e "sessioninfo::session_info('CoRC');"

Rscript -e "$R_OPTS install.packages('dplyr');"
Rscript -e "setwd(system.file('tests', 'other', package = 'CoRC')); source('test_biomodels.R');"

# CoRC testset
# if [ $UNSTABLE_RSCRIPT ]; then
#     # Older Rscript causes crashes
#     R -q -e "testthat::test_package('CoRC');"
# else
#     Rscript -e "testthat::test_package('CoRC');"
# fi
