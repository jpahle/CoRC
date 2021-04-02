#!/bin/bash
set -x

# relocate for easy access
cp CoRC/libs/COPASI.so .
cp CoRC/R/swig_wrapper.R .
cp CoRC/inst/extdata/test_names.cps .

# install CoRC and further dependencies
Rscript -e "$R_OPTS remotes::install_local('CoRC/', upgrade = 'never', INSTALL_opts = '--install-tests', configure.args = '--copasi-lib-path=/work/COPASI.so');"

# Debug info
Rscript -e "sessioninfo::session_info('CoRC');"

# CoRC testset
if [ $UNSTABLE_RSCRIPT ]; then
    # Older Rscript causes crashes
    R -q -e "testthat::test_package('CoRC');"
else
    Rscript -e "testthat::test_package('CoRC');"
fi
