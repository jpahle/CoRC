#!/bin/bash
set -x

# relocate for easy access
cp CoRC/libs/COPASI.so .
cp CoRC/R/swig_wrapper.R .
cp CoRC/inst/extdata/test_names.cps .

# do a simple test with just the swig wrapper and test_names.cps
# this file seems to be prone to crashing on bad bindings
Rscript -e 'library(methods); library(isnullptr); dyn.load("COPASI.so"); source("swig_wrapper.R"); m <- CRootContainer_addDatamodel(); m$loadModel("test_names.cps"); m$getModel()$getObjectName();'
