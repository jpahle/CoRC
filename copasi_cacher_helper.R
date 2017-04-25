## Load the COPASI swig package and save the R session to enable the use of this session as a cache for the COPASI.R file.

library(methods)

source("copasi-dev/build_copasi_r_bindings/copasi/bindings/R/COPASI.R")

save(list = ls(all.names = TRUE), file = "copasi_cache.RData")
