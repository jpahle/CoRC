## Load the COPASI swig package and initialize API.

if (!file.exists("copasi_cache.RData")) {
  system2("Rscript", c("--vanilla", "copasi_cache_helper.R"))
}
load("copasi_cache.RData")

# eg. example9 only works with cacheMetaData
cacheMetaData(1)

dyn.load(paste0("copasi-dev/build_copasi_r_bindings/copasi/bindings/R/COPASI", .Platform$dynlib.ext))
