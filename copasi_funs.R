## Load the COPASI swig package and initialize API.
source("debug_help.R")

source("copasi_cacher.R")
load("copasi_cache.RData")

# eg. example9 only works with cacheMetaData
cacheMetaData(1)

dyn.load(paste0("copasi-dev/build_copasi_r_bindings/copasi/bindings/R/COPASI", .Platform$dynlib.ext))

loadModel <- function(filename) {
    dataModel <- CCopasiRootContainer_addDatamodel()
    CCopasiDataModel_loadModel(dataModel,filename)
    CCopasiDataModel_getModel(dataModel)
}
