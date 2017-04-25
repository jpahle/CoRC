## Load the COPASI swig package and initialize API.
source("debug_help.R")

source("copasi_cacher.R")
load("copasi_cache.RData")

# eg. example9 only works with cacheMetaData
cacheMetaData(1)

dyn.load(paste0("copasi-dev/build_copasi_r_bindings/copasi/bindings/R/COPASI", .Platform$dynlib.ext))

API_CURRENT_DATAMODEL <- NULL

getCurrentModel <- function() {
    if (is.null(API_CURRENT_DATAMODEL)) stop("No model currently in use.")
    
    API_CURRENT_DATAMODEL
}

loadModel <- function(filename) {
    assert_that(is.readable(filename))
    
    datamodel <- CRootContainer_addDatamodel()
    success <- datamodel$loadModel(filename)
    
    if (!success) {
        CRootContainer_removeDatamodel(datamodel)
        stop("Couldn't load model file")
    }
    
    API_CURRENT_DATAMODEL <<- datamodel
    datamodel
}
