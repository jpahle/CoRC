## Load the COPASI swig package and initialize API.
## This file attempts to load the API into its own environment called cop or in case of the bindings cop$bind.
source("debug_help.R")

cop <- function() {
    bind <- new.env()
    
    setupEnvironment <- function() {
        source("copasi_cacher.R", local = TRUE)
        load("copasi_cache.RData", envir = bind)
        
        # eg. example9 only works with cacheMetaData
        cacheMetaData(bind)
        
        dyn.load(paste0("copasi-dev/build_copasi_r_bindings/copasi/bindings/R/COPASI", .Platform$dynlib.ext))
    }
    
    setupEnvironment()
    
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
    
    # this allows for the copasi wrapper to be supplied in its own environment
    # functions are called from outside with the name$functionName syntax
    environment()
}

# cop is created which is an environment containing this wrapper
cop <- cop()
