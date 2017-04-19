## Load the COPASI swig package and initialize API.
## This file attempts to load the API into its own environment called cop or in case of the bindings cop$bind.

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
        dataModel <- bind$CCopasiRootContainer_addDatamodel()
        bind$CCopasiDataModel_loadModel(dataModel,filename)
        bind$CCopasiDataModel_getModel(dataModel)
    }
    
    # this allows for the copasi wrapper to be supplied in its own environment
    # functions are called from outside with the name$functionName syntax
    environment()
}

# cop is created which is an environment containing this wrapper
cop <- cop()
