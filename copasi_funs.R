library(tidyverse, warn.conflicts = FALSE)
    
#options(echo = FALSE) # disable echoing of input

# This is an example on how to build models with the COPASI backend API.

# First we load the COPASI package
dyn.load(paste("copasi-dev/build_copasi_r_bindings/copasi/bindings/R/COPASI", .Platform$dynlib.ext, sep=""))
source("copasi-dev/build_copasi_r_bindings/copasi/bindings/R/COPASI.R")
# I Don't know exactly what the next line does, but this is what the SWIG
# documentation has to say about it:
# The cacheMetaData(1) will cause R to refresh its object tables. Without it, inheritance of wrapped objects may fail.
cacheMetaData(1)
  
enumToInteger <- function(name,type) {
    if (is.character(name)) {
        ans <- as.integer(get0(paste(".__E__", type, sep = ""))[name])
        if (length(ans) == 0) {ans <- as.integer(get(paste(".__E__", substr(type, 3, nchar(type)), sep = ""))[name])}
        if (is.na(ans)) {warning("enum not found ", name, " ", type)}
        ans
    }
}
  
enumFromInteger <- function(i,type) {
    itemlist <- get0(paste(".__E__", type, sep=""))
    if (length(itemlist) == 0) {itemlist <- get(paste(".__E__", substr(type, 3, nchar(type)), sep = ""))}
    names(itemlist)[match(i, itemlist)]
}

loadModel <- function(filename) {
    dataModel <- CCopasiRootContainer_addDatamodel()
    CCopasiDataModel_loadModel(dataModel,filename)
    CCopasiDataModel_getModel(dataModel)
}