cop <- function() {
  bind <- NULL
  
  setupEnvironment <- function() {
    library(tidyverse, warn.conflicts = FALSE)
    
    dyn.load(paste0("copasi-dev/build_copasi_r_bindings/copasi/bindings/R/COPASI", .Platform$dynlib.ext))
    
    # if (file.exists("copasi-r-bindings/COPASI.rds"))
    #   try(bind <<- readRDS("copasi-r-bindings/COPASI.rds"), silent = TRUE)
    
    if (!is.environment(bind) || !is.character(bind$RCACHEVERSION) || bind$RCACHEVERSION != version$minor) {
      bind <<- new.env()
      # size arg might be useful for performance
      # bind <<- new.env(size = 6000L)
      source("copasi-dev/build_copasi_r_bindings/copasi/bindings/R/COPASI.R", local = bind)
      # I Don't know exactly what the next line does, but this is what the SWIG
      # documentation has to say about it:
      # The cacheMetaData(1) will cause R to refresh its object tables. Without it, inheritance of wrapped objects may fail.
      cacheMetaData(1)
      bind$RCACHEVERSION <- version$minor
      saveRDS(bind, "copasi-r-bindings/COPASI.rds")
    }
  }
  
  setupEnvironment()
  
  bind$enumToInteger <- function(name,type)
  {
      if (is.character(name)) {
          ans <- as.integer(get0(paste(".__E__", type, sep = ""))[name])
          if (length(ans) == 0) {ans <- as.integer(get(paste(".__E__", substr(type, 3, nchar(type)), sep = ""))[name])}
          if (is.na(ans)) {warning("enum not found ", name, " ", type)}
          ans
      }
  }
  
  bind$enumFromInteger =
      function(i,type)
      {
          itemlist <- get0(paste(".__E__", type, sep=""))
          if (length(itemlist) == 0) {itemlist <- get(paste(".__E__", substr(type, 3, nchar(type)), sep = ""))}
          names(itemlist)[match(i, itemlist)]
      }
  
  # this allows for the copasi wrapper to be supplied in its own environment
  # functions are called from outside with the name$functionName syntax
  environment()
}

# cop is created which is an environment containing this wrapper
cop <- cop()
