.onLoad <- function(libname, pkgname) {
  # methods::cacheMetaData(1)
  
  # This is to make devtools::load_all work but can be removed later
  library.dynam("COPASI", "rCopasi", .libPaths())
  # 
  
  invisible()
}

.onUnload <- function(libpath) {
  library.dynam.unload("COPASI", libpath)
}
