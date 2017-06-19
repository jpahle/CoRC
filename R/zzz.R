.onLoad <- function(libname, pkgname) {
  # methods::cacheMetaData(1)
  
  # This is to make devtools::load_all work but can be removed later.
  # Same in .onUnload
  # library.dynam("COPASI", pkgname, libname)
  library.dynam("COPASI", pkgname, .libPaths())
  #
  
  invisible()
}

.onUnload <- function(libpath) {
  # This is to make devtools::load_all work but can be removed later.
  # Same in .onLoad
  if ("COPASI" %in% names(getLoadedDLLs())) library.dynam.unload("COPASI", libpath)
  #
}
