.onLoad <- function(libname, pkgname) {
  # methods::cacheMetaData(1)

  invisible()
}

.onUnload <- function(libpath) {
  library.dynam.unload("COPASI", libpath)
}
