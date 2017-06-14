.onLoad <- function(libname, pkgname) {
  # methods::cacheMetaData(1)
  
  # This is to make devtools::load_all work but can be removed later.
  # Same in .onUnload
  library.dynam("COPASI", "CoRC", .libPaths())
  #
  
  invisible()
}

.onUnload <- function(libpath) {
  # This is to make devtools::load_all work but can be removed later.
  # Same in .onLoad
  if ("COPASI" %in% map_chr(.dynLibs(), "name")) library.dynam.unload("COPASI", libpath)
  #
}
