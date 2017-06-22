.onLoad <- function(libname, pkgname) {
  # hack for load_all() 
  libname <- .libPaths()
  
  iserr <- try(library.dynam("COPASI", pkgname, libname), silent = TRUE)
  if (is(iserr, "try-error")) warning(pkgname, ": Copasi binaries are not installed. Use getCopasi to install them.")
}

.onUnload <- function(libpath) {
  try(library.dynam.unload("COPASI", libpath), silent = TRUE)
}

#' @export
getCopasi <- function(path) {
  libsdir <- file.path(system.file(package = getPackageName()), "libs", "x64")
  success <- TRUE
  if (!dir.exists(libsdir)) success <- dir.create(libsdir, recursive = TRUE)
  libfile <- file.path(libsdir, paste0("COPASI", .Platform$dynlib.ext))
  if (success) success <- file.copy(path, libfile)

  if (success) {
    library.dynam("COPASI", getPackageName(), .libPaths())
    message(getPackageName(), ": Successfully loaded copasi binaries.")
  } else {
    message(getPackageName(), ": Loading of copasi binaries failed.")
  }
}
