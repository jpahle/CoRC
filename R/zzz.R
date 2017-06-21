.onLoad <- function(libname, pkgname) {
  shared_lib_path <- file.path(system.file("libs", package = pkgname), paste0("COPASI", .Platform$dynlib.ext))
  
  if (file.exists(shared_lib_path)) library.dynam("COPASI", pkgname, libname)

  invisible()
}

.onUnload <- function(libpath) {
  if ("COPASI" %in% map_chr(.dynLibs(), "name")) library.dynam.unload("COPASI", libpath)
}

#' @export
getCopasi <- function(path) {
  libsdir <- file.path(system.file(package = "CoRC"), "libs")
  success <- TRUE
  if (!dir.exists(libsdir)) success <- dir.create(libsdir)
  libfile <- file.path(libsdir, paste0("COPASI", .Platform$dynlib.ext))
  if (success) success <- file.copy(path, libfile)

  if (success) {
    library.dynam("COPASI", "CoRC", .libPaths())
    cat("CoRC: Successfully loaded copasi binaries.")
  } else {
    cat("CoRC: Loading of copasi binaries failed.")
  }
}
