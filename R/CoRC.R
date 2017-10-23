#' CoRC: Copasi R Console.
#'
#' CoRC, the Copasi R Connector, links the Complex Pathway Simulator COPASI to R.
#' It provides easy access to the powerful biochemical model editing, simulation and analysis backend of Copasi.
#' This allows the user to develop elaborate scripts and workflows for analyses that would require a great deal of tedious manual work otherwise.
#' These scripts can then be run interactively or be sent to cluster or cloud facilities for more demanding calculations.
#' 
#' CoRC features:
#'   
#' * high-level API for Copasi
#' * define reproducible workflows using Copasi's powerful modelling tools
#' * easy parallization of Copasi tasks
#' * enables straight forward publication-ready plotting with the R ecosystem
#' * ...
#' 
#' It is based on a current development version of Copasi but is expected to closely follow official Copasi releases in the future.
#'
#' @section quantity accessors:
#' Use dataframes to manipulate species and quantities and stuff
#'
#' @import assertthat
#' @import methods
#' @import purrr
#' @importFrom isnullptr isnullptr
#' @importFrom rlang .data
#' @importFrom stats runif na.omit
#' @importFrom stringr fixed coll regex
#' @importFrom utils head tail download.file
"_PACKAGE"

# Output flag for tasks
OUTPUTFLAG <- 119L

#' @export
COPASI_VERSION <- "https://github.com/copasi/COPASI/commit/25182a8c711b51c42af397f87418ec3bb6b0cd04"

COPASI_BIN_VERSION <- 7L
COPASI_BIN_HASHES <- list(
  x86_64 = c(
    windows = "7dd534d7b5e4b9fbb63e96d093dc51a694ad811c5009f935983399ba9f0123f5",
    darwin = "72a70e72d2f5e6902c6f8a7d648a988cc42314a5d2ee430a52cfc4bc6a85c5f7",
    unix = "527043b30886c535d2933aaa161e0ea1ca527dcd2ef79ea475f8358d1b7b43db"
  )
)

# Package environment for persistent options etc
pkg_env <- new.env(parent = emptyenv())
# Variable to keep track of the default c_datamodel
pkg_env$c_curr_dm <- NULL
# List to keep track of loaded models.
# If I keep getting new references from C instead of using this list,
# the user can crash the R session by using unloaded model references
pkg_env$cl_loaded_dms <- list()

.onLoad <- function(libname, pkgname) {
  # hack for devtools::load_all()
  # requires for the package to be installed already
  if (getNamespaceName(environment(library.dynam.unload)) == "devtools")
    libname <- .libPaths()
  
  backports::import(pkgname, c("anyNA", "dir.exists", "hasName", "lengths"))
    
  try(library.dynam("COPASI", pkgname, libname), silent = TRUE)
  # TODO
  try(CCopasiMessage_clearDeque(), silent = TRUE)
  # In this single case only warn instead of stop
  assert_binaries(partial(warning, immediate. = TRUE), pkgname)
}

.onUnload <- function(libpath) {
  try(library.dynam.unload("COPASI", libpath), silent = TRUE)
}

#' Install copasi binaries
#'
#' \code{getCopasi} automatically downloads binaries or retrieves them from given path.
#' 
#' In case of no internet connection, run the function and retrieve the URL from the error message.
#' Download the file manually, copy it to a local path and give its path via the \code{path} argument.
#' 
#' To install copasi binaries, you need write access to the packages installation folder.
#'
#' @param path optional file path to copasi binaries
#' @param force optional bool to force overwriting the binaries
#' @param quiet optional bool to silence messages
#' @export
getCopasi <- function(path = NULL, force = FALSE, quiet = FALSE) {
  assert_that(
    is.null(path) || is.readable(path),
    is.flag(force), noNA(force),
    is.flag(quiet), noNA(quiet)
  )
  
  # CHECK OS
  os <- NULL
  if (.Platform$OS.type == "windows") {
    os <- "windows"
  } else if (.Platform$OS.type == "unix") {
    if (substr(R.Version()$os, 1L, 6L) == "darwin")
      os <- "darwin"
    else
      os <- "unix"
    # Gather data about os (created for ubuntu but may work for other unix os)
    # osfile <- readLines("/etc/os-release")
    # 
    # id_string <- na.omit(stringr::str_match(osfile, "^ID=(.+)$"))
    # v_string <- na.omit(stringr::str_match(osfile, "^VERSION_ID=\\\"(\\d+)\\.(\\d+)\\\"$"))
    # 
    # if (!is_empty(id_string)) id_string <- id_string[1, 2]
    # if (!is_empty(v_string)) v_string <- v_string[1, c(2,3)]
    # 
    # os <- paste0(id_string, "_", paste0(v_string, collapse = "_"))
  }
  arch <- R.Version()$arch
  
  # CHECK CURRENT FILE
  libsdir <- file.path(system.file(package = getPackageName()), "libs")
  
  # Win needs different locations for the .dll files (subfolders).
  if (os == "windows") {
    if (version$arch == "x86_64") {
      libsdir <- file.path(libsdir, "x64")
      # not sure what $arch actually is for x86
    } else if (version$arch == "x86") {
      libsdir <- file.path(libsdir, "i386")
    }
  }
  
  libfile <- file.path(libsdir, paste0("COPASI", .Platform$dynlib.ext))
  
  # if no path is given we download the binaries
  if (is.null(path)) {
    assert_that(
      !is.null(os),
      msg = "Unsupported platform. Supply copasi binaries via the `path` argument."
    )
    
    assert_that(
      arch %in% names(COPASI_BIN_HASHES),
      msg = paste0("Architecture `", arch, "` is currently unsupported.")
    )
    
    assert_that(
      os %in% names(COPASI_BIN_HASHES[[arch]]),
      msg = paste0("There are currently no binaries available for your platform (", os, "_", arch, ").")
    )
    
    # Don't do anything if the current lib file is good.
    if (!force && file.exists(libfile)) {
      current_file_hash <- digest::digest(libfile, algo = "sha256", file = TRUE)
      if (current_file_hash == COPASI_BIN_HASHES[[arch]][os]) {
        if (!quiet)
          message("Skipping download because current binaries are up to date and uncorrupted.")
        return(invisible())
      }
    }
    
    dlurl <- paste0(
      "http://juergen.pahle.de/CoRC_libs/",
      "v", COPASI_BIN_VERSION, "/",
      "COPASI_", os,
      "_", arch,
      .Platform$dynlib.ext
    )
    
    dlpath <- tempfile(pattern = "COPASI", fileext = .Platform$dynlib.ext)
    
    # download the binaries
    dlstatus <- download.file(url = dlurl, destfile = dlpath, method = "auto", quiet = quiet, mode = "wb")
    
    assert_that(dlstatus == 0, msg = "Downloading copasi binaries failed.")
    
    # Check if the hash matches
    assert_that(
      digest::digest(dlpath, algo = "sha256", file = TRUE) == COPASI_BIN_HASHES[[arch]][os],
      msg = "Downloaded copasi binaries are corrupted."
    )
  } else {
    dlpath <- path
  }
  
  # Try to unload COPASI if loaded so the binaries can be overwritten
  try(unloadAllModels(), silent = TRUE)
  try(library.dynam.unload("COPASI", system.file(package = getPackageName())), silent = TRUE)
  
  # Create folder
  if (!dir.exists(libsdir)) {
    assert_that(
      dir.create(libsdir, recursive = TRUE),
      msg = "No write access to package directory."
    )
  }
  
  # Copy file into package folder
  assert_that(
    file.copy(dlpath, libfile, overwrite = TRUE),
    msg = "Copying copasi binaries into package folder failed."
  )
  
  # Reload libary
  library.dynam("COPASI", getPackageName(), .libPaths())
  # TODO
  CCopasiMessage_clearDeque()
  if (!quiet)
    message(getPackageName(), ": Successfully loaded copasi binaries.")
  
  invisible()
}

# check if copasi lib is loaded.
# default method for error is stop.
# .onLoad uses warning instead.
assert_binaries <- function(method = stop, pkgname = getPackageName()) {
  if (!("COPASI" %in% map_chr(.dynLibs(), "name")))
    method(pkgname, ": Copasi binaries are not installed. Use ", pkgname, "::getCopasi() to install them.")
}
