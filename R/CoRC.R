#' CoRC: Copasi R Console.
#'
#' Run timecourses and stuff
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
COPASI_VERSION <- "https://github.com/copasi/COPASI/commit/81c1003bb7aa3839cbde506c4343a115da0e678f"

COPASI_BIN_VERSION <- 6L
COPASI_BIN_HASHES <- list(
  x86_64 = c(
    windows = "",
    darwin = "85cd69c8c75835df206f9c5b270bb3b4cf390f5a8f33470c90e2290cb4a7b410",
    unix = ""
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
    
  try(library.dynam("COPASI", pkgname, libname), silent = TRUE)
  # In this single case only warn instead of stop
  assert_binaries(warning, pkgname)
}

.onUnload <- function(libpath) {
  try(library.dynam.unload("COPASI", libpath), silent = TRUE)
}

#' Install copasi binaries
#'
#' \code{getCopasi} automatically downloads binaries or retrieves them from given path.
#' To install copasi binaries, you need write access to the packages installation folder.
#'
#' @param path optional file path to copasi binaries
#' @param force optional bool to force overwriting the binaries
#' @param silent optional bool to silence messages
#' @export
getCopasi <- function(path = NULL, force = FALSE, silent = FALSE) {
  assert_that(
    is.null(path) || is.readable(path),
    is.flag(force), noNA(force),
    is.flag(silent), noNA(silent)
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
  
  if (!dir.exists(libsdir)) {
    assert_that(
      dir.create(libsdir, recursive = TRUE),
      msg = "No write access to package directory."
    )
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
        if (!silent)
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
    dlstatus <- download.file(url = dlurl, destfile = dlpath, method = "auto", quiet = silent, mode = "wb")
    
    assert_that(dlstatus == 0, msg = "Downloading copasi binaries failed.")
    
    # Check if the hash matches
    assert_that(
      digest::digest(dlpath, algo = "sha256", file = TRUE) == COPASI_BIN_HASHES[[arch]][os],
      msg = "Downloaded copasi binaries are corrupted."
    )
  }
  
  # Try to unload COPASI if loaded so the binaries can be overwritten
  pkg_env$cl_loaded_dms <- list()
  pkg_env$c_curr_dm <- NULL
  try(library.dynam.unload("COPASI", system.file(package = getPackageName())), silent = TRUE)
  
  # Copy file into package folder
  assert_that(
    file.copy(path, libfile, overwrite = TRUE),
    msg = "Copying copasi binaries into package folder failed."
  )
  
  # Reload libary
  library.dynam("COPASI", getPackageName(), .libPaths())
  message(getPackageName(), ": Successfully loaded copasi binaries.")
}

assert_binaries <- function(method = stop, pkgname = getPackageName()) {
  if (!("COPASI" %in% map_chr(.dynLibs(), "name")))
    method(pkgname, ": Copasi binaries are not installed. Use ", pkgname, "::getCopasi() to install them.")
}
