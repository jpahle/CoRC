#' CoRC: COPASI R Connector.
#'
#' CoRC, the COPASI R Connector, links the Complex Pathway Simulator COPASI to R.
#' It provides easy access to the powerful biochemical model editing, simulation and analysis backend of COPASI.
#' This allows the user to develop elaborate scripts and workflows for analyses that would require a great deal of tedious manual work otherwise.
#' These scripts can then be run interactively or be sent to cluster or cloud facilities for more demanding calculations.
#' 
#' CoRC features:
#'   
#' * high-level API for COPASI in the R language.
#' * Immediate access to R's data analysis capabilities and publication-ready plotting.
#' * Reproducible workflows from data generation to analysis and plotting (R scripts and notebooks).
#' * Rule-based modification of model structure to test structural variations or create large models.
#' * Scaling up assays, e.g. from 3 models to 3000.
#' * Handling of multiple models at once.
#' * Parallelization on multi-core machines or computing clusters.
#' 
#' It is based on a current development version of COPASI but is expected to closely follow official COPASI releases in the future.
#'
#' @import assertthat
#' @import methods
#' @import purrr
#' @importFrom isnullptr isnullptr
#' @importFrom rlang .data
#' @importFrom stringr fixed coll regex
#' @importFrom utils head tail
"_PACKAGE"

# Output flag for tasks
OUTPUTFLAG <- 119L

COPASI_BIN_VERSION <- 12L
COPASI_BIN_HASHES <- list(
  x86_64 = c(
    windows = "ae0758aebc2b0961ab3473e841c7d52bb4cc3dca08665de9390db94e5b159779",
    darwin  = "f9ac7a95e2402c50651242dd2eb476133f4b108fbcfd8229b6c48eb45d88d368",
    unix    = "d000af64588fc9d02dd86ae7f0e71dffcec2f6161bfd7b7f9add9e5355751195"
  )
)

COPASI_BIN_BASE_URL <- "http://juergen.pahle.de/CoRC_libs/"
dl_url_former <- function(base = COPASI_BIN_BASE_URL, version = COPASI_BIN_VERSION, os, arch, ext = .Platform$dynlib.ext) {
  paste0(
    base,
    "v", version, "/",
    "COPASI_", os,
    "_", arch,
    ext
  )
}

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
  
  backports::import(pkgname, c("anyNA", "dir.exists", "lengths"))
    
  try(library.dynam("COPASI", pkgname, libname), silent = TRUE)
  # TODO
  # clearing the deque hides the annoying message about COPASI home directory on linux
  try(CCopasiMessage_clearDeque(), silent = TRUE)
  # In this single case only warn instead of stop
  assert_binaries(partial(warning, immediate. = TRUE), pkgname)
}

.onUnload <- function(libpath) {
  try(library.dynam.unload("COPASI", libpath), silent = TRUE)
}

#' Install COPASI binaries
#'
#' \code{getCopasi} automatically downloads binaries or retrieves them from given path.
#' 
#' In case of no internet connection, run the function, retrieve the URL from the error message, and download the file manually.
#' Alternatively, download the binaries from the release section on github.
#' Copy the file to a local path and give its path via the \code{path} argument.
#' 
#' To install COPASI binaries, you need write access to the packages installation folder.
#'
#' @param path optional file path to COPASI binaries
#' @param force optional bool to force overwriting the binaries
#' @param quiet optional bool to silence messages
#' @export
getCopasi <- function(path = NULL, force = FALSE, quiet = FALSE) {
  assert_that(
    is.null(path) || is.readable(path),
    is.flag(force), noNA(force),
    is.flag(quiet), noNA(quiet)
  )
  
  pkgname <- getPackageName()
  pkgpath <- system.file(package = pkgname)
  
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
    # id_string <- stats::na.omit(stringr::str_match(osfile, "^ID=(.+)$"))
    # v_string <- stats::na.omit(stringr::str_match(osfile, "^VERSION_ID=\\\"(\\d+)\\.(\\d+)\\\"$"))
    # 
    # if (!is_empty(id_string)) id_string <- id_string[1, 2]
    # if (!is_empty(v_string)) v_string <- v_string[1, c(2,3)]
    # 
    # os <- paste0(id_string, "_", paste0(v_string, collapse = "_"))
  }
  arch <- R.Version()$arch
  
  # CHECK CURRENT FILE
  libsdir <- file.path(pkgpath, "libs")
  
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
      msg = "Unsupported platform. Supply COPASI binaries via the `path` argument."
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
    
    dlurl <- dl_url_former(os = os, arch = arch)
    
    dlpath <- tempfile(pattern = "COPASI", fileext = .Platform$dynlib.ext)
    
    # download the binaries
    dlstatus <- utils::download.file(url = dlurl, destfile = dlpath, method = "auto", quiet = quiet, mode = "wb")
    
    assert_that(dlstatus == 0, msg = "Downloading COPASI binaries failed.")
    
    # Check if the hash matches
    assert_that(
      digest::digest(dlpath, algo = "sha256", file = TRUE) == COPASI_BIN_HASHES[[arch]][os],
      msg = "Downloaded COPASI binaries are corrupted."
    )
  } else {
    dlpath <- path
  }
  
  # Try to unload libs if loaded so the binaries can be overwritten
  try(unloadAllModels(), silent = TRUE)
  try(library.dynam.unload("COPASI", pkgpath), silent = TRUE)
  
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
    msg = "Copying COPASI binaries into package folder failed."
  )
  
  # Reload libary
  library.dynam("COPASI", pkgname, file.path(pkgpath, ".."))
  # TODO
  # clearing the deque hides the annoying message about COPASI home directory on linux
  CCopasiMessage_clearDeque()
  if (!quiet)
    message(pkgname, ": Successfully loaded COPASI binaries.")
  
  invisible()
}

#' Get COPASI version numbers
#'
#' \code{getVersion} returns the current version numbers of the COPASI binaries.
#' 
#' @return Numeric vector consisting of the major version, minor version and build numbers.
#' @export
getVersion <- function() {
  assert_binaries()
  
  c_version <- CVersion_VERSION()
  
  c(
    major = c_version$getVersionMajor(),
    minor = c_version$getVersionMinor(),
    build = c_version$getVersionDevel()
  )
}

# check if COPASI lib is loaded.
# default method for error is stop.
# .onLoad uses warning instead.
assert_binaries <- function(method = stop, pkgname = getPackageName()) {
  if (!is.loaded("R_swig_CRootContainer_init", PACKAGE = "COPASI"))
    method(pkgname, ": COPASI binaries are not installed. Use ", pkgname, "::getCopasi() to install them.")
}
