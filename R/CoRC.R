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

# https://github.com/tidyverse/magrittr/issues/29
utils::globalVariables(c("."))

# Output flag for tasks
OUTPUTFLAG <- 119L

COPASI_BIN_VERSION <- 22L
COPASI_BIN_HASHES <- list(
  x86_64 = c(
    darwin  = "e1200b125d69ae85f80d1040809e487039a8cd23e34bd3cf9fe91926771d85dd",
    linux   = "c4bbc9db165405e1aa4736a659ea97fe355c244e611df3846b13757efb402e35",
    windows = "3366a4aee2037412fda3769636ca7d1d3897b95ec382617f90f45f2faa24cc97"
  )
)

COPASI_BIN_BASE_URL_PAHLE <- "http://juergen.pahle.de/CoRC_libs/"
dl_url_former_pahle <- function(base = COPASI_BIN_BASE_URL_PAHLE, version = COPASI_BIN_VERSION, os, arch, ext = .Platform$dynlib.ext) {
  paste0(
    base,
    "v", version, "/",
    "COPASI_", os,
    "_", arch,
    ext
  )
}

COPASI_BIN_BASE_URL_GITHUB <- "https://github.com/jpahle/CoRC/"
dl_url_former_github <- function(base = COPASI_BIN_BASE_URL_GITHUB, version = package_version(getNamespaceVersion(getPackageName())), os, arch, ext = .Platform$dynlib.ext) {
  paste0(
    base,
    "releases/download/",
    "v", version$major, ".", version$minor, ".", version$patchlevel, "/",
    "COPASI_", os,
    "_", arch,
    ext
  )
}

# The DLLInfo of the COAPSI library allows calling the methods
# This gets populated in .onLoad
COPASI <- list(info = new("externalptr"))
class(COPASI) <- "DLLInfo"

# Package environment for persistent options etc
pkg_env <- new.env(parent = emptyenv())
# Variable to keep track of the default c_datamodel
pkg_env$c_curr_dm <- NULL
# List to keep track of loaded models.
# If I keep getting new references from C instead of using this list,
# the user can crash the R session by using unloaded model references
pkg_env$cl_loaded_dms <- list()

.onLoad <- function(libname, pkgname) {
  backports::import(pkgname, c("anyNA", "dir.exists", "lengths"))
  backports::import(pkgname, "hasName", force = TRUE)
  
  load <- function() {
    COPASI <<- library.dynam("COPASI", pkgname, libname)
    # clearing the deque hides the annoying message about COPASI home directory on linux
    CCopasiMessage_clearDeque()
    CJitCompiler_SetJitBufferSize(1024 * 8 * 16)
  }
  
  # if loading COPASI fails, download and try again
  tryCatch(load(), error = function(e) {
    # if this is done during package install, it doesn't hard fail if the arch is unsupported
    # this will allow the install to finish
    # yet, loading on said unsupported arch, will then fail
    if (get_copasi(libname, pkgname, Sys.getenv("R_PACKAGE_NAME") == getPackageName()))
      load()
  })
  
  invisible()
}

.onUnload <- function(libpath) {
  try(library.dynam.unload("COPASI", libpath), silent = TRUE)
  invisible()
}

#' Get COPASI version numbers
#'
#' \code{getVersion} returns the current version numbers of the COPASI binaries.
#' 
#' @return Integer vector consisting of the major version, minor version and build numbers.
#' @export
getVersion <- function() {
  c_version <- CVersion_VERSION()
  
  c(
    major = c_version$getVersionMajor(),
    minor = c_version$getVersionMinor(),
    build = c_version$getVersionDevel()
  )
}

# successor to getCopasi but not exported
# is used primarily (hopefully exclusively) during pkg install
get_copasi <- function(libname, pkgname, is_pkg_install) {
  # R_LIBS <- strsplit(Sys.getenv("R_LIBS"), ";", fixed = TRUE)[[1]]
  # R_PACKAGE_DIR <- Sys.getenv("R_PACKAGE_DIR")
  # R_OSTYPE <- Sys.getenv("R_OSTYPE")
  R_ARCH <- Sys.getenv("R_ARCH")
  R_ARCH_BIN <- Sys.getenv("R_ARCH_BIN")
  COPASI_LIB_PATH <- Sys.getenv("COPASI_LIB_PATH")
  # R_PACKAGE_NAME <- Sys.getenv("R_PACKAGE_NAME")
  
  os <- tolower(Sys.info()["sysname"])
  arch <- R.Version()$arch
  
  print0 <- function(...) cat(..., "\n", sep = "")
  print_env <- function(env) print0(env, " = ", Sys.getenv(env))
  
  print0("platform = ", os, "_", arch)
  
  # print0(pkgname)
  # print0(libname)
  # print0(R_LIBS)
  # print_env("R_PACKAGE_DIR")
  # print_env("R_OSTYPE")
  # print_env("R_ARCH")
  # print_env("R_ARCH_BIN")
  print_env("COPASI_LIB_PATH")
  # print_env("R_PACKAGE_NAME")
  # print0("os = ", os)
  # print0("arch = ", arch)
  
  # print(Sys.getenv())
  
  # R_ARCH
  # windows64 = /x64
  # windows64_32install = /i386
  # ubunut64 = 
  # maxos64 = 
  
  # R_ARCH_BIN
  # windows64 = /x64
  # windows64_32install = /x64
  # ubunut64 = 
  # maxos64 = 
  
  # R_OSTYPE
  # windows64 = windows
  # ubunut64 = unix
  # maxos64 = unix
  
  # Sys.info()["sysname"]
  # windows64 = Windows
  # ubunut64 = linux
  # maxos64 = darwin
  
  # R.Version()$arch
  # windows64 = x86_64
  # ubunut64 = x86_64
  # maxos64 = x86_64
  
  digest <- partial(digest::digest, algo = "sha256", file = TRUE)
  
  libsdir_base <- file.path(libname, pkgname, "libs")
  
  # print0("libsdir_base = ", libsdir_base)
  
  dir_create_if_missing <- function(x, recursive = FALSE) {
    dir.exists(x) || dir.create(x, recursive = recursive)
  }
  
  # fake compatibility with all known arches, so that install is sure to proceed on a multiarch system
  assert_that(dir_create_if_missing(libsdir_base))
  assert_that(dir_create_if_missing(file.path(libsdir_base, "i386")))
  assert_that(dir_create_if_missing(file.path(libsdir_base, "x64")))
  assert_that(dir_create_if_missing(file.path(libsdir_base, R_ARCH_BIN)))
  
  libsdir <- file.path(libsdir_base, R_ARCH)
  
  # print0("libsdir = ", libsdir)
  
  assert_that(dir_create_if_missing(libsdir, recursive = TRUE))
  
  libfile <- file.path(libsdir, paste0("COPASI", .Platform$dynlib.ext))
  lib_hash <- pluck(COPASI_BIN_HASHES, arch, os)
  
  # if no path is given we download the COAPSI libraries
  if (is.null(COPASI_LIB_PATH) || COPASI_LIB_PATH == "" || is.na(COPASI_LIB_PATH)) {
    if (is.null(lib_hash)) {
      msg <- paste0("There are currently no COAPSI libraries available for this platform (", os, "_", arch, ").")
      if (is_pkg_install) {
        cat(msg, "\n")
        return(FALSE)
      } else {
        stop(msg)
      }
    }
    
    # Don't do anything if the current lib file is good.
    if (file.exists(libfile)) {
      current_file_hash <- digest(libfile)
      if (current_file_hash == lib_hash) {
        cat("Keeping present COPASI libraries.\n")
        return(TRUE)
      }
    }
    
    cat("Downloading COPASI libraries for your system.\n")
    
    copasi_lib_path_msg <- paste0("Your platform is ", os, "_", arch, ". Consider providing libraries via `configure.vars` 'COPASI_LIB_PATH'.")
    
    assert_that(
      capabilities(what = "http/ftp"),
      msg = paste0("R doesn't have internet capabilities. Cannot download COPASI libraries. ", copasi_lib_path_msg)
    )
    
    # this was used in some examples. I am not sure whether I need this.
    if (os == "windows" && getRversion() < "3.3.0")
      setInternet2()
    
    dlfun <- quietly(possibly(utils::download.file, otherwise = 1))
    
    dlurl <- dl_url_former_pahle(os = os, arch = arch)
    
    # download the binaries from pahle url first
    dlresult_pahle <- dlfun(url = dlurl, destfile = libfile, method = "auto", quiet = FALSE, mode = "wb")
    dlsuccess_pahle <- dlresult_pahle$result == 0
    
    dl_is_valid <- FALSE
    
    if (dlsuccess_pahle)
      dl_is_valid <- digest(libfile) == lib_hash
    
    if (!dl_is_valid) {
      dlurl <- dl_url_former_github(os = os, arch = arch)
      
      # download the binaries from github
      dlresult_github <- dlfun(url = dlurl, destfile = libfile, method = "auto", quiet = FALSE, mode = "wb")
      dlsuccess_github <- dlresult_github$result == 0
      
      if (dlsuccess_github)
        dl_is_valid <- digest(libfile) == lib_hash
    }
    
    if (!dlsuccess_pahle && !dlsuccess_github) {
      warning(dlresult_pahle$warnings)
      warning(dlresult_github$warnings)
      stop("Downloading the COPASI libraries failed. ", copasi_lib_path_msg)
    }
    
    # Check if the hash matches
    assert_that(
      dl_is_valid,
      msg = paste0("Downloaded COPASI libraries are corrupted. Best try again. ", copasi_lib_path_msg)
    )
  } else {
    cat("Including externally supplied COPASI libraries.\n")
    if (!is.null(lib_hash) && lib_hash != digest(COPASI_LIB_PATH))
      warning("The externally supplied COPASI libraries do not conform to expected libaries. Please ensure that the correct libraries were supplied. Your platform is ", os, "_", arch, ".")
    
    assert_that(file.copy(COPASI_LIB_PATH, libfile, overwrite = TRUE))
  }
  
  return(TRUE)
}
