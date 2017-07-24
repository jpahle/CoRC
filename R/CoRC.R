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
#' @importFrom rlang .data
#' @importFrom utils head tail
"_PACKAGE"

# Output flag for tasks
OUTPUTFLAG <- 119L

#' @export
COPASI_VERSION <- "https://github.com/copasi/COPASI/commit/ed1bee159a3eb7b21010d9a0ae61be9d3b411959"

COPASI_BIN_VERSION <- 1L
COPASI_BIN_HASHES <-
  c(
    win = "36066727e4e184daa3f9d97331b976c0e8350db219a6ddc6babee18a11ad03c0",
    darwin = "bbea29ab57c4616e808ca4e5230b0d18f2316253c8e041167015975a994600cd",
    ubuntu_16_10 = "22392cd755921b94d611cb25e1d8ae2d5acc295f5af7e2d89636103c37f0370a"
  )

.onLoad <- function(libname, pkgname) {
  # hack for load_all() 
  libname <- .libPaths()
  
  e <- try(library.dynam("COPASI", pkgname, libname), silent = TRUE)
  if (is.error(e)) warning(pkgname, ": Copasi binaries are not installed. Use ", pkgname, "::getCopasi() to install them.")
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
#' @export
getCopasi <- function(path = NULL) {
  assert_that(is.null(path) || is.readable(path))
  
  # if no path is given we download the binaries
  if (is_null(path)) {
    dlurl <- "http://juergen.pahle.de/CoRC_libs/"
    dlurl <- paste0(dlurl, "v", floor(COPASI_BIN_VERSION / 10), COPASI_BIN_VERSION %% 10L, "/")
    
    x64folder <- FALSE
    if (.Platform$OS.type == "windows") {
      platform <- "win"
      x64folder <- TRUE
    } else if (substr(version$os, 1L, 6L) == "darwin") {
      platform <- "darwin"
    } else {
      # Gather data about os (created for ubuntu but may work for other unix os)
      id = character()
      version = character()
      if (file.exists("/etc/os-release")) {
        osfile <- readLines("/etc/os-release")
        id = na.omit(stringr::str_match(osfile, "^ID=(.+)$"))
        version = na.omit(stringr::str_match(osfile, "^VERSION_ID=\\\"(\\d+)\\.(\\d+)\\\"$"))
        if (!is_empty(id)) id <- id[1,2]
        if (!is_empty(version)) version <- version[1,c(2,3)]
      }
      platform <- paste0(id, "_", paste0(version, collapse = "_"))
      
      assert_that(
        platform %in% names(COPASI_BIN_HASHES),
        msg == "There are currently no binaries available for your platform (", platform, ")."
      )
    }
    
    dlurl <- paste0(dlurl, "COPASI_", platform)
    
    assert_that(version$arch == "x86_64")
    dlurl <- paste0(dlurl, "_x64")
    
    dlurl <- paste0(dlurl, .Platform$dynlib.ext)
    
    path <- tempfile(pattern = "COPASI", fileext = .Platform$dynlib.ext)
    
    # download the binaries
    success = download.file(url = dlurl, destfile = path, method = "auto", mode = "wb")
    
    assert_that(success == 0, msg = "Downloading copasi binaries failed.")
    
    # Check if the hash matches
    assert_that(
      digest::digest(path, algo = "sha256", file = TRUE) == COPASI_BIN_HASHES[[platform]],
      msg = "Downloaded copasi binaries are corrupted."
    )
  }
  
  libsdir <- file.path(system.file(package = getPackageName()), "libs")
  
  # It seems in some cases a x64 folder is needed inside libs but I am unsure when and if it needs to contain files.
  if (x64folder && version$arch == "x86_64") libsdir <- file.path(libsdir, "x64")
  
  success <- TRUE
  if (!dir.exists(libsdir)) success <- dir.create(libsdir, recursive = TRUE)
  
  # Try to unload COPASI if loaded so the binaries can be overwritten
  try(library.dynam.unload("COPASI", system.file(package = getPackageName())), silent = TRUE)
  
  # Copy file into package folder
  if (success) success <- file.copy(path, file.path(libsdir, paste0("COPASI", .Platform$dynlib.ext)), overwrite = TRUE)

  assert_that(success, msg = "Copying copasi binaries into package folder failed.")
  
  # Reload libary
  library.dynam("COPASI", getPackageName(), .libPaths())
  message(getPackageName(), ": Successfully loaded copasi binaries.")
}
