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
COPASI_VERSION <- "https://github.com/copasi/COPASI/commit/61123de94942748e873f2b3eb2cec327c88cd55c"

COPASI_BIN_VERSION <- 2L
COPASI_BIN_HASHES <-
  list(
    "3.3" =
      c(
        darwin = "521ebcf2441f86cf2c4d977dcca1e80a5d1c92571ba9c7e66210f9c2a25f6225"
        # win = "",
        # ubuntu_16_10 = ""
      ),
    "3.4" =
      c(
        darwin = "ac390a6aa89197f53d640148225384a8396690954f09b7325b26930bbe2206e7"
        # win = "",
        # ubuntu_16_10 = ""
      )
  )

.onLoad <- function(libname, pkgname) {
  # hack for load_all() 
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
#' @export
getCopasi <- function(path = NULL) {
  assert_that(is.null(path) || is.readable(path))
  
  # if no path is given we download the binaries
  if (is_null(path)) {
    dlurl <- "http://juergen.pahle.de/CoRC_libs/"
    dlurl <- paste0(dlurl, "v", COPASI_BIN_VERSION, "/")
    
    r_version <- paste0(version$major, ".", strsplit(version$minor, ".", fixed = TRUE)[[1]][1])
    assert_that(rversion %in% names(COPASI_BIN_HASHES), msg = paste0("Versions ", r_version, "(.x) of R are not supported."))
    
    dlurl <- paste0(dlurl, r_version, "/")
    
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
      digest::digest(path, algo = "sha256", file = TRUE) == COPASI_BIN_HASHES[[r_version]][[platform]],
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

assert_binaries <- function(method = stop, pkgname = getPackageName()) {
  if (!("COPASI" %in% map(.dynLibs(), "name")))
    method(pkgname, ": Copasi binaries are not installed. Use ", pkgname, "::getCopasi() to install them.")
}

# FIXES FOR OLD VERSION OF PURRR
map_dfr <- map_df
modify_if <- map_if
iwalk <- function (.x, .f, ...) {
  .f <- suppressWarnings(as_function(.f, ...))
  walk2(.x, seq_along(.x), .f, ...)
}
has_element <- function (.x, .y) {
  some(.x, identical, .y)
}
