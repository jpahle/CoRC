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
COPASI_VERSION <- "https://github.com/copasi/COPASI/commit/48626220afe9a1bad4cb3e16031efe723b1d71fc"

COPASI_BIN_VERSION <- 5L
COPASI_BIN_HASHES <-
  list(
    "3.3" =
      c(
        # darwin = ""
        # win = "",
        # ubuntu_16_10 = ""
      ),
    "3.4" =
      c(
        darwin = "e3a5960f96522562038f41a0914a4b7c942e6b80641716d34ef0ac85014bf0cf",
        win = "93abbe8eb28f491d61a713e4229733002cdfdf55503a6dc923b47aacbb443cec",
        ubuntu_16_04 = "ba3cb84e1c50465e5d02a10a8a8c4ddf7d1d7c8b6bcd29b3d0f1ba2d37e64ef0",
        ubuntu_16_10 = "ba3cb84e1c50465e5d02a10a8a8c4ddf7d1d7c8b6bcd29b3d0f1ba2d37e64ef0",
        ubuntu_17_04 = "ba3cb84e1c50465e5d02a10a8a8c4ddf7d1d7c8b6bcd29b3d0f1ba2d37e64ef0"
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
  version <- R.Version()
  
  # if no path is given we download the binaries
  if (is.null(path)) {
    dlurl <- "http://juergen.pahle.de/CoRC_libs/"
    
    r_version <- paste0(version$major, ".", strsplit(version$minor, ".", fixed = TRUE)[[1]][1])
    assert_that(r_version %in% names(COPASI_BIN_HASHES), msg = paste0("Versions ", r_version, "(.x) of R are not supported."))
    
    dlurl <- paste0(dlurl, r_version, "/")
    
    dlurl <- paste0(dlurl, "v", COPASI_BIN_VERSION, "/")
    
    x64folder <- FALSE
    if (.Platform$OS.type == "windows") {
      platform <- "win"
      x64folder <- TRUE
    } else if (substr(version$os, 1L, 6L) == "darwin") {
      platform <- "darwin"
    } else {
      # Gather data about os (created for ubuntu but may work for other unix os)
      id_string <- character()
      v_string <- character()
      if (file.exists("/etc/os-release")) {
        osfile <- readLines("/etc/os-release")
        id_string = na.omit(stringr::str_match(osfile, "^ID=(.+)$"))
        v_string = na.omit(stringr::str_match(osfile, "^VERSION_ID=\\\"(\\d+)\\.(\\d+)\\\"$"))
        if (!is_empty(id_string)) id_string <- id_string[1, 2]
        if (!is_empty(v_string)) v_string <- v_string[1, c(2,3)]
      }
      platform <- paste0(id_string, "_", paste0(v_string, collapse = "_"))
      
      assert_that(
        platform %in% names(COPASI_BIN_HASHES[[r_version]]),
        msg = paste0("There are currently no binaries available for your platform (", platform, ").")
      )
    }
    
    dlurl <- paste0(dlurl, "COPASI_", platform)
    
    assert_that(version$arch == "x86_64")
    dlurl <- paste0(dlurl, "_x64")
    
    dlurl <- paste0(dlurl, .Platform$dynlib.ext)
    
    path <- tempfile(pattern = "COPASI", fileext = .Platform$dynlib.ext)
    
    # download the binaries
    dlstatus <- download.file(url = dlurl, destfile = path, method = "auto", mode = "wb")
    
    assert_that(dlstatus == 0, msg = "Downloading copasi binaries failed.")
    
    # Check if the hash matches
    assert_that(
      digest::digest(path, algo = "sha256", file = TRUE) == COPASI_BIN_HASHES[[r_version]][[platform]],
      msg = "Downloaded copasi binaries are corrupted."
    )
  }
  
  libsdir <- file.path(system.file(package = getPackageName()), "libs")
  
  # It seems in some cases a x64 folder is needed inside libs but I am unsure when and if it needs to contain files.
  if (x64folder && version$arch == "x86_64")
    libsdir <- file.path(libsdir, "x64")
  
  success <- TRUE
  if (!dir.exists(libsdir))
    success <- dir.create(libsdir, recursive = TRUE)
  
  # Try to unload COPASI if loaded so the binaries can be overwritten
  try(library.dynam.unload("COPASI", system.file(package = getPackageName())), silent = TRUE)
  
  # Copy file into package folder
  if (success)
    success <- file.copy(path, file.path(libsdir, paste0("COPASI", .Platform$dynlib.ext)), overwrite = TRUE)

  assert_that(success, msg = "Copying copasi binaries into package folder failed.")
  
  # Reload libary
  library.dynam("COPASI", getPackageName(), .libPaths())
  message(getPackageName(), ": Successfully loaded copasi binaries.")
}

assert_binaries <- function(method = stop, pkgname = getPackageName()) {
  if (!("COPASI" %in% map_chr(.dynLibs(), "name")))
    method(pkgname, ": Copasi binaries are not installed. Use ", pkgname, "::getCopasi() to install them.")
}
