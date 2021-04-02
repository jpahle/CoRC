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
  
  library.dynam("COPASI", pkgname, libname)
  # clearing the deque hides the annoying message about COPASI home directory on linux
  CCopasiMessage_clearDeque()
}

.onUnload <- function(libpath) {
  try(library.dynam.unload("COPASI", libpath), silent = TRUE)
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
