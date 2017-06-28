#' Get the currently active model
#'
#' \code{getCurrentModel} returns the currently active model.
#'
#' @return a model object
#' @export
getCurrentModel <- function() {
  assert_that(!is_null(pkg_env$curr_dm), msg = "No model currently in use.")

  pkg_env$curr_dm
}

#' Set the currently active model
#'
#' \code{setCurrentModel} sets the given model as the currently active model.
#'
#' @param datamodel a model object
#' @export
setCurrentModel <- function(datamodel) {
  assert_that(confirmDatamodel(datamodel))

  pkg_env$curr_dm <- datamodel
}

#' Get a list of loaded models
#'
#' \code{getLoadedModels} returns a list of all loaded models.
#'
#' @return a list of model objects
#' @export
getLoadedModels <- function() {
  dm_list <- CRootContainer_getDatamodelList()

  seq_along_cv(dm_list) %>%
    map(~ get_from_cv(dm_list, .x))
}

#' Load a model
#'
#' \code{loadModel} loads a model into copasi and returns a reference to it.
#'
#' @param filename path to model
#' @return a model object
#' @export
loadModel <- function(filename) {
  assert_that(assertthat::is.readable(filename))

  datamodel <- CRootContainer_addDatamodel()
  success <- datamodel$loadModel(normalizePath(filename))

  if (!success) {
    CRootContainer_removeDatamodel(datamodel)
    stop("Couldn't load model file.")
  }

  pkg_env$curr_dm <- datamodel
  datamodel
}

#' Unload a model
#'
#' \code{unloadModel} frees memory by unloading a model from copasi
#'
#' @param datamodel a model object
#' @export
unloadModel <- function(datamodel = pkg_env$curr_dm) {
  assert_that(confirmDatamodel(datamodel))

  datamodel <- CRootContainer_removeDatamodel(datamodel)

  pkg_env$curr_dm <- NULL
}

#' Unload all loaded models
#'
#' \code{unloadAllModels} frees memory by unloading all loaded models from copasi
#'
#' @export
unloadAllModels <- function() {
  dm_list <- CRootContainer_getDatamodelList()

  seq_along_cv(dm_list) %>%
    walk(~ CRootContainer_removeDatamodel(get_from_cv(dm_list, 0)))

  pkg_env$curr_dm <- NULL
}

#' Save a model as a .cps file
#'
#' \code{saveCPS} saves a model as a .cps file
#'
#' @param filename a path to save to
#' @param overwrite is overwriting existing files allowed?
#' @param datamodel a model object
#' @export
saveCPS <- function(filename = datamodel$getFileName(), overwrite = FALSE, datamodel = pkg_env$curr_dm) {
  assert_that(confirmDatamodel(datamodel), is_scalar_character(filename))

  if (!assertthat::has_extension(filename, "cps")) {
    filename <- paste0(filename, ".cps")
  }

  success <- datamodel$saveModel(normalizePath(filename), overwriteFile = overwrite)

  assert_that(success, msg = paste0("Model failed to save at: ", filename))

  invisible(success)
}

#' Open the given model in the Copasi UI
#'
#' @param readin if TRUE, the function waits for Copasi to quit and then reads in the temporary model file, overwriting the give datamodel
#' @param copasi_loc location of CopasiUI
#' @param datamodel a model object
#' @export
openCopasi <- function(readin = FALSE, copasi_loc = "CopasiUI", datamodel = pkg_env$curr_dm) {
  assert_that(confirmDatamodel(datamodel), is_scalar_logical(readin), is_scalar_character(copasi_loc))
  
  if (.Platform$OS.type == "windows") {
    found <- !suppressWarnings(system2("where", args = copasi_loc, stdout = FALSE, stderr = FALSE))
    
    # where command can't find executables with full paths. Just test if readable for now.
    if (!found && !missing(copasi_loc)) found <- assertthat::is.readable(copasi_loc)
  } else if (.Platform$OS.type == "unix") {
    # On darwin, CopasiUI doesn't seem to be in path so try a default location if copasi_loc wasn't given.
    if (substr(version$os, 1, 6) == "darwin" && system2("which", args = c("-s", copasi_loc)) && missing(copasi_loc)) copasi_loc <- "/Applications/COPASI/CopasiUI.app/Contents/MacOS/CopasiUI"
    
    found <- !system2("which", args = c("-s", copasi_loc))
  }
  
  assert_that(
    found,
    msg = "Could not find CopasiUI."
  )
  
  file <- tempfile(fileext = ".cps")
  datamodel$saveModel(file, overwriteFile = TRUE)

  if (readin) {
    if (.Platform$OS.type == "windows")
      system2(copasi_loc, file, wait = TRUE, invisible = FALSE)
    else
      system2(copasi_loc, file, wait = TRUE)
    
    datamodel$loadModel(file)
    file.remove(file)
  } else {
    if (.Platform$OS.type == "windows")
      system2(copasi_loc, file, wait = FALSE, invisible = FALSE)
    else
      system2(copasi_loc, file, wait = FALSE)
  }
  
  invisible()
}

#' Load example models
#'
#' \code{loadExamples} loads several example models and returns them in a list.
#'
#' @return a list of model objects
#' @export
loadExamples <- function() {
  list(
    brusselator = loadModel(system.file("extdata", "brusselator.cps", package = "CoRC")),
    chemotaxis = loadModel(system.file("extdata", "chemotaxis_4.cps", package = "CoRC")),
    multicomp = loadModel(system.file("extdata", "multicomp.cps", package = "CoRC"))
  )
}
