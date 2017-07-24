#' Get the currently active model
#'
#' \code{getCurrentModel} returns the currently active model.
#'
#' @return a model object
#' @export
getCurrentModel <- function() {
  assert_that(!is.null(pkg_env$curr_dm), msg = "No model currently in use.")

  pkg_env$curr_dm
}

#' Set the currently active model
#'
#' \code{setCurrentModel} sets the given model as the currently active model.
#'
#' @param datamodel a model object
#' @return invisibly returns the model object
#' @export
setCurrentModel <- function(datamodel) {
  assert_datamodel(datamodel)

  pkg_env$curr_dm <- datamodel
  
  invisible(datamodel)
}

#' Get a list of loaded models
#'
#' \code{getLoadedModels} returns a list of all loaded models.
#'
#' @return a list of model objects
#' @export
getLoadedModels <- function() {
  dm_list <- CRootContainer_getDatamodelList()

  get_cdv(dm_list)
}

# helper for loading models from urls
url_to_string <- function(x) {
  con <- try(url(x), silent = TRUE)
  if (is.error(con)) return()
  
  result <- quietly(readLines)(con)
  close(con)
  
  assert_that(is_empty(result$warnings), msg = "Could not interpret url contents.")
  
  paste0(result$result, collapse = "\n")
}

#' Load a model
#'
#' \code{loadModel} loads a model into copasi and returns a reference to it.
#'
#' @param path url or path
#' @return a model object
#' @export
loadModel <- function(path) {
  assert_that(is.string(path))
  
  datamodel <- CRootContainer_addDatamodel()
  
  model <- url_to_string(path)
  if (!is_null(model)) {
    success <- grab_msg(datamodel$loadModelFromString(model),
                        purge = "The content is created with a newer version .* of COPASI.")
  } else {
    assert_that(is.readable(path))
    
    success <- grab_msg(datamodel$loadModel(normalizePathC(path)),
                        purge = "The content is created with a newer version .* of COPASI.")
  }
  
  if (!success) {
    CRootContainer_removeDatamodel(datamodel)
    stop("Couldn't load model file.")
  }
  
  pkg_env$curr_dm <- datamodel
  
  invisible(datamodel)
}

#' Load SBML data
#'
#' \code{loadSBML} loads SBML data into copasi and returns a reference to it.
#'
#' @param path url or path
#' @return a model object
#' @export
loadSBML <- function(path) {
  assert_that(is.string(path))
  
  datamodel <- CRootContainer_addDatamodel()
  
  sbml <- url_to_string(path)
  if (!is_null(sbml)) {
    success <- grab_msg(datamodel$importSBMLFromString(sbml))
  } else {
    assert_that(is.readable(path))
    
    success <- grab_msg(datamodel$importSBML(normalizePathC(path)))
  }
  
  if (!success) {
    CRootContainer_removeDatamodel(datamodel)
    stop("Couldn't load SBML data.")
  }
  
  pkg_env$curr_dm <- datamodel
  
  invisible(datamodel)
}

#' Unload a model
#'
#' \code{unloadModel} frees memory by unloading a model from copasi
#'
#' @param datamodel a model object
#' @export
unloadModel <- function(datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  
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

  get_cdv(dm_list) %>%
    walk(~ CRootContainer_removeDatamodel(.x))

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
  assert_datamodel(datamodel)
  assert_that(is.string(filename))

  if (!has_extension(filename, "cps"))
    filename <- paste0(filename, ".cps")
  
  if (file.exists(filename)) {
    assert_that(overwrite, msg = paste0('File: \"', filename, '\" already exists and overwrite is set to FALSE.'))
    filepath <- normalizePathC(filename)
  } else {
    filepath <- file.path(normalizePathC(dirname(filename)), basename(filename))
  }
  
  success <- grab_msg(datamodel$saveModel(filepath, overwriteFile = overwrite))

  if (!success) {
    stop('Model failed to save at: "', filename, '".')
  }

  invisible(success)
}

#' Load example models
#'
#' \code{loadExamples} loads several example models and returns them in a list.
#'
#' @param indices optional indices of example models to load.
#' @return a list of model objects
#' @export
loadExamples <- function(indices = NULL) {
  models <- 
    c(
      "brusselator.cps",
      "chemotaxis_4.cps",
      "multicomp.cps",
      "simple.cps"
    )
  
  assert_that(is.null(indices) || all(indices %in% seq_along(models)), msg = "Invalid indices.")
  
  if (!is.null(indices)) models <- models[indices]
  
  pkgname <- getPackageName()
  map(models, ~ loadModel(system.file("extdata", .x, package = pkgname)))
}

#' Open the given model in the copasi UI
#'
#' @param readin if TRUE, the function waits for Copasi to quit and then reads in the temporary model file, overwriting the give datamodel
#' @param copasi_loc location of CopasiUI
#' @param datamodel a model object
#' @export
openCopasi <- function(readin = FALSE, copasi_loc = "CopasiUI", datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  assert_that(
    is.flag(readin) && !is.na(readin),
    is.string(copasi_loc) && !is.na(copasi_loc)
  )
  
  if (.Platform$OS.type == "windows") {
    found <- !suppressWarnings(system2("where", args = copasi_loc, stdout = FALSE, stderr = FALSE))
    
    # where command can't find executables with full paths. Just test if readable for now.
    if (!found && !missing(copasi_loc)) found <- is.readable(copasi_loc)
  } else if (.Platform$OS.type == "unix") {
    # On darwin, CopasiUI doesn't seem to be in path so try a default location if copasi_loc wasn't given.
    if (substr(version$os, 1L, 6L) == "darwin" && system2("which", args = c("-s", copasi_loc)) && missing(copasi_loc)) copasi_loc <- "/Applications/COPASI/CopasiUI.app/Contents/MacOS/CopasiUI"
    
    found <- !system2("which", args = c("-s", copasi_loc))
  }
  
  assert_that(
    found,
    msg = "Could not find CopasiUI."
  )
  
  # Create a temp file for the model to open in the UI
  # This potentially could cause issues if it is possible for temp files to have spaces in its path on windows
  file <- tempfile(fileext = ".cps")
  grab_msg(datamodel$saveModel(file, overwriteFile = TRUE))
  
  if (readin) {
    if (.Platform$OS.type == "windows")
      system2(copasi_loc, file, wait = TRUE, invisible = FALSE)
    else
      system2(copasi_loc, file, wait = TRUE)
    
    grab_msg(datamodel$loadModel(file))
    file.remove(file)
  } else {
    if (.Platform$OS.type == "windows")
      system2(copasi_loc, file, wait = FALSE, invisible = FALSE)
    else
      system2(copasi_loc, file, wait = FALSE)
  }
  
  invisible()
}
