# helper to remove models that have NULL pointers
discard_unloaded_models <- function() {
  pkg_env$cl_loaded_dms <-
    pkg_env$cl_loaded_dms %>%
    discard(map_lgl(., has_null_pointer))
}

#' Get the currently active model
#'
#' \code{getCurrentModel} returns the currently active model.
#' 
#' By default, the currently active model is the last model that has been used.
#' If working with a single model, the \code{getCurrentModel} function will supply this model to any function call including the \code{model} argument.
#' This makes the \code{model} argument used by most of this packages functions safe to ignore in most use cases.
#'
#' @return a model object
#' @family model handlers
#' @export
getCurrentModel <- function() {
  c_curr_dm <- pkg_env$c_curr_dm 
  
  assert_that(
    !is.null(c_curr_dm),
    # There might be a case where a NULL pointer is here because of saving and loading workspaces?
    !has_null_pointer(c_curr_dm),
    msg = "No model currently in use."
  )

  c_curr_dm
}

#' Set the currently active model
#'
#' \code{setCurrentModel} sets the given model as the currently active model.
#'
#' @param model a model object
#' @return invisibly returns the model object
#' @family model handlers
#' @export
setCurrentModel <- function(model) {
  c_datamodel <- assert_datamodel(model)
  
  pkg_env$c_curr_dm <- c_datamodel
  
  invisible(c_datamodel)
}

#' Get a list of loaded models
#'
#' \code{getLoadedModels} returns a list of all loaded models.
#'
#' @return a list of model objects
#' @family model handlers
#' @export
getLoadedModels <- function() {
  discard_unloaded_models()
  
  # get_cdv(CRootContainer_getDatamodelList())
  pkg_env$cl_loaded_dms
}

# helper for loading models from connections
# If x is a readable path, the contents are returned as string
con_to_string <- function(x) {
  result <- readr::read_file(x)
  
  # check if e.g. binary file
  assert_that(validEnc(result), msg = "Could not interpret path contents.")

  assert_that(nchar(result) > 0, msg = "Path contents are empty.")
  
  result
}

#' Load an empty model
#'
#' \code{newModel} creates a new model returns a reference to it.
#'
#' @return a model object
#' @export
#' @family model loading
newModel <- function() {
  assert_binaries()
  
  c_datamodel <- CRootContainer_addDatamodel()
  
  pkg_env$c_curr_dm <- c_datamodel
  pkg_env$cl_loaded_dms <- append(pkg_env$cl_loaded_dms, c_datamodel)
  
  c_datamodel
}

#' Load a model
#'
#' \code{loadModel} loads a model into COPASI and returns a reference to it.
#'
#' @param path url or path
#' @return a model object
#' @family model loading
#' @export
loadModel <- function(path) {
  assert_binaries()
  assert_that(inherits(path, "connection") || is.string(path) && noNA(path))
  
  c_datamodel <- CRootContainer_addDatamodel()
  
  success <- FALSE
  
  if (is.character(path) && file.exists(path) && is.readable(path)) {
    success <- grab_msg(c_datamodel$loadModel(normalizePathC(path)))
  }
  
  if (!success) {
    model_str <- con_to_string(path)
    success <- grab_msg(c_datamodel$loadModelFromString(model_str, normalizePathC(getwd())))
  }
  
  if (!success) {
    CRootContainer_removeDatamodel(c_datamodel)
    stop("Failed to load model.")
  }
  
  pkg_env$c_curr_dm <- c_datamodel
  pkg_env$cl_loaded_dms <- append(pkg_env$cl_loaded_dms, c_datamodel)
  
  c_datamodel
}

#' Load a model from a string
#'
#' \code{loadModelFromString} loads a model from a string.
#'
#' @param model string
#' @family model loading
#' @export
loadModelFromString <- function(model) {
  assert_binaries()
  assert_that(is.string(model), noNA(model))
  
  c_datamodel <- CRootContainer_addDatamodel()
  
  success <- grab_msg(c_datamodel$loadModelFromString(model, normalizePathC(getwd())))
  
  if (!success) {
    CRootContainer_removeDatamodel(c_datamodel)
    stop("Failed to load model.")
  }
  
  pkg_env$c_curr_dm <- c_datamodel
  pkg_env$cl_loaded_dms <- append(pkg_env$cl_loaded_dms, c_datamodel)
  
  c_datamodel
}

#' Load an SBML model
#'
#' \code{loadSBML} loads an SBML model into COPASI and returns a reference to it.
#'
#' @param path url or path
#' @return a model object
#' @family model loading
#' @export
loadSBML <- function(path) {
  assert_binaries()
  assert_that(inherits(path, "connection") || is.string(path) && noNA(path))
  
  c_datamodel <- CRootContainer_addDatamodel()
  
  success <- FALSE
  
  if (is.character(path) && file.exists(path) && is.readable(path)) {
    success <- grab_msg(c_datamodel$importSBML(normalizePathC(path)))
  }
  
  if (!success) {
    sbml_str <- con_to_string(path)
    success <- grab_msg(c_datamodel$importSBMLFromString(sbml_str))
  }
  
  # failed imports sometimes generate NULL instead of FALSE
  if (is.null(success) || !success) {
    CRootContainer_removeDatamodel(c_datamodel)
    stop("Failed to load SBML model.")
  }
  
  pkg_env$c_curr_dm <- c_datamodel
  pkg_env$cl_loaded_dms <- append(pkg_env$cl_loaded_dms, c_datamodel)
  
  c_datamodel
}

#' Load an SBML model from a string
#'
#' \code{loadSBMLFromString} loads an SBML model from a string.
#'
#' @param sbml string
#' @family model loading
#' @export
loadSBMLFromString <- function(sbml) {
  assert_binaries()
  assert_that(is.string(sbml), noNA(sbml))
  
  c_datamodel <- CRootContainer_addDatamodel()
  
  success <- grab_msg(c_datamodel$importSBMLFromString(sbml))
  
  # failed imports sometimes generate NULL instead of FALSE
  if (is.null(success) || !success) {
    CRootContainer_removeDatamodel(c_datamodel)
    stop("Failed to load SBML model.")
  }
  
  pkg_env$c_curr_dm <- c_datamodel
  pkg_env$cl_loaded_dms <- append(pkg_env$cl_loaded_dms, c_datamodel)
  
  c_datamodel
}

#' Load a combine archive
#'
#' \code{loadCombineArchive} loads a combine archive into COPASI and returns a reference to it.
#'
#' @param path path
#' @return a model object
#' @family model loading
#' @export
loadCombineArchive <- function(path) {
  assert_binaries()
  assert_that(is.string(path), noNA(path), file.exists(path), is.readable(path))
  
  c_datamodel <- CRootContainer_addDatamodel()
  
  success <- grab_msg(c_datamodel$openCombineArchive(normalizePathC(path)))
  
  # failed imports sometimes generate NULL instead of FALSE
  if (is.null(success) || !success) {
    CRootContainer_removeDatamodel(c_datamodel)
    stop("Failed to load combine archive.")
  }
  
  pkg_env$c_curr_dm <- c_datamodel
  pkg_env$cl_loaded_dms <- append(pkg_env$cl_loaded_dms, c_datamodel)
  
  c_datamodel
}

#' Unload a model
#'
#' \code{unloadModel} frees memory by unloading the given model from COPASI.
#'
#' @param model a model object
#' @family model loading
#' @export
unloadModel <- function(model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  # CRootContainer_removeDatamodel(c_datamodel)
  delete(c_datamodel)
  
  discard_unloaded_models()
  pkg_env$c_curr_dm <- NULL
  
  invisible()
}

#' Unload all loaded models
#'
#' \code{unloadAllModels} frees memory by unloading all loaded models from COPASI.
#'
#' @family model loading
#' @export
unloadAllModels <- function() {
  discard_unloaded_models()
  
  pkg_env$cl_loaded_dms %>%
    # walk(~ CRootContainer_removeDatamodel(.x))
    walk(delete)
  
  pkg_env$cl_loaded_dms <- list()
  pkg_env$c_curr_dm <- NULL
  
  invisible()
}

#' Save the model as a .cps file
#'
#' \code{saveModel} saves the given model as a .cps file
#'
#' @param filename a path to save to
#' @param overwrite is overwriting existing files allowed?
#' @param model a model object
#' @family model loading
#' @export
saveModel <- function(filename = model$getFileName(), overwrite = FALSE, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(
    is.string(filename), noNA(filename), filename != "",
    is.flag(overwrite), noNA(overwrite)
  )

  # if (!has_extension(filename, "cps"))
  #   filename <- paste0(filename, ".cps")
  
  if (file.exists(filename)) {
    assert_that(
      overwrite,
      msg = paste0("File `", filename, "` already exists and overwrite is set to FALSE.")
    )
    filepath <- normalizePathC(filename)
  } else {
    filepath <- file.path(normalizePathC(dirname(filename)), basename(filename))
  }
  
  assert_that(
    grab_msg(c_datamodel$saveModel(filepath, overwriteFile = overwrite)),
    msg = paste0("Model failed to save at `", filename, "`.")
  )
  
  invisible()
}

#' Save the model to string
#'
#' \code{saveModelToString} returns the given model as a string.
#'
#' @param model a model object
#' @return The COPASI model file, as string.
#' @family model loading
#' @export
saveModelToString <- function(model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  grab_msg(c_datamodel$saveModelToString())
}

#' Save the model as a SBML file
#'
#' \code{saveSBML} exports the given model as a SBML .xml file
#'
#' @param filename a path to save to
#' @param level numeric sbml level
#' @param version numeric sbml version
#' @param overwrite is overwriting existing files allowed?
#' @param model a model object
#' @family model loading
#' @export
saveSBML <- function(filename, level, version, overwrite = FALSE, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(
    is.string(filename), noNA(filename), filename != "",
    is.count(level),
    is.count(version),
    is.flag(overwrite), noNA(overwrite)
  )
  
  # if (!has_extension(filename, "xml"))
  #   filename <- paste0(filename, ".xml")
  
  if (file.exists(filename)) {
    assert_that(
      overwrite,
      msg = paste0("File `", filename, "` already exists and overwrite is set to FALSE.")
    )
    filepath <- normalizePathC(filename)
  } else {
    filepath <- file.path(normalizePathC(dirname(filename)), basename(filename))
  }
  
  assert_that(
    isTRUE(grab_msg(c_datamodel$exportSBML(filepath, overwriteFile = overwrite, sbmlLevel = level, sbmlVersion = version))),
    msg = paste0("Model failed to save at `", filename, "`.")
  )
  
  invisible()
}

#' Save the model to an SBML string
#'
#' \code{saveSBMLToString} returns the given model as an SBML string.
#'
#' @param level numeric sbml level
#' @param version numeric sbml version
#' @param model a model object
#' @return The model SBML file, as string.
#' @family model loading
#' @export
saveSBMLToString <- function(level, version, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(
    is.count(level),
    is.count(version)
  )
  
  grab_msg(c_datamodel$exportSBMLToString(sbmlLevel = level, sbmlVersion = version))
}

#' Save the model as a combine archive
#'
#' \code{saveCombineArchive} exports the given model as a combine archive .omex file.
#'
#' @param filename a path to save to
#' @param include_copasi flag
#' @param include_sbml flag
#' @param include_data flag
#' @param include_sedml flag
#' @param overwrite is overwriting existing files allowed?
#' @param model a model object
#' @family model loading
#' @export
saveCombineArchive <- function(filename, include_copasi = TRUE, include_sbml = TRUE, include_data = TRUE, include_sedml = TRUE, overwrite = FALSE, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(
    is.string(filename), noNA(filename), filename != "",
    is.flag(include_copasi), noNA(include_copasi),
    is.flag(include_sbml), noNA(include_sbml),
    is.flag(include_data), noNA(include_data),
    is.flag(include_sedml), noNA(include_sedml),
    is.flag(overwrite), noNA(overwrite)
  )
  
  if (file.exists(filename)) {
    assert_that(
      overwrite,
      msg = paste0("File `", filename, "` already exists and overwrite is set to FALSE.")
    )
    filepath <- normalizePathC(filename)
  } else {
    filepath <- file.path(normalizePathC(dirname(filename)), basename(filename))
  }
  
  assert_that(
    # somehow, this function returns FALSE
    isFALSE(grab_msg(c_datamodel$exportCombineArchive(
      filepath,
      includeCOPASI = include_copasi,
      includeSBML = include_sbml,
      includeData = include_data,
      includeSEDML = include_sedml,
      overwriteFile = overwrite
    ))),
    msg = paste0("Model failed to save at `", filename, "`.")
  )
  
  invisible()
}

#' Load example models
#'
#' \code{loadExamples} loads several example models and returns them in a list.
#'
#' @param indices optional indices of example models to load.
#' @return a list of model objects
#' @family model loading
#' @export
loadExamples <- function(indices = NULL) {
  assert_binaries()
  
  models <- 
    c(
      "brusselator.cps",
      "chemotaxis_4.cps",
      "test_names.cps",
      "test_paramest.cps"
    )
  
  assert_that(is.null(indices) || is.numeric(indices) && all(indices %in% seq_along(models)), msg = "Invalid indices.")
  
  if (!is.null(indices))
    models <- models[indices]
  
  pkgname <- getPackageName()
  map(models, ~ loadModel(system.file("extdata", .x, package = pkgname)))
}

#' Compile the model
#' 
#' \code{compileModel} potentially forces compilation of the given model.
#' 
#' @param force flag
#' @param model a model object
#' @family model actions
#' @export
compileModel <- function(force = FALSE, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(is.flag(force), noNA(force))
  
  if (force)
    f <- c_datamodel$getModel()$forceCompile
  else
    f <- c_datamodel$getModel()$compileIfNecessary
  
  assert_that(grab_msg(f()), msg = "Compilation of the model failed.")
  
  invisible()
}

#' Apply the model's initial state
#' 
#' \code{applyInitialState} applies the given model's initial state.
#' 
#' @param model a model object
#' @family model actions
#' @export
applyInitialState <- function(model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  grab_msg(c_datamodel$getModel()$applyInitialValues)
  
  invisible()
}

#' Update the model's initial state from the current state
#' 
#' \code{updateInitialState} updates the given model's initial state from the current state.
#' 
#' @param model a model object
#' @family model actions
#' @export
updateInitialState <- function(model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  grab_msg(c_datamodel$getModel()$applyInitialValues)
  
  invisible()
}

#' Convert the model to irreversible reactions
#' 
#' \code{convertToIrreversible} converts the given model's reactions to irreversible reactions.
#' 
#' @param model a model object
#' @family model actions
#' @export
convertToIrreversible <- function(model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  assert_that(
    grab_msg(c_datamodel$getModel()$convert2NonReversible()),
    msg = "Converting the model's reactions to irreversible reactions failed."
  )
  
  invisible()
}

#' Open the model in the COPASI GUI
#' 
#' \code{openCopasi} saves the given model to a temporary file and opens it in the COPASI GUI.
#' 
#' @param readin if \code{TRUE}, the function waits for COPASI to quit and then reads in the temporary model file, overwriting the given model.
#' @param copasi_loc location of CopasiUI
#' @param model a model object
#' @family model handlers
#' @export
openCopasi <- function(readin = FALSE, copasi_loc = "CopasiUI", model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(
    is.flag(readin) && !is.na(readin),
    is.string(copasi_loc) && !is.na(copasi_loc)
  )
  
  if (.Platform$OS.type == "windows") {
    os <- "windows"
    
    found <- !suppressWarnings(system2("where", args = copasi_loc, stdout = FALSE, stderr = FALSE))
    
    # 'where' command can't find executables with full paths. Just test if readable for now.
    if (!found && !missing(copasi_loc))
      found <- is.readable(copasi_loc)
  } else if (.Platform$OS.type == "unix") {
    os <- "unix"
    
    if (substr(version$os, 1L, 6L) == "darwin") {
      os <- "darwin"
      
      # On darwin, CopasiUI doesn't seem to be in path so try a default location if copasi_loc wasn't given.
      if (system2("which", args = c("-s", copasi_loc)) && missing(copasi_loc))
        copasi_loc <- "/Applications/COPASI/CopasiUI.app/Contents/MacOS/CopasiUI"
    }
    
    found <- !system2("which", args = c(copasi_loc), stdout = NULL)
  }
  
  assert_that(
    found,
    msg = "Could not find CopasiUI. Consider specifying the `copasi_loc` argument or adding `CopasiUI` to PATH."
  )
  
  c_fittask <- as(c_datamodel$getTask("Parameter Estimation"), "_p_CFitTask")
  c_fitproblem <- as(c_fittask$getProblem(), "_p_CFitProblem")
  has_experiments <- !is_empty(c_fitproblem$getExperimentSet()$getFileNames()) || !is_empty(c_fitproblem$getCrossValidationSet()$getFileNames())
  
  if (!readin && has_experiments) {
    warning(
      "Using readin = TRUE because the model references experimental data which breaks when using the systems temp folder.",
      immediate. = TRUE
    )
    readin <- TRUE
  }
  
  if (readin) {
    # Create a temp file for the model
    file <- tempfile(pattern = "CoRC", tmpdir = normalizePathC(get_ref_dir(c_datamodel)), fileext = ".cps")
    grab_msg(c_datamodel$saveModel(file, overwriteFile = TRUE))
    
    if (os == "windows")
      system2(copasi_loc, file, wait = TRUE, invisible = FALSE)
    else if (os == "darwin")
      system2("open", c("-W", "-n", "-a", copasi_loc, file))
    else 
      system2(copasi_loc, file, wait = TRUE)
    
    grab_msg(c_datamodel$loadModel(file))
    file.remove(file)
  } else {
    # Create a temp file for the model to open in the UI
    # This potentially could cause issues if it is possible for temp files to have spaces in its path on windows
    file <- tempfile(pattern = "CoRC", fileext = ".cps")
    grab_msg(c_datamodel$saveModel(file, overwriteFile = TRUE))
    
    if (os == "windows")
      system2(copasi_loc, file, wait = FALSE, invisible = FALSE)
    else if (os == "darwin")
      system2("open", c("-n", "-a", copasi_loc, file))
    else
      system2(copasi_loc, file, wait = FALSE)
  }
  
  invisible()
}
