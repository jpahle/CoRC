#' Get the currently active model
#'
#' \code{getCurrentModel} returns the currently active model
#'
#' @return a model object
#' @export
getCurrentModel <- function() {
  assert_that(!is_null(pkg_env$curr_dm), msg = "No model currently in use.")

  pkg_env$curr_dm
}

#' Load a model
#'
#' \code{loadModel} loads a model into copasi and returns a reference to it.
#'
#' @param filename path to model
#'
#' @return a model object
#' @export
loadModel <- function(filename) {
  assert_that(assertthat::is.readable(filename))

  datamodel <- CRootContainer_addDatamodel()
  success <- datamodel$loadModel(filename)

  if (!success) {
    CRootContainer_removeDatamodel(datamodel)
    stop("Couldn't load model file.")
  }

  pkg_env$curr_dm <- datamodel
  datamodel
}

#' Unload a model
#'
#' \code{unloadModel} frees memory by unloading a currently active model from copasi
#'
#' @param datamodel a model object
#' @export
unloadModel <- function(datamodel = pkg_env$curr_dm) {
  assert_that(confirmDatamodel(datamodel))

  datamodel <- CRootContainer_removeDatamodel(datamodel)
}

#' Save a model as a .cps file
#'
#' \code{saveCPS} saves a model as a .cps file
#'
#' @param filename a path to save to
#' @param overwrite is overwriting existing files allowed?
#' @param datamodel a model object
#' @export
saveCPS <- function(filename, overwrite = FALSE, datamodel = pkg_env$curr_dm) {
  assert_that(is_scalar_character(filename))

  if (!assertthat::has_extension(filename, "cps")) {
    filename <- paste0(filename, ".cps")
  }

  success <- datamodel$saveModel(filename, overwriteFile = overwrite)

  assert_that(success, msg = paste0("Model failed to save at: ", filename))

  invisible(success)
}
