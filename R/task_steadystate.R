#' Run to stready state
#'
#' \code{runSteadyState} calculates the steady state and returns results in a list.
#'
#' @param calculateJacobian boolean
#' @param performStabilityAnalysis boolean
#' @param updateModel boolean
#' @param method list
#' @param model a model object
#' @return a list of results
#' @export
runSteadyState <- function(calculateJacobian = NULL, performStabilityAnalysis = NULL, updateModel = NULL, method = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  # use the worker function to apply all given arguments
  # the worker function returns all args needed to restore previous settings
  restorationCall <- ss_settings_worker(
    .type = "temporary",
    calculateJacobian = calculateJacobian,
    performStabilityAnalysis = performStabilityAnalysis,
    updateModel = updateModel,
    method = method,
    c_datamodel = c_datamodel
  )
  
  c_task <- as(c_datamodel$getTask("Steady-State"), "_p_CSteadyStateTask")
  
  success <- grab_msg(c_task$initializeRaw(OUTPUTFLAG))
  if (success)
    success <- grab_msg(c_task$processRaw(TRUE))
  if (success)
    ret <- ss_result_worker(c_datamodel)
  
  # Call the worker again to restore previous settings.
  do.call(ss_settings_worker, restorationCall)
  
  # Assertions only after restoration of settings
  assert_that(
    success,
    msg = paste0("Processing the task failed.")
  )
  
  ret
}

#' Set steady state settings
#'
#' \code{setSteadyStateSettings} sets steady state settings including method options.
#'
#' @param calculateJacobian boolean
#' @param performStabilityAnalysis boolean
#' @param updateModel boolean
#' @param executable boolean
#' @param method list
#' @param model a model object
#' @export
setSteadyStateSettings <- function(calculateJacobian = NULL, performStabilityAnalysis = NULL, updateModel = NULL, executable = NULL, method = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(is.null(executable) || is.flag(executable) && !is.na(executable))
  
  # Call the worker to set most settings
  ss_settings_worker(
    .type = "permanent",
    calculateJacobian = calculateJacobian,
    performStabilityAnalysis = performStabilityAnalysis,
    updateModel = updateModel,
    method = method,
    c_datamodel = c_datamodel
  )
  
  c_task <- as(c_datamodel$getTask("Steady-State"), "_p_CSteadyStateTask")
  
  if (!is.null(executable)) {
    c_task$setScheduled(executable)
  }
  
  invisible()
}

ss_settings_worker <- function(.type, calculateJacobian = NULL, performStabilityAnalysis = NULL, updateModel = NULL, method = NULL, c_datamodel) {
  c_task <- as(c_datamodel$getTask("Steady-State"), "_p_CSteadyStateTask")
  c_problem <- as(c_task$getProblem(), "_p_CSteadyStateProblem")
  
  assert_that(
    is.null(calculateJacobian)        || is.flag(calculateJacobian)        && !is.na(calculateJacobian),
    is.null(performStabilityAnalysis) || is.flag(performStabilityAnalysis) && !is.na(performStabilityAnalysis),
    is.null(updateModel)              || is.flag(updateModel)              && !is.na(updateModel),
    is.null(method)                   || is.list(method)
  )
  
  if (isTRUE(performStabilityAnalysis) && !isTRUE(calculateJacobian)) stop("performStabilityAnalysis can only be set in combination with calculateJacobian.")
  
  errors <- FALSE
  
  restorationCall <- list(
    .type = "restore",
    c_datamodel = c_datamodel
  )
  
  if (!is.null(calculateJacobian)) {
    restorationCall$calculateJacobian <- as.logical(c_problem$isJacobianRequested())
    c_problem$setJacobianRequested(calculateJacobian)
  }
  
  if (!is.null(performStabilityAnalysis)) {
    restorationCall$performStabilityAnalysis <- as.logical(c_problem$isStabilityAnalysisRequested())
    c_problem$setStabilityAnalysisRequested(performStabilityAnalysis)
  }
  
  if (!is.null(updateModel)) {
    restorationCall$updateModel <- as.logical(c_task$isUpdateModel())
    c_task$setUpdateModel(updateModel)
  }
  
  if (!is.null(method) && !is_empty(method)) {
    c_method = as(c_task$getMethod(), "_p_CSteadyStateMethod")
  
    # get some info on what parameters the method has
    methodstruct <- methodstructure(c_method) %>% tibble::rowid_to_column()
    
    method <-
      tibble::tibble(value = method) %>%
      dplyr::mutate(rowid = pmatch(names(value), methodstruct$name))
    
    # all names that are not names of method parameters
    bad_names <- names(method$value)[is.na(method$rowid)]
    
    # merge method with relevant lines from methodstruct and check if the given values is allowed
    method <-
      method %>%
      dplyr::filter(!is.na(rowid), map_lgl(value, negate(is_null))) %>%
      dplyr::left_join(methodstruct, by = "rowid") %>%
      dplyr::mutate(
        allowed = map2_lgl(control_fun, value, ~ {
          if (!is_null(.x)) .x(.y)
          # No control function defined means just pass
          else TRUE
        })
      )
    
    # all parameters that did not satisfy the tests in methodstruct$control_fun
    forbidden <- dplyr::filter(method, !allowed)
    
    method <- dplyr::filter(method, allowed)
    
    # gather old value and then set new value
    method <-
      method %>%
      dplyr::mutate(
        oldval = map2(get_fun, object, ~ .x(.y)),
        success = pmap_lgl(., function(set_fun, object, value, ...) {set_fun(object, value)})
      )
    
    # parameters where trying to set it somehow failed as per feedback from copasi
    failures <- dplyr::filter(method, !success)
    
    method <- dplyr::filter(method, success)
    
    # Overwritten parameters need to be in the restorationCall
    if (nrow(method) != 0L) {
      restorationCall$method <- method %>% dplyr::select(name, oldval) %>% tibble::deframe()
    }
    
    # Give complete feedback error.
    errmsg <- ""
    if (!is_empty(bad_names)) errmsg <- paste0(errmsg, "Method parameter(s) \"", paste0(bad_names, collapse = "\", \""), "\" invalid. Should be one of : \"", paste0(methodstruct$name, collapse = "\", \""), "\"\n")
    if (nrow(forbidden) != 0L) errmsg <- paste0(errmsg, "Method parameter(s) \"", paste0(forbidden$name, collapse = "\", \""), "\" have to be of type(s) ", paste0(forbidden$type, collapse = "\", \""), ".\n")
    if (nrow(failures) != 0L) errmsg <- paste0(errmsg, "Method parameter(s) \"", paste0(failures$name, collapse = "\", \""), "\" could not be set.\n")
    if (nchar(errmsg) != 0L) {
      try(stop(errmsg))
      errors <- TRUE
    }
  }
  
  if (errors && .type != "restore") {
    do.call(ss_settings_worker, restorationCall)
    stop("Rolled back task due to errors during setup.")
  }
  
  restorationCall
}

ss_result_worker <- function(c_datamodel) {
  c_model <- as(c_datamodel$getModel(), "_p_CModel")
  c_task <- as(c_datamodel$getTask("Steady-State"), "_p_CSteadyStateTask")
  c_method <- as(c_task$getMethod(), "_p_CSteadyStateMethod")
  
  ret <- list()
  
  ret$result <- c_task$getResult()
  
  cl_metabs <- get_cdv(c_model$getMetabolites())
  cl_metabs_ss <- discard(cl_metabs, is.na(map_swig_dbl(cl_metabs, "getTransitionTime")))
  
  ret$species <-
    tibble::tibble(
      key = map_chr(cl_metabs_ss, get_cn),
      name = map_swig_chr(cl_metabs_ss, "getObjectName"),
      type = cl_metabs_ss %>% map_swig_chr("getStatus") %>% stringr::str_to_lower(),
      concentration = map_swig_dbl(cl_metabs_ss, "getConcentration"),
      concentration.rate = map_swig_dbl(cl_metabs_ss, "getConcentrationRate"),
      number = map_swig_dbl(cl_metabs_ss, "getValue"),
      number.rate = map_swig_dbl(cl_metabs_ss, "getRate"),
      transitiontime = map_swig_dbl(cl_metabs_ss, "getTransitionTime")
    )
  
  cl_reacts <- get_cdv(c_model$getReactions())
  
  ret$reactions <-
    tibble::tibble(
      key = map_chr(cl_reacts, get_cn),
      name = map_swig_chr(cl_reacts, "getObjectName"),
      concentration.flux = map_swig_dbl(cl_reacts, "getFlux"),
      particlenum.flux = map_swig_dbl(cl_reacts, "getParticleFlux")
    )
  
  ret$jacobian.complete <- get_annotated_matrix(c_task$getJacobianAnnotated())
    
  ret$jacobian.reduced <- get_annotated_matrix(c_task$getJacobianXAnnotated())
  
  ret$protocol <- c_method$getMethodLog()
  
  ret
}
