#' Run to stready state
#'
#' \code{runSteadyState} calculates the steady state and returns results in a list.
#'
#' @param calculateJacobian flag
#' @param performStabilityAnalysis flag
#' @param updateModel flag
#' @param executable flag
#' @param method list
#' @param model a model object
#' @return a list of results
#' @export
runSteadyState <- function(calculateJacobian = NULL, performStabilityAnalysis = NULL, updateModel = NULL, executable = NULL, method = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  # does assertions
  settings <- ss_assemble_settings(
    calculateJacobian = calculateJacobian,
    performStabilityAnalysis = performStabilityAnalysis,
    updateModel = updateModel,
    executable = executable
  )
  
  c_task <- as(c_datamodel$getTask("Steady-State"), "_p_CSteadyStateTask")
  
  # does assertions
  method_settings <- ss_assemble_method(method, c_task)
  
  # try to avoid doing changes for performance reasons
  do_settings <- !is_empty(settings)
  do_method <- !is_empty(method_settings)
  
  c_method <- as(c_task$getMethod(), "_p_CSteadyStateMethod")
  
  # save all previous settings
  if (do_settings)
    pre_settings <- ss_get_settings(c_task)
  if (do_method)
    pre_method_settings <- get_method_settings(c_method)
  
  # apply settings
  success <- !is.error(try(ss_set_settings(settings, c_task)))
  if (success)
    success <- !is.error(try(set_method_settings(method_settings, c_method)))
  # initialize task
  if (success)
    success <- grab_msg(c_task$initializeRaw(OUTPUTFLAG))
  # run task and save current settings
  if (success) {
    full_settings <- ss_get_settings(c_task)
    full_settings$method <- get_method_settings(c_method)
    success <- grab_msg(c_task$processRaw(TRUE))
  }
  # get results
  if (success)
    ret <- ss_get_results(c_datamodel, full_settings)
  
  # revert all settings
  if (do_settings)
    ss_set_settings(pre_settings, c_task)
  if (do_method)
    set_method_settings(pre_method_settings, c_method)
  
  # assertions only after restoration of settings
  assert_that(
    success,
    msg = paste0("Processing the task failed.")
  )
  
  ret
}

#' Set steady state settings
#'
#' \code{setSteadyStateSettings} sets steady state task settings including method options.
#'
#' @param calculateJacobian flag
#' @param performStabilityAnalysis flag
#' @param updateModel flag
#' @param executable flag
#' @param method list
#' @param model a model object
#' @export
setSteadyStateSettings <- function(calculateJacobian = NULL, performStabilityAnalysis = NULL, updateModel = NULL, executable = NULL, method = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  # does assertions
  settings <- ss_assemble_settings(
    calculateJacobian = calculateJacobian,
    performStabilityAnalysis = performStabilityAnalysis,
    updateModel = updateModel,
    executable = executable
  )
  
  c_task <- as(c_datamodel$getTask("Steady-State"), "_p_CSteadyStateTask")
  
  # does assertions
  method_settings <- ss_assemble_method(method, c_task)
  
  c_method <- as(c_task$getMethod(), "_p_CSteadyStateMethod")
  
  ss_set_settings(settings, c_task)
  set_method_settings(method_settings, c_method)
  
  invisible()
}

#' Get steady state settings
#'
#' \code{getSteadyStateSettings} gets steady state task settings including method options.
#'
#' @param model a model object
#' @return A list of time course task settings including method options.
#' @export
getSteadyStateSettings <- function(model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  c_task <- as(c_datamodel$getTask("Steady-State"), "_p_CSteadyStateTask")
  c_method <- as(c_task$getMethod(), "_p_CSteadyStateMethod")
  
  ret <- ss_get_settings(c_task)
  ret$method <- get_method_settings(c_method)
  
  ret
}

# The following functions should be the basis for implementation of any task
# They should allow for a common workflow with most tasks

# does assertions
# returns a list of settings
ss_assemble_settings <- function(calculateJacobian, performStabilityAnalysis, updateModel, executable) {
  assert_that(
    is.null(calculateJacobian)        || noNA(calculateJacobian)        && is.flag(calculateJacobian),
    is.null(performStabilityAnalysis) || noNA(performStabilityAnalysis) && is.flag(performStabilityAnalysis),
    is.null(updateModel)              || noNA(updateModel)              && is.flag(updateModel),
    is.null(executable)               || noNA(executable)               && is.flag(executable)
  )
  
  if (isTRUE(performStabilityAnalysis) && !isTRUE(calculateJacobian))
    stop("performStabilityAnalysis can only be set in combination with calculateJacobian.")
  
  list(
    calculateJacobian = calculateJacobian,
    performStabilityAnalysis = performStabilityAnalysis,
    updateModel = updateModel,
    executable = executable
  ) %>%
    discard(is_null)
}

# does assertions
# returns a list of method settings
ss_assemble_method <- function(method, c_task) {
  if (is_null(method))
    return(list())
  
  assert_that(is.list(method), !has_name(method, "method"))
  
  method
}

# gets full list of settings
ss_get_settings <- function(c_task) {
  c_problem <- as(c_task$getProblem(), "_p_CSteadyStateProblem")
  
  list(
    calculateJacobian =        as.logical(c_problem$isJacobianRequested()),
    performStabilityAnalysis = as.logical(c_problem$isStabilityAnalysisRequested()),
    updateModel =              as.logical(c_task$isUpdateModel()),
    executable =               as.logical(c_task$isScheduled())
  )
}

# sets all settings given in list
ss_set_settings <- function(data, c_task) {
  if (is_empty(data))
    return()
  
  c_problem <- as(c_task$getProblem(), "_p_CSteadyStateProblem")
  
  if (!is_null(data$calculateJacobian))
    c_problem$setJacobianRequested(data$calculateJacobian)
  
  if (!is_null(data$performStabilityAnalysis))
    c_problem$setStabilityAnalysisRequested(data$performStabilityAnalysis)
  
  if (!is_null(data$updateModel))
    c_task$setUpdateModel(data$updateModel)
  
  if (!is_null(data$executable))
    c_task$setScheduled(data$executable)
}

# gathers all results
ss_get_results <- function(c_datamodel, settings) {
  c_model <- as(c_datamodel$getModel(), "_p_CModel")
  c_task <- as(c_datamodel$getTask("Steady-State"), "_p_CSteadyStateTask")
  c_method <- as(c_task$getMethod(), "_p_CSteadyStateMethod")
  
  result <- c_task$getResult()
  
  cl_metabs <- get_cdv(c_model$getMetabolites())
  # if no transition time, metabs aren't listed
  cl_metabs_ss <- discard(cl_metabs, is.na(map_swig_dbl(cl_metabs, "getTransitionTime")))
  
  species <-
    tibble::tibble(
      key = map_swig_chr(cl_metabs_ss, "getObjectDisplayName"),
      name = map_swig_chr(cl_metabs_ss, "getObjectName"),
      type = cl_metabs_ss %>% map_swig_chr("getStatus") %>% stringr::str_to_lower(),
      concentration = map_swig_dbl(cl_metabs_ss, "getConcentration"),
      concentration.rate = map_swig_dbl(cl_metabs_ss, "getConcentrationRate"),
      number = map_swig_dbl(cl_metabs_ss, "getValue"),
      number.rate = map_swig_dbl(cl_metabs_ss, "getRate"),
      transitiontime = map_swig_dbl(cl_metabs_ss, "getTransitionTime")
    )
  
  cl_reacts <- get_cdv(c_model$getReactions())
  
  reactions <-
    tibble::tibble(
      key = map_swig_chr(cl_reacts, "getObjectDisplayName"),
      name = map_swig_chr(cl_reacts, "getObjectName"),
      concentration.flux = map_swig_dbl(cl_reacts, "getFlux"),
      particlenum.flux = map_swig_dbl(cl_reacts, "getParticleFlux")
    )
  
  jacobian.complete <- NULL
  jacobian.reduced <- NULL
  
  if (settings$calculateJacobian) {
    jacobian.complete <- get_annotated_matrix(c_task$getJacobianAnnotated())
    jacobian.reduced <- get_annotated_matrix(c_task$getJacobianXAnnotated())
  }
  
  protocol <- c_method$getMethodLog()
  
  list(
    settings = settings,
    result = result,
    species = species,
    reactions = reactions,
    jacobian.complete = jacobian.complete,
    jacobian.reduced = jacobian.reduced,
    protocol = protocol
  )
}
