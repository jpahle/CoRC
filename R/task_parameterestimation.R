#' Run parameter estimation
#'
#' \code{runParamEst} runs parameter estimation and returns results in a list.
#'
#' @param calculateJacobian boolean
#' @param performStabilityAnalysis boolean
#' @param calculateStatistics boolean
#' @param updateModel boolean
#' @param method  character or list
#' @param datamodel a model object
#' @return a list of results
#' @export
runParamEst <- function(randomizeStartValues = NULL, createParameterSets = NULL, calculateStatistics = NULL, updateModel = NULL, method = NULL, datamodel = pkg_env$curr_dm) {
  # use the worker function to apply all given arguments
  # the worker function returns all args needed to restore previous settings
  restorationCall <- set_pes_worker(
    randomizeStartValues = randomizeStartValues,
    createParameterSets = createParameterSets,
    calculateStatistics = calculateStatistics,
    updateModel = updateModel,
    method = method,
    datamodel = datamodel
  )
  
  model <- as(datamodel$getModel(), "_p_CModel")
  task <- as(datamodel$getTask("Parameter Estimation"), "_p_CFitTask")
  problem <- as(task$getProblem(), "_p_CFitProblem")
  method <- as(task$getMethod(), "_p_COptMethod")
  
  success <- grab_msg(task$process(TRUE))
  
  do.call(set_pes_worker, restorationCall)
  
  assert_that(
    success,
    msg = paste0("Processing the task failed:\n", task$getProcessError())
  )
  
  ret <- list()
  
  ret
}

#' Set parameter estimation settings
#'
#' \code{setParamEstSettings} sets parameter estimation settings including method options.
#'
#' @param randomizeStartValues boolean
#' @param createParameterSets boolean
#' @param calculateStatistics boolean
#' @param updateModel boolean
#' @param executable boolean
#' @param method character or list
#' @param datamodel a model object
#' @export
setParamEstSettings <- function(randomizeStartValues = NULL, createParameterSets = NULL, calculateStatistics = NULL, updateModel = NULL, executable = NULL, method = NULL, datamodel = pkg_env$curr_dm) {
  assert_that(is.null(executable) || is_scalar_logical(executable))
  
  # Call the worker to set most settings
  set_pes_worker(
    randomizeStartValues = randomizeStartValues,
    createParameterSets = createParameterSets,
    calculateStatistics = calculateStatistics,
    updateModel = updateModel,
    method = method,
    datamodel = datamodel
  )
  
  if (!is.null(executable)) {
    datamodel$getTask("Parameter Estimation")$setScheduled(executable)
  }
  
  invisible()
}

set_pes_worker <- function(randomizeStartValues = NULL, createParameterSets = NULL, calculateStatistics = NULL, updateModel = NULL, method = NULL, datamodel = NULL) {
  assert_that(confirmDatamodel(datamodel))
  
  task <- as(datamodel$getTask("Parameter Estimation"), "_p_CFitTask")
  
  problem <- as(task$getProblem(), "_p_CFitProblem")
  
  assert_that(
    is.null(randomizeStartValues)   || is_scalar_logical(randomizeStartValues) && !is.na(randomizeStartValues),
    is.null(createParameterSets)    || is_scalar_logical(createParameterSets)  && !is.na(createParameterSets),
    is.null(calculateStatistics)    || is_scalar_logical(calculateStatistics)  && !is.na(calculateStatistics),
    is.null(updateModel)            || is_scalar_logical(updateModel)          && !is.na(updateModel),
    is.null(method)                 || is_scalar_character(method)             && !is.na(method) || is_list(method) && is_scalar_character(method$method) && !is.na(method$method)
  )
  
  restorationCall <- list(datamodel = datamodel)
  
  if (!is.null(method)) {
    if (is_scalar_character(method)) method <- list(method = method)
    # hack to get nice error message if method string is not accepted.
    with(method, rlang::arg_match(method, names(.__E___CTaskEnum__Method)[task$getValidMethods() + 1L]))
  }
  
  if (!is.null(randomizeStartValues)) {
    restorationCall$randomizeStartValues <- as.logical(problem$getRandomizeStartValues())
    problem$setRandomizeStartValues(randomizeStartValues)
  }
  
  if (!is.null(createParameterSets)) {
    restorationCall$createParameterSets <- as.logical(problem$getCreateParameterSets())
    problem$setCreateParameterSets(createParameterSets)
  }
  
  if (!is.null(calculateStatistics)) {
    restorationCall$calculateStatistics <- as.logical(problem$getCalculateStatistics())
    problem$setCalculateStatistics(calculateStatistics)
  }
  
  if (!is.null(updateModel)) {
    restorationCall$updateModel <- as.logical(task$isUpdateModel())
    task$setUpdateModel(updateModel)
  }
  
  if (!is.null(method) && !is_empty(method)) {
    # We need to keep track of the previously set method
    restorationCall$method_old = task$getMethod()$getSubType()
    
    task$setMethodType(method$method)
    restorationCall$method <- list(method = method$method)
    method_cop = as(task$getMethod(), "_p_CFitMethod")
    
    method <- method[names(method) != "method"]
    
    if (!is_empty(method)) {
      # get some info on what parameters the method has
      methodstruct <- methodstructure(method_cop) %>% tibble::rowid_to_column()
      
      method <-
        tibble::tibble(value = method) %>%
        dplyr::mutate(rowid = pmatch(names(value), methodstruct$name))
      
      # all names that are not names of method parameters
      bad_names <- names(method$value)[is.na(method$rowid)]
      
      # merge method with relevant lines from methodstruct and check if the given values is allowed
      method <-
        method %>%
        dplyr::filter(!is.na(rowid), !map_lgl(value, is_null)) %>%
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
      
      # First restore everything and then give complete feedback error.
      if (!is_empty(bad_names) || nrow(forbidden) != 0L || nrow(failures) != 0L) {
        do.call(set_pes_worker, restorationCall)
        errmsg <- ""
        if (!is_empty(bad_names)) errmsg <- paste0(errmsg, "Method parameter(s) \"", paste0(bad_names, collapse = "\", \""), "\" invalid. Should be one of : \"", paste0(methodstruct$name, collapse = "\", \""), "\"\n")
        if (nrow(forbidden) != 0L) errmsg <- paste0(errmsg, "Method parameter(s) \"", paste0(forbidden$name, collapse = "\", \""), "\" have to be of type(s) ", paste0(forbidden$type, collapse = "\", \""), ".\n")
        if (nrow(failures) != 0L) errmsg <- paste0(errmsg, "Method parameter(s) \"", paste0(failures$name, collapse = "\", \""), "\" could not be set.\n")
        stop(errmsg)
      }
    }
  }
  
  restorationCall
}
