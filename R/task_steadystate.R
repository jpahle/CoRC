#' Run to stready state
#'
#' \code{runSteadyState} calculates the steady state and returns results in a list.
#'
#' @param calculateJacobian boolean
#' @param performStabilityAnalysis boolean
#' @param updateModel boolean
#' @param method list
#' @param datamodel a model object
#' @return a list of results
#' @export
runSteadyState <- function(calculateJacobian = NULL, performStabilityAnalysis = NULL, updateModel = NULL, method = NULL, datamodel = pkg_env$curr_dm) {
  # use the worker function to apply all given arguments
  # the worker function returns all args needed to restore previous settings
  restorationCall <- set_sss_worker(
    calculateJacobian = calculateJacobian,
    performStabilityAnalysis = performStabilityAnalysis,
    updateModel = updateModel,
    method = method,
    datamodel = datamodel
  )
  
  model <- as(datamodel$getModel(), "_p_CModel")
  
  task <- as(datamodel$getTask("Steady-State"), "_p_CSteadyStateTask")

  problem <- as(task$getProblem(), "_p_CSteadyStateProblem")
  
  method <- as(task$getMethod(), "_p_CSteadyStateMethod")

  success <- grab_msg(task$process(TRUE))
  
  do.call(set_sss_worker, restorationCall)
  
  assert_that(
    success,
    msg = paste0("Processing the task failed:\n", task$getProcessError())
  )
  
  ret <- list(
    result = NULL,
    species = NULL,
    # compartments = NULL,
    # modelquantities = NULL,
    reactions = NULL,
    # stability = NULL,
    # jacobian.complete = NULL,
    # jacobian.reduced = NULL,
    protocol = NULL
  )
  
  ret$result <- task$getResult()

  ret$species <-
    get_cv(model$getMetabolites()) %>%
    map_df(~ {
      transit <- .x$getTransitionTime()
      if (!is.na(transit)) {
        list(
          key = list(structure(.x$getCN()$getString(), class = "copasi_key")),
          name = .x$getObjectName(),
          type = stringr::str_to_lower(.x$getStatus()),
          concentration = .x$getConcentration(),
          concentration.rate = .x$getConcentrationRate(),
          particlenum = .x$getValue(),
          particlenum.rate = .x$getRate(),
          transitiontime = transit
        )
      }
    })
  
  ret$reactions <-
    get_cv(model$getReactions()) %>%
    map_df(~ {
      list(
        key = list(structure(.x$getCN()$getString(), class = "copasi_key")),
        name = .x$getObjectName(),
        concentration.flux = .x$getFlux(),
        particlenum.flux = .x$getParticleFlux()
      )
    })
  
  ret$protocol <- method$getMethodLog()
  
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
#' @param datamodel a model object
#' @export
setSteadyStateSettings <- function(calculateJacobian = NULL, performStabilityAnalysis = NULL, updateModel = NULL, executable = NULL, method = NULL, datamodel = pkg_env$curr_dm) {
  assert_that(is.null(executable) || is_scalar_logical(executable))
  
  # Call the worker to set most settings
  set_sss_worker(
    calculateJacobian = calculateJacobian,
    performStabilityAnalysis = performStabilityAnalysis,
    updateModel = updateModel,
    method = method,
    datamodel = datamodel
  )
  
  task <- as(datamodel$getTask("Steady-State"), "_p_CSteadyStateTask")
  
  if (!is.null(executable)) {
    task$setScheduled(executable)
  }
  
  invisible()
}

set_sss_worker <- function(calculateJacobian = NULL, performStabilityAnalysis = NULL, updateModel = NULL, method = NULL, datamodel = NULL) {
  assert_that(confirmDatamodel(datamodel))
  
  task <- as(datamodel$getTask("Steady-State"), "_p_CSteadyStateTask")
  
  problem <- as(task$getProblem(), "_p_CSteadyStateProblem")
  
  assert_that(
    is.null(calculateJacobian)        || is_scalar_logical(calculateJacobian)        && !is.na(calculateJacobian),
    is.null(performStabilityAnalysis) || is_scalar_logical(performStabilityAnalysis) && !is.na(performStabilityAnalysis),
    is.null(updateModel)              || is_scalar_logical(updateModel)              && !is.na(updateModel),
    is.null(method)                   || is_list(method)
  )
  
  if (isTRUE(performStabilityAnalysis) && !isTRUE(calculateJacobian)) stop("performStabilityAnalysis can only be set in combination with calculateJacobian.")
  
  restorationCall <- list(datamodel = datamodel)
  
  if (!is.null(calculateJacobian)) {
    restorationCall$calculateJacobian <- problem$isJacobianRequested()
    problem$setJacobianRequested(calculateJacobian)
  }
  
  if (!is.null(performStabilityAnalysis)) {
    restorationCall$performStabilityAnalysis <- problem$isStabilityAnalysisRequested()
    problem$setStabilityAnalysisRequested(performStabilityAnalysis)
  }
  
  if (!is.null(updateModel)) {
    restorationCall$updateModel <- task$isUpdateModel()
    task$setUpdateModel(updateModel)
  }
  
  if (!is.null(method) && !is_empty(method)) {
    method_cop = as(task$getMethod(), "_p_CTrajectoryMethod")
  
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
    if (nrow(method) != 0) {
      restorationCall$method <- method %>% dplyr::select(name, oldval) %>% tibble::deframe()
    }
    
    # First restore everything and then give complete feedback error.
    if (!is_empty(bad_names) || nrow(forbidden) != 0 || nrow(failures) != 0) {
      do.call(set_sss_worker, restorationCall)
      errmsg <- ""
      if (!is_empty(bad_names)) errmsg <- paste0(errmsg, "Method parameter(s) \"", paste0(bad_names, collapse = "\", \""), "\" invalid. Should be one of : \"", paste0(methodstruct$name, collapse = "\", \""), "\"\n")
      if (nrow(forbidden) != 0) errmsg <- paste0(errmsg, "Method parameter(s) \"", paste0(forbidden$name, collapse = "\", \""), "\" have to be of type(s) ", paste0(forbidden$type, collapse = "\", \""), ".\n")
      if (nrow(failures) != 0) errmsg <- paste0(errmsg, "Method parameter(s) \"", paste0(failures$name, collapse = "\", \""), "\" could not be set.\n")
      stop(errmsg)
    }
  }
  
  restorationCall
}
