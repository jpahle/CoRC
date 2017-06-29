#' Run to stready state
#'
#' \code{runSteadyState} calculates the steady state and returns the species concentrations as a data frame.
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
  restorationCall <- set_ss_worker(
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

  success <- task$process(TRUE)
  
  do.call(set_ss_worker, restorationCall)
  
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

  metabs <- model$getMetabolites()

  ret$species <-
    seq_along_cv(metabs) %>%
    map_df(~ {
      metab <- get_from_cv(metabs, .x)
      transit <- metab$getTransitionTime()
      if (!is.na(transit)) {
        list(
          key = list(structure(metab$getCN()$getString(), class = "copasi_key")),
          name = metab$getObjectDisplayName(),
          type = stringr::str_to_lower(metab$getStatus()),
          concentration = metab$getConcentration(),
          concentration.rate = metab$getConcentrationRate(),
          particlenum = metab$getValue(),
          particlenum.rate = metab$getRate(),
          transitiontime = transit
        )
      }
    })
  
  reactions <- model$getReactions()
  
  ret$reactions <-
    seq_along_cv(reactions) %>%
    map_df(~ {
      reaction <- get_from_cv(reactions, .x)
      list(
        key = list(structure(reaction$getCN()$getString(), class = "copasi_key")),
        name = reaction$getObjectName(),
        concentration.flux = reaction$getFlux(),
        particlenum.flux = reaction$getParticleFlux()
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
#' @param method list
#' @param datamodel a model object
#' @export
setSteadyStateSettings <- function(calculateJacobian = NULL, performStabilityAnalysis = NULL, updateModel = NULL, method = NULL, datamodel = pkg_env$curr_dm) {
  # Call the worker to set all settings
  set_ss_worker(
    calculateJacobian = calculateJacobian,
    performStabilityAnalysis = performStabilityAnalysis,
    updateModel = updateModel,
    method = method,
    datamodel = datamodel
  )
  
  invisible()
}

set_ss_worker <- function(calculateJacobian = NULL, performStabilityAnalysis = NULL, updateModel = NULL, method = NULL, datamodel = NULL) {
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
    restorationCall$updateModel <- (task$isUpdateModel() == 1L)
    task$setUpdateModel(updateModel)
  }
  
  if (!is.null(method) && !is_empty(method)) {
    method_cop = as(task$getMethod(), "_p_CTrajectoryMethod")
  
    validstruct <- methodstructure(method_cop)
    
    method <-
      tibble::tibble(name = names(method), value = method) %>%
      dplyr::mutate(varposition = pmatch(name, names(validstruct)))
    
    # all names that are not names of method parameters
    bad_names <- method$name[is.na(method$varposition)]
    
    method <- dplyr::filter(method, !is.na(varposition), !map_lgl(value, is_null))
    
    method <- dplyr::mutate(
      method,
      name = names(validstruct)[varposition],
      type = validstruct[varposition],
      allowed = map2_lgl(type, value, ~ {
        fun <- cparameter_control_functions[[.x]]
        if (!is_null(fun)) {
          fun(.y)
        } else {
          TRUE
        }
      })
    )
    
    # all parameters that did not satisfy the tests in cparameter_control_functions
    forbidden <- dplyr::filter(method, !allowed)
    
    method <- dplyr::filter(method, allowed)
    
    method <- dplyr::mutate(
      method,
      param = map(varposition, ~ method_cop$getParameter(.x - 1L)),
      oldval = map2(type, param, ~ cparameter_get_functions[[.x]](.y)),
      success = pmap_lgl(
        list(type = type, param = param, value = value),
        function(type, param, value) {cparameter_set_functions[[type]](param, value)}
      )
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
      do.call(set_ss_worker, restorationCall)
      errmsg <- ""
      if (!is_empty(bad_names)) errmsg <- paste0(errmsg, "Method parameter(s) \"", paste0(bad_names, collapse = "\", \""), "\" invalid. Should be one of : \"", paste0(names(validstruct), collapse = "\", \""), "\"\n")
      if (nrow(forbidden) != 0) errmsg <- paste0(errmsg, "Method parameter(s) \"", paste0(forbidden$name, collapse = "\", \""), "\" have to be of type(s) ", paste0(forbidden$type, collapse = "\", \""), ".\n")
      if (nrow(failures) != 0) errmsg <- paste0(errmsg, "Method parameter(s) \"", paste0(failures$name, collapse = "\", \""), "\" could not be set.\n")
      stop(errmsg)
    }
  }
  
  restorationCall
}
