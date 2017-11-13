#' Run metabolic control analysis
#'
#' \code{runMetabolicControlAnalysis} runs metabolic control analysis and returns the results in a list.
#'
#' @param perform_steady_state_analysis flag
#' @param executable flag
#' @param method list
#' @param model A model object.
#' @eval paste0("@return A list of results.
#' \\itemize{
#'   \\item \\code{$result_ss} can be one of ", rox_print_v(names(.__E___CSteadyStateMethod__ReturnCode)), ".
#' }")
#' @family metabolic control analysis
#' @export
runMetabolicControlAnalysis <- function(perform_steady_state_analysis = NULL, executable = NULL, method = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  # does assertions
  settings <- mca_assemble_settings(
    perform_steady_state_analysis = perform_steady_state_analysis,
    executable                    = executable
  )
  
  # does assertions
  method_settings <- mca_assemble_method(method)
  
  # try to avoid doing changes for performance reasons
  do_settings <- !is_empty(settings)
  do_method <- !is_empty(method_settings)
  
  c_model <- c_datamodel$getModel()
  c_task <- as(c_datamodel$getTask("Metabolic Control Analysis"), "_p_CMCATask")
  c_method <- as(c_task$getMethod(), "_p_CMCAMethod")
  
  # save all previous settings
  if (do_settings)
    pre_settings <- mca_get_settings(c_task)
  if (do_method)
    pre_method_settings <- get_method_settings(c_method)
  
  tryCatch({
    # apply settings
    if (do_settings)
      mca_set_settings(settings, c_task)
    if (do_method)
      set_method_settings(method_settings, c_method)
    
    grab_msg(c_model$compileIfNecessary())
    
    # initialize task
    assert_that(
      grab_msg(c_task$initializeRaw(OUTPUTFLAG)),
      msg = "Initializing the task failed."
    )
    
    # run task and save current settings
    full_settings <- mca_get_settings(c_task)
    full_settings$method <- get_method_settings(c_method)
    assert_that(
      grab_msg(c_task$processRaw(TRUE)),
      msg = "Processing the task failed."
    )
    
    # write back info from math container to model
    update_model_from_mc(c_task$getMathContainer())
    
    # get results
    ret <- mca_get_results(c_task, full_settings)
  },
  finally = {
    # revert all settings
    if (do_settings)
      mca_set_settings(pre_settings, c_task)
    if (do_method)
      set_method_settings(pre_method_settings, c_method)
  })
  
  ret
}

#' Set metabolic control analysis settings
#'
#' \code{setMetabolicControlAnalysis} sets metabolic control analysis task settings including method options.
#'
#' @param perform_steady_state_analysis flag
#' @param executable flag
#' @param method list
#' @param model a model object
#' @family metabolic control analysis
#' @export
setMetabolicControlAnalysisSettings <- function(perform_steady_state_analysis = NULL, executable = NULL, method = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  # does assertions
  settings <- mca_assemble_settings(
    perform_steady_state_analysis = perform_steady_state_analysis,
    executable                    = executable
  )
  
  # does assertions
  method_settings <- mca_assemble_method(method)
  
  c_task <- as(c_datamodel$getTask("Metabolic Control Analysis"), "_p_CMCATask")
  c_method <- as(c_task$getMethod(), "_p_CMCAMethod")
  
  mca_set_settings(settings, c_task)
  set_method_settings(method_settings, c_method)
  
  invisible()
}

#' Get metabolic control analysis settings
#'
#' \code{getMetabolicControlAnalysisSettings} gets metabolic control analysis settings including method options.
#'
#' @param model a model object
#' @return A list of metabolic control analysis task settings including method options.
#' @family metabolic control analysis
#' @export
getMetabolicControlAnalysisSettings <- function(model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  c_task <- as(c_datamodel$getTask("Metabolic Control Analysis"), "_p_CMCATask")
  c_method <- as(c_task$getMethod(), "_p_CMCAMethod")
  
  ret <- mca_get_settings(c_task)
  ret$method <- get_method_settings(c_method)
  
  ret
}

#' @rdname runMetabolicControlAnalysis
#' @export
runMCA <- runMetabolicControlAnalysis
#' @rdname setMetabolicControlAnalysisSettings
#' @export
setMCA <- setMetabolicControlAnalysisSettings
#' @rdname getMetabolicControlAnalysisSettings
#' @export
getMCA <- getMetabolicControlAnalysisSettings

# The following functions should be the basis for implementation of any task
# They should allow for a common workflow with most tasks

# does assertions
# returns a list of settings
mca_assemble_settings <- function(perform_steady_state_analysis, executable) {
  assert_that(
    is.null(perform_steady_state_analysis) || is.flag(perform_steady_state_analysis) && noNA(perform_steady_state_analysis),
    is.null(executable)                    || is.flag(executable)                    && noNA(executable)
  )
  
  list(
    perform_steady_state_analysis = perform_steady_state_analysis,
    executable                    = executable
  ) %>%
    discard(is.null)
}

# does assertions
# returns a list of method settings
mca_assemble_method <- function(method) {
  if (is.null(method))
    return(list())
  
  assert_that(
    is.list(method) && (is_empty(method) || !is.null(names(method))),
    msg = "method must be a named list."
  )
  assert_that(!hasName(method, "method"))
  
  method
}

# gets full list of settings
mca_get_settings <- function(c_task) {
  c_problem <- as(c_task$getProblem(), "_p_CMCAProblem")
  
  list(
    perform_steady_state_analysis = as.logical(c_problem$isSteadyStateRequested()),
    executable                    = as.logical(c_task$isScheduled())
  )
}

# sets all settings given in list
mca_set_settings <- function(data, c_task) {
  if (is_empty(data))
    return()
  
  c_problem <- as(c_task$getProblem(), "_p_CMCAProblem")
  
  if (!is.null(data$perform_steady_state_analysis))
    c_problem$setSteadyStateRequested(data$perform_steady_state_analysis)
  
  if (!is.null(data$executable))
    c_task$setScheduled(data$executable)
}

# gathers all results
mca_get_results <- function(c_task, settings) {
  c_method <- as(c_task$getMethod(), "_p_CMCAMethod")
  
  ss_result <- c_method$getSteadyStateStatus()
  
  elasticities_scaled <- get_annotated_matrix(c_method$getScaledElasticitiesAnn())
  elasticities_unscaled <- get_annotated_matrix(c_method$getUnscaledElasticitiesAnn())
  
  flux_control_coefficients_scaled <- NULL
  flux_control_coefficients_unscaled <- NULL
  concentration_control_coefficients_scaled <- NULL
  concentration_control_coefficients_unscaled <- NULL
  
  if (ss_result != "foundEquilibrium") {
    flux_control_coefficients_scaled <- get_annotated_matrix(c_method$getScaledFluxCCAnn())
    flux_control_coefficients_unscaled <- get_annotated_matrix(c_method$getUnscaledFluxCCAnn())
    concentration_control_coefficients_scaled <- get_annotated_matrix(c_method$getScaledConcentrationCCAnn())
    concentration_control_coefficients_unscaled <- get_annotated_matrix(c_method$getUnscaledConcentrationCCAnn())
  }
  
  list(
    settings                                    = settings,
    result_ss                                   = ss_result,
    elasticities_scaled                         = elasticities_scaled,
    elasticities_unscaled                       = elasticities_unscaled,
    flux_control_coefficients_scaled            = flux_control_coefficients_scaled,
    flux_control_coefficients_unscaled          = flux_control_coefficients_unscaled,
    concentration_control_coefficients_scaled   = concentration_control_coefficients_scaled,
    concentration_control_coefficients_unscaled = concentration_control_coefficients_unscaled
  )
}
