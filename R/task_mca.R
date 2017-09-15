#' Run metabolic control analysis
#'
#' \code{runMetabolicControlAnalysis} runs metabolic control analysis and returns the results in a list.
#'
#' @param performSteadyStateAnalysis flag
#' @param executable flag
#' @param method list
#' @param model a model object
#' @return a list of results
#' @export
runMetabolicControlAnalysis <- function(performSteadyStateAnalysis = NULL, executable = NULL, method = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  # does assertions
  settings <- mca_assemble_settings(
    performSteadyStateAnalysis = performSteadyStateAnalysis,
    executable = executable
  )
  
  c_task <- as(c_datamodel$getTask("Metabolic Control Analysis"), "_p_CMCATask")
  
  # does assertions
  method_settings <- mca_assemble_method(method, c_task)
  
  # try to avoid doing changes for performance reasons
  do_settings <- !is_empty(settings)
  do_method <- !is_empty(method_settings)
  
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
#' @param performSteadyStateAnalysis flag
#' @param executable flag
#' @param method list
#' @param model a model object
#' @export
setMetabolicControlAnalysisSettings <- function(performSteadyStateAnalysis = NULL, executable = NULL, method = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  # does assertions
  settings <- ss_assemble_settings(
    performSteadyStateAnalysis = performSteadyStateAnalysis,
    executable = executable
  )
  
  c_task <- as(c_datamodel$getTask("Metabolic Control Analysis"), "_p_CMCATask")
  
  # does assertions
  method_settings <- mca_assemble_method(method, c_task)
  
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
mca_assemble_settings <- function(performSteadyStateAnalysis, executable) {
  assert_that(
    is.null(performSteadyStateAnalysis) || is.flag(performSteadyStateAnalysis) && noNA(performSteadyStateAnalysis),
    is.null(executable)                 || is.flag(executable)                 && noNA(executable)
  )
  
  list(
    performSteadyStateAnalysis = performSteadyStateAnalysis,
    executable = executable
  ) %>%
    discard(is.null)
}

# does assertions
# returns a list of method settings
mca_assemble_method <- function(method, c_task) {
  if (is.null(method))
    return(list())
  
  assert_that(is.list(method), !has_name(method, "method"))
  
  method
}

# gets full list of settings
mca_get_settings <- function(c_task) {
  c_problem <- as(c_task$getProblem(), "_p_CMCAProblem")
  
  list(
    performSteadyStateAnalysis = as.logical(c_problem$isSteadyStateRequested()),
    executable                 = as.logical(c_task$isScheduled())
  )
}

# sets all settings given in list
mca_set_settings <- function(data, c_task) {
  if (is_empty(data))
    return()
  
  c_problem <- as(c_task$getProblem(), "_p_CMCAProblem")
  
  if (!is.null(data$performSteadyStateAnalysis))
    c_problem$setSteadyStateRequested(data$performSteadyStateAnalysis)
  
  if (!is.null(data$executable))
    c_task$setScheduled(data$executable)
}

# gathers all results
mca_get_results <- function(c_task, settings) {
  c_method <- as(c_task$getMethod(), "_p_CMCAMethod")
  
  ss.result <- c_method$getSteadyStateStatus()
  
  elasticities.scaled <- get_annotated_matrix(c_method$getScaledElasticitiesAnn())
  elasticities.unscaled <- get_annotated_matrix(c_method$getUnscaledElasticitiesAnn())
  
  flux.control.coefficients.scaled <- NULL
  flux.control.coefficients.unscaled <- NULL
  concentration.control.coefficients.scaled <- NULL
  concentration.control.coefficients.unscaled <- NULL
  
  if (ss.result != "foundEquilibrium") {
    flux.control.coefficients.scaled <- get_annotated_matrix(c_method$getScaledFluxCCAnn())
    flux.control.coefficients.unscaled <- get_annotated_matrix(c_method$getUnscaledFluxCCAnn())
    concentration.control.coefficients.scaled <- get_annotated_matrix(c_method$getScaledConcentrationCCAnn())
    concentration.control.coefficients.unscaled <- get_annotated_matrix(c_method$getUnscaledConcentrationCCAnn())
  }
  
  list(
    settings = settings,
    ss.result = ss.result,
    elasticities.scaled = elasticities.scaled,
    elasticities.unscaled = elasticities.unscaled,
    flux.control.coefficients.scaled = flux.control.coefficients.scaled,
    flux.control.coefficients.unscaled = flux.control.coefficients.unscaled,
    concentration.control.coefficients.scaled = concentration.control.coefficients.scaled,
    concentration.control.coefficients.unscaled = concentration.control.coefficients.unscaled
  )
}
