#' Run linear noise approximation
#'
#' \code{runLinearNoiseApproximation} runs linear noise approximation and returns the results in a list.
#'
#' @param performSteadyStateAnalysis flag
#' @param executable flag
#' @param method list
#' @param model a model object
#' @return a list of results
#' @family linear noise approximation
#' @export
runLinearNoiseApproximation <- function(performSteadyStateAnalysis = NULL, executable = NULL, method = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  # does assertions
  settings <- lna_assemble_settings(
    performSteadyStateAnalysis = performSteadyStateAnalysis,
    executable = executable
  )
  
  c_task <- as(c_datamodel$getTask("Linear Noise Approximation"), "_p_CLNATask")
  
  # does assertions
  method_settings <- lna_assemble_method(method, c_task)
  
  # try to avoid doing changes for performance reasons
  do_settings <- !is_empty(settings)
  do_method <- !is_empty(method_settings)
  
  c_method <- as(c_task$getMethod(), "_p_CLNAMethod")
  
  # save all previous settings
  if (do_settings)
    pre_settings <- lna_get_settings(c_task)
  if (do_method)
    pre_method_settings <- get_method_settings(c_method)
  
  tryCatch({
    # apply settings
    if (do_settings)
      lna_set_settings(settings, c_task)
    if (do_method)
      set_method_settings(method_settings, c_method)
    
    # initialize task
    assert_that(
      grab_msg(c_task$initializeRaw(OUTPUTFLAG)),
      msg = "Initializing the task failed."
    )
    
    # run task and save current settings
    full_settings <- lna_get_settings(c_task)
    full_settings$method <- get_method_settings(c_method)
    assert_that(
      grab_msg(c_task$processRaw(TRUE)),
      msg = "Processing the task failed."
    )
    
    # get results
    ret <- lna_get_results(c_task, full_settings)
  },
  finally = {
    # revert all settings
    if (do_settings)
      lna_set_settings(pre_settings, c_task)
    if (do_method)
      set_method_settings(pre_method_settings, c_method)
  })
  
  ret
}

#' Set linear noise approximation settings
#'
#' \code{setLinearNoiseApproximation} sets linear noise approximation task settings including method options.
#'
#' @param performSteadyStateAnalysis flag
#' @param executable flag
#' @param method list
#' @param model a model object
#' @family linear noise approximation
#' @export
setLinearNoiseApproximationSettings <- function(performSteadyStateAnalysis = NULL, executable = NULL, method = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  # does assertions
  settings <- ss_assemble_settings(
    performSteadyStateAnalysis = performSteadyStateAnalysis,
    executable = executable
  )
  
  c_task <- as(c_datamodel$getTask("Linear Noise Approximation"), "_p_CLNATask")
  
  # does assertions
  method_settings <- lna_assemble_method(method, c_task)
  
  c_method <- as(c_task$getMethod(), "_p_CLNAMethod")
  
  lna_set_settings(settings, c_task)
  set_method_settings(method_settings, c_method)
  
  invisible()
}

#' Get linear noise approximation settings
#'
#' \code{getLinearNoiseApproximationSettings} gets linear noise approximation settings including method options.
#'
#' @param model a model object
#' @return A list of metabolic control analysis task settings including method options.
#' @family linear noise approximation
#' @export
getLinearNoiseApproximationSettings <- function(model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  c_task <- as(c_datamodel$getTask("Linear Noise Approximation"), "_p_CLNATask")
  c_method <- as(c_task$getMethod(), "_p_CLNAMethod")
  
  ret <- lna_get_settings(c_task)
  ret$method <- get_method_settings(c_method)
  
  ret
}

#' @rdname runLinearNoiseApproximation
#' @export
runLNA <- runLinearNoiseApproximation
#' @rdname setLinearNoiseApproximationSettings
#' @export
setLNA <- setLinearNoiseApproximationSettings
#' @rdname getLinearNoiseApproximationSettings
#' @export
getLNA <- getLinearNoiseApproximationSettings

# The following functions should be the basis for implementation of any task
# They should allow for a common workflow with most tasks

# does assertions
# returns a list of settings
lna_assemble_settings <- function(performSteadyStateAnalysis, executable) {
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
lna_assemble_method <- function(method, c_task) {
  if (is.null(method))
    return(list())
  
  assert_that(is.list(method), !has_name(method, "method"))
  
  method
}

# gets full list of settings
lna_get_settings <- function(c_task) {
  c_problem <- as(c_task$getProblem(), "_p_CLNAProblem")
  
  list(
    performSteadyStateAnalysis = as.logical(c_problem$isSteadyStateRequested()),
    executable                 = as.logical(c_task$isScheduled())
  )
}

# sets all settings given in list
lna_set_settings <- function(data, c_task) {
  if (is_empty(data))
    return()
  
  c_problem <- as(c_task$getProblem(), "_p_CLNAProblem")
  
  if (!is.null(data$performSteadyStateAnalysis))
    c_problem$setSteadyStateRequested(data$performSteadyStateAnalysis)
  
  if (!is.null(data$executable))
    c_task$setScheduled(data$executable)
}

# gathers all results
lna_get_results <- function(c_task, settings) {
  c_method <- as(c_task$getMethod(), "_p_CLNAMethod")
  
  ss.result <- c_method$getSteadyStateStatus()
  lna.result <- c_method$getEigenValueStatus()
  
  covariance.matrix <- get_annotated_matrix(c_method$getCovarianceMatrixAnn())
  covariance.matrix.reduced <- get_annotated_matrix(c_method$getCovarianceMatrixReducedAnn())
  b.matrix.reduced <- get_annotated_matrix(c_method$getBMatrixReducedAnn())

  list(
    settings = settings,
    ss.result = ss.result,
    covariance.matrix = covariance.matrix,
    covariance.matrix.reduced = covariance.matrix.reduced,
    b.matrix.reduced = b.matrix.reduced
  )
}
