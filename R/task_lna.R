#' Run linear noise approximation
#'
#' \code{runLinearNoiseApproximation} runs linear noise approximation and returns the results in a list.
#'
#' The \href{https://jpahle.github.io/CoRC/articles/task_management.html}{online article on managing tasks} provides some further context.
#'
#' @param perform_steady_state_analysis flag
#' @param executable flag
#' @param method list of further method arguments and their values.
#' @param model A model object.
#' @eval paste0("@return A list of results.
#' \\itemize{
#'   \\item \\code{$result_ss} can be one of ", rox_print_v(names(.__E___CSteadyStateMethod__ReturnCode)), ".
#'   \\item \\code{$result_lna} can be one of ", rox_print_v(names(.__E___CLNAMethod__EVStatus)), ".
#' }")
#' @family linear noise approximation
#' @export
runLinearNoiseApproximation <- function(perform_steady_state_analysis = NULL, executable = NULL, method = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  # does assertions
  settings <- lna_assemble_settings(
    perform_steady_state_analysis = perform_steady_state_analysis,
    executable                    = executable
  )
  
  # does assertions
  method_settings <- lna_assemble_method(method)
  
  # try to avoid doing changes for performance reasons
  do_settings <- !is_empty(settings)
  do_method <- !is_empty(method_settings)
  
  c_model <- c_datamodel$getModel()
  c_task <- as(c_datamodel$getTask("Linear Noise Approximation"), "_p_CLNATask")
  c_method_ss <- as(c_datamodel$getTask("Steady-State")$getMethod(), "_p_CSteadyStateMethod")
  
  # save all previous settings
  if (do_settings)
    pre_settings <- lna_get_settings(c_task)
  if (do_method)
    pre_method_settings <- get_method_settings(c_method_ss)
  
  tryCatch({
    # apply settings
    if (do_settings)
      lna_set_settings(settings, c_task)
    if (do_method)
      set_method_settings(method_settings, c_method_ss)
    
    compile_and_check(c_model)
    
    # initialize task
    assert_that(
      grab_msg(c_task$initializeRaw(OUTPUTFLAG)),
      msg = "Initializing the task failed."
    )
    
    # save current settings
    full_settings <- lna_get_settings(c_task)
    full_settings$method <- get_method_settings(c_method_ss)
    
    # run task
    process_task(c_task)
    
    # get results
    ret <- lna_get_results(c_task, full_settings)
  },
  finally = {
    # revert all settings
    if (do_settings)
      lna_set_settings(pre_settings, c_task)
    if (do_method)
      set_method_settings(pre_method_settings, c_method_ss)
  })
  
  ret
}

#' Set linear noise approximation settings
#'
#' \code{setLinearNoiseApproximation} sets linear noise approximation task settings including method options.
#'
#' The \href{https://jpahle.github.io/CoRC/articles/task_management.html}{online article on managing tasks} provides some further context.
#'
#' @param perform_steady_state_analysis flag
#' @param executable flag
#' @param method list
#' @param model a model object
#' @family linear noise approximation
#' @export
setLinearNoiseApproximationSettings <- function(perform_steady_state_analysis = NULL, executable = NULL, method = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  # does assertions
  settings <- lna_assemble_settings(
    perform_steady_state_analysis = perform_steady_state_analysis,
    executable                    = executable
  )
  
  # does assertions
  method_settings <- lna_assemble_method(method)
  
  c_task <- as(c_datamodel$getTask("Linear Noise Approximation"), "_p_CLNATask")
  c_method_ss <- as(c_datamodel$getTask("Steady-State")$getMethod(), "_p_CSteadyStateMethod")
  
  lna_set_settings(settings, c_task)
  set_method_settings(method_settings, c_method_ss)
  
  invisible()
}

#' Get linear noise approximation settings
#'
#' \code{getLinearNoiseApproximationSettings} gets linear noise approximation settings including method options.
#'
#' The \href{https://jpahle.github.io/CoRC/articles/task_management.html}{online article on managing tasks} provides some further context.
#'
#' @param model a model object
#' @return A list of metabolic control analysis task settings including method options.
#' @family linear noise approximation
#' @export
getLinearNoiseApproximationSettings <- function(model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  c_task <- as(c_datamodel$getTask("Linear Noise Approximation"), "_p_CLNATask")
  c_method_ss <- as(c_datamodel$getTask("Steady-State")$getMethod(), "_p_CSteadyStateMethod")
  
  ret <- lna_get_settings(c_task)
  ret$method <- get_method_settings(c_method_ss)
  
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
lna_assemble_settings <- function(perform_steady_state_analysis, executable) {
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
lna_assemble_method <- function(method) {
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
lna_get_settings <- function(c_task) {
  c_problem <- as(c_task$getProblem(), "_p_CLNAProblem")
  
  list(
    perform_steady_state_analysis = as.logical(c_problem$isSteadyStateRequested()),
    executable                    = as.logical(c_task$isScheduled())
  )
}

# sets all settings given in list
lna_set_settings <- function(data, c_task) {
  if (is_empty(data))
    return()
  
  c_problem <- as(c_task$getProblem(), "_p_CLNAProblem")
  
  if (!is.null(data$perform_steady_state_analysis))
    c_problem$setSteadyStateRequested(data$perform_steady_state_analysis)
  
  if (!is.null(data$executable))
    c_task$setScheduled(data$executable)
}

# gathers all results
lna_get_results <- function(c_task, settings) {
  c_method <- as(c_task$getMethod(), "_p_CLNAMethod")
  
  ss_result <- c_method$getSteadyStateStatus()
  lna_result <- c_method$getEigenValueStatus()
  
  covariance_matrix <- get_annotated_matrix(c_method$getCovarianceMatrixAnn())
  covariance_matrix_reduced <- get_annotated_matrix(c_method$getCovarianceMatrixReducedAnn())
  b_matrix_reduced <- get_annotated_matrix(c_method$getBMatrixReducedAnn())

  list(
    settings                  = settings,
    result_ss                 = ss_result,
    result_lna                = lna_result,
    covariance_matrix         = covariance_matrix,
    covariance_matrix_reduced = covariance_matrix_reduced,
    b_matrix_reduced          = b_matrix_reduced
  )
}
