#' Run optimization
#'
#' \code{runOptimization} runs optimization and returns the results in a list.
#' 
#' @param expression string
#' @param maximize flag
#' @eval rox_param("subtask", "string", task_enum)
#' @param randomizeStartValues flag
#' @param calculateStatistics flag
#' @param updateModel flag
#' @param executable flag
#' @param parameters copasi_param or list of copasi_param objects
#' @eval rox_method_param("Optimization", "_p_COptTask")
#' @param model a model object
#' @return a list of results
#' @family optimization
#' @export
runOptimization <- function(expression = NULL, maximize = NULL, subtask = NULL, randomizeStartValues = NULL, calculateStatistics = NULL, updateModel = NULL, executable = NULL, parameters = NULL, method = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  # does assertions
  settings <- opt_assemble_settings(
    expression           = expression,
    maximize             = maximize,
    subtask              = subtask,
    randomizeStartValues = randomizeStartValues,
    calculateStatistics  = calculateStatistics,
    updateModel          = updateModel,
    executable           = executable
  )
  
  c_task <- as(c_datamodel$getTask("Optimization"), "_p_COptTask")
  
  # does assertions
  method_settings <- opt_assemble_method(method, c_task)
  
  c_problem <- as(c_task$getProblem(), "_p_COptProblem")
  
  # does assertions
  parameter_list <- opt_assemble_parameters(parameters, c_problem)

  # try to avoid doing changes for performance reasons
  do_settings <- !is_empty(settings)
  do_method <- !is_empty(method_settings)
  do_parameters <- !is_empty(parameter_list)
  
  tryCatch({
    # save all previous settings
    if (do_settings)
      pre_settings <- opt_get_settings(c_task)
    if (do_method) {
      # keep track of the originally set method
      pre_method <- c_task$getMethod()$getSubType()
      # change the method first, then save the settings for the new method
      if (!is.null(method_settings$method))
        c_task$setMethodType(method_settings$method)
      c_method <- as(c_task$getMethod(), "_p_COptMethod")
      pre_method_settings <- get_method_settings(c_method, with_name = TRUE)
    } else {
      c_method <- as(c_task$getMethod(), "_p_COptMethod")
    }
    
    # apply settings
    if (do_settings)
      opt_set_settings(settings, c_task)
    if (do_method)
      set_method_settings(method_settings, c_method)
    if (do_parameters)
      walk(parameter_list, addOptimizationParameter, c_datamodel)
    
    # initialize task
    assert_that(
      grab_msg(c_task$initializeRaw(OUTPUTFLAG)),
      msg = "Initializing the task failed."
    )
    
    # run task and save current settings
    full_settings <- opt_get_settings(c_task)
    full_settings$method <- get_method_settings(c_method, with_name = TRUE)
    assert_that(
      grab_msg(c_task$processRaw(TRUE)),
      msg = "Processing the task failed."
    )
    
    # get results
    ret <- opt_get_results(c_task, full_settings)
  },
  finally = {
    # revert all settings
    if (do_settings)
      opt_set_settings(pre_settings, c_task)
    if (do_method) {
      set_method_settings(pre_method_settings, c_method)
      c_task$setMethodType(pre_method)
    }
    if (do_parameters)
      clearOptimizationParameters()
  })
  
  ret
}

#' Set optimization settings
#'
#' \code{setOptimizationSettings} sets optimization task settings including parameters, experiments and method options.
#'
#' @param randomizeStartValues flag
#' @param createParameterSets flag
#' @param calculateStatistics flag
#' @param updateModel flag
#' @param executable flag
#' @param parameters copasi_param or list of copasi_param objects
#' @param experiments copasi_exp or list of copasi_exp objects
#' @eval rox_method_param("Optimization", "_p_COptTask")
#' @param model a model object
#' @family optimization
#' @export
setOptimizationSettings <- function(randomizeStartValues = NULL, createParameterSets = NULL, calculateStatistics = NULL, updateModel = NULL, executable = NULL, parameters = NULL, experiments = NULL, method = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  # does assertions
  settings <- pe_assemble_settings(
    randomizeStartValues = randomizeStartValues,
    createParameterSets = createParameterSets,
    calculateStatistics = calculateStatistics,
    updateModel = updateModel,
    executable = executable
  )
  
  c_task <- as(c_datamodel$getTask("Optimization"), "_p_COptTask")
  
  # does assertions
  method_settings <- opt_assemble_method(method, c_task)
  
  c_problem <- as(c_task$getProblem(), "_p_COptProblem")
  
  # does assertions
  parameter_list <- opt_assemble_parameters(parameters, c_problem)

  # parameters get rolled back when not setting them properly
  tryCatch(
    walk(parameter_list, addOptimizationParameter, c_datamodel),
    error = function(e) {
      clearParameters(c_datamodel)
      base::stop(e)
      # stop("Failed when applying parameters.")
    }
  )

  # switch to given method
  if (!is.null(method_settings$method))
    c_task$setMethodType(method_settings$method)
  
  c_method <- as(c_task$getMethod(), "_p_COptMethod")
  
  opt_set_settings(settings, c_task)
  set_method_settings(method_settings, c_method)
  
  invisible()
}

#' Set optimization settings
#'
#' \code{getOptimizationSettings} gets optimization task settings including method options.
#'
#' @param model a model object
#' @return A list of parameter estimation task settings including method options.
#' @family optimization
#' @export
getOptimizationSettings <- function(model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  c_task <- as(c_datamodel$getTask("Optimization"), "_p_COptTask")
  c_method <- as(c_task$getMethod(), "_p_COptMethod")
  
  ret <- opt_get_settings(c_task)
  ret$method <- get_method_settings(c_method, with_name = TRUE)
  
  ret
}

#' @rdname runOptimization
#' @export
runOpt <- runOptimization
#' @rdname setOptimizationSettings
#' @export
setOpt<- setOptimizationSettings
#' @rdname getOptimizationSettings
#' @export
getOpt <- getOptimizationSettings

new_copasi_parm <- function(x, lower, upper, start) {
  assert_that(
    is.string(x),
    is.number(lower),
    is.number(upper),
    is.number(start),
    lower <= start,
    start <= upper
  )
  
  structure(
    list(
      key = x,
      lower = lower,
      upper = upper,
      start = start
    ),
    class = "copasi_parm"
  )
}

#' @export
validate_copasi_parm <- function(x) {
  assert_that(
    is.string(x$key),
    is.number(x$lower),
    is.number(x$upper),
    is.number(x$start),
    x$lower <= x$start,
    x$start <= x$upper
  )
}

#' @export
is.copasi_parm <- function(x) {
  inherits(x, "copasi_parm")
}

#' @export
copasi_parm <- function(key = NULL, lower.bound = 1e-6, upper.bound = 1e6, start.value = (lower.bound + upper.bound) / 2) {
  new_copasi_parm(
    key,
    lower = lower.bound,
    upper = upper.bound,
    start = start.value
  )
}

#' Define an optimization parameter
#' 
#' @param key entity key
#' @param lower.bound lower value bound
#' @param upper.bound upper value bound
#' @param start.value start value
#' @return copasi_parm object for input into related functions
#' @seealso \code{\link{addOptimizationParameter}} \code{\link{clearOptimizationParameters}}
#' @family optimization
#' @export
defineOptimizationParameter <- copasi_parm

#' Add an optimization parameter
#' 
#' @param copasi_parm object as returned by \code{\link{defineOptimizationParameter}}
#' @param model a model object
#' @seealso \code{\link{defineOptimizationParameter}} \code{\link{clearOptimizationParameters}}
#' @family optimization
#' @export
addOptimizationParameter <- function(copasi_parm, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(
    is.copasi_parm(copasi_parm)
  )
  validate_copasi_parm(copasi_parm)
  
  c_obj <- xn_to_object(copasi_parm$key, c_datamodel)
  # Test if the key is valid
  # This can probably be a more elaborate and safe test (by using dn_to_object(accepted_types))
  assert_that(
    !is.null(c_obj),
    msg = "The given parameter is invalid for this model"
  )
  
  c_task <- as(c_datamodel$getTask("Optimization"), "_p_COptTask")
  c_problem <- as(c_task$getProblem(), "_p_COptProblem")
  
  c_optitem <- c_problem$addOptItem(c_obj$getCN())
  c_optitem$setLowerBound(CCommonName(as.character(copasi_parm$lower)))
  c_optitem$setUpperBound(CCommonName(as.character(copasi_parm$upper)))
  c_optitem$setStartValue(copasi_parm$start)
}

#' @seealso \code{\link{addOptimizationParameter}}
#' @export
clearOptimizationParameters <- function(model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  c_task <- as(c_datamodel$getTask("Optimization"), "_p_COptTask")
  c_problem <- as(c_task$getProblem(), "_p_COptProblem")
  
  seq_len_0(c_problem$getOptItemSize()) %>% walk(~ c_problem$removeOptItem(0))
}

opt_assemble_parameters <- function(parameters, c_problem) {
  assert_that(is.null(parameters) || is.list(parameters) && every(parameters, is.copasi_parm) || is.copasi_parm(parameters))
  
  if (is.copasi_parm(parameters))
    parameters <- list(parameters)
  
  if (is_empty(parameters))
    return(list())
  
  assert_that(
    c_problem$getOptItemSize() == 0L,
    msg = "This function can not set parameters if there are already parameters set in copasi."
  )
  
  walk(parameters, validate_copasi_parm)
  
  parameters
}

# The following functions should be the basis for implementation of any task
# They should allow for a common workflow with most tasks

# does assertions
# returns a list of settings
opt_assemble_settings <- function(expression, maximize, subtask, randomizeStartValues, calculateStatistics, updateModel, executable) {
  assert_that(
    is.null(expression)           || is.string(expression)         && noNA(expression),
    is.null(maximize)             || is.flag(maximize)             && noNA(maximize),
    is.null(subtask)              || is.string(subtask),
    is.null(randomizeStartValues) || is.flag(randomizeStartValues) && noNA(randomizeStartValues),
    is.null(calculateStatistics)  || is.flag(calculateStatistics)  && noNA(calculateStatistics),
    is.null(updateModel)          || is.flag(updateModel)          && noNA(updateModel),
    is.null(executable)           || is.flag(executable)           && noNA(executable)
  )
  
  if (!is.null(subtask)) {
    subtask <- rlang::arg_match(subtask, task_enum)
  }
  
  list(
    expression = expression,
    maximize = maximize,
    subtask = subtask,
    randomizeStartValues = randomizeStartValues,
    calculateStatistics = calculateStatistics,
    updateModel = updateModel,
    executable = executable
  ) %>%
    discard(is.null)
}

# does assertions
# returns a list of method settings
opt_assemble_method <- function(method, c_task) {
  if (is.null(method))
    return(list())
  
  assert_that(is.string(method) || is.list(method))
  
  if (is_scalar_character(method))
    method <- list(method = method)
  
  if (has_name(method, "method"))
    # hack to get nice error message if method string is not accepted.
    method$method <- method$method %>% (function(method) rlang::arg_match(method, names(.__E___CTaskEnum__Method)[c_task$getValidMethods() + 1L]))
  
  method
}

# gets full list of settings
opt_get_settings <- function(c_task) {
  c_datamodel <- c_task$getObjectDataModel()
  c_problem <- as(c_task$getProblem(), "_p_COptProblem")
  
  list(
    expression           = read_expr(c_problem$getObjectiveFunction(), c_datamodel),
    maximize             = as.logical(c_problem$maximize()),
    subtask              = c_problem$getSubtaskType(),
    randomizeStartValues = as.logical(c_problem$getRandomizeStartValues()),
    calculateStatistics  = as.logical(c_problem$getCalculateStatistics()),
    updateModel          = as.logical(c_task$isUpdateModel()),
    executable           = as.logical(c_task$isScheduled())
  )
}

# sets all settings given in list
opt_set_settings <- function(data, c_task) {
  if (is_empty(data))
    return()
  
  c_problem <- as(c_task$getProblem(), "_p_COptProblem")
  
  if (!is.null(data$expression))
    c_datamodel <- c_task$getObjectDataModel()
    c_problem$setObjectiveFunction(write_expr(data$expression, c_datamodel))
  
  if (!is.null(data$maximize))
    c_problem$setMaximize(data$maximize)
  
  if (!is.null(data$subtask))
    c_problem$setSubtaskType(data$subtask)
  
  if (!is.null(data$randomizeStartValues))
    c_problem$setRandomizeStartValues(data$randomizeStartValues)
  
  if (!is.null(data$calculateStatistics))
    c_problem$setCalculateStatistics(data$calculateStatistics)
  
  if (!is.null(data$updateModel))
    c_task$setUpdateModel(data$updateModel)
  
  if (!is.null(data$executable))
    c_task$setScheduled(data$executable)
}

# gathers all results
opt_get_results <- function(c_task, settings) {
  c_problem <- as(c_task$getProblem(), "_p_COptProblem")
  c_method <- as(c_task$getMethod(), "_p_COptMethod")
  
  cl_items <- get_sv(c_problem$getOptItemList()) %>% map(as, Class = "_p_COptItem")
  
  evals <- c_problem$getFunctionEvaluations()
  evaltime <- c_problem$getExecutionTime()
  
  main <-
    list(
      "Objective Value" = c_problem$getSolutionValue(),
      "Function Evaluations" = evals,
      "CPU Time [s]" = evaltime,
      "Evaluations/second [1/s]" = evals / evaltime
    ) %>%
    transform_names()
  
  parameters <-
    tibble::tibble(
      "Parameter" = map_swig_chr(cl_items, "getObjectDisplayName"),
      "Lower Bound" = map_swig_dbl(cl_items, "getLowerBoundValue"),
      "Start Value" = map_swig_dbl(cl_items, "getStartValue"),
      "Value" = get_cv(c_problem$getSolutionVariables()),
      "Upper Bound" = map_swig_dbl(cl_items, "getUpperBoundValue"),
      "Gradient" = get_cv(c_problem$getVariableGradients())
    ) %>%
    transform_names()
  
  protocol <- c_method$getMethodLog()$getPlainLog()
  
  list(
    settings = settings,
    main = main,
    parameters = parameters,
    protocol = protocol
  )
}

