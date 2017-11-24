#' Run optimization
#'
#' \code{runOptimization} runs optimization and returns the results in a list.
#' 
#' @param expression Expression to optimize, as string.
#' @param maximize flag
#' @eval paste0("@param subtask string
#' 
#' Available tasks: ", rox_print_v(task_enum), ".")
#' @param randomize_start_values flag
#' @param calculate_statistics flag
#' @param update_model flag
#' @param executable flag
#' @param parameters copasi_param or list of copasi_param objects
#' @eval rox_method_param("Optimization", "_p_COptTask")
#' @param model A model object.
#' @return A list of results.
#' @family optimization
#' @export
runOptimization <- function(expression = NULL, maximize = NULL, subtask = NULL, randomize_start_values = NULL, calculate_statistics = NULL, update_model = NULL, executable = NULL, parameters = NULL, method = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  # does assertions
  settings <- opt_assemble_settings(
    expression             = expression,
    maximize               = maximize,
    subtask                = subtask,
    randomize_start_values = randomize_start_values,
    calculate_statistics   = calculate_statistics,
    update_model           = update_model,
    executable             = executable
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
  
  c_model <- c_datamodel$getModel()
  
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
      addOptimizationParameter(parameter_list, model = c_datamodel)
    
    compile_and_check(c_model)
    
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
    
    # write back info from math container to model
    update_model_from_mc(c_task$getMathContainer())
    
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
#' @param expression Expression to optimize, as string.
#' @param maximize flag
#' @eval paste0("@param subtask string
#' 
#' Availables tasks: ", rox_print_v(task_enum), ".")
#' @param randomize_start_values flag
#' @param calculate_statistics flag
#' @param update_model flag
#' @param executable flag
#' @param parameters copasi_param or list of copasi_param objects
#' @eval rox_method_param("Optimization", "_p_COptTask")
#' @param model a model object
#' @family optimization
#' @export
setOptimizationSettings <- function(expression = NULL, maximize = NULL, subtask = NULL, randomize_start_values = NULL, calculate_statistics = NULL, update_model = NULL, executable = NULL, parameters = NULL, method = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  # does assertions
  settings <- opt_assemble_settings(
    expression             = expression,
    maximize               = maximize,
    subtask                = subtask,
    randomize_start_values = randomize_start_values,
    calculate_statistics   = calculate_statistics,
    update_model           = update_model,
    executable             = executable
  )
  
  c_task <- as(c_datamodel$getTask("Optimization"), "_p_COptTask")
  
  # does assertions
  method_settings <- opt_assemble_method(method, c_task)
  
  c_problem <- as(c_task$getProblem(), "_p_COptProblem")
  
  # does assertions
  parameter_list <- opt_assemble_parameters(parameters, c_problem)

  # parameters get rolled back when not setting them properly
  tryCatch(
    addOptimizationParameter(parameter_list, model = c_datamodel),
    error = function(e) {
      clearOptimizationParameters(c_datamodel)
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

new_copasi_parm <- function(x, start, lower, upper) {
  ret <- structure(
    list(
      ref   = x,
      start = start,
      lower = lower,
      upper = upper
    ),
    class = "copasi_parm"
  )
  
  validate_copasi_parm(ret)
  
  ret
}

#' @export
validate_copasi_parm <- function(x) {
  assert_that(
    all(hasName(x, c("ref", "start", "lower", "upper")))
  )
  
  with(x, {
    assert_that(
      is.string(ref),   noNA(ref),
      is.number(start), noNA(start),
      is.number(lower), noNA(lower),
      is.number(upper), noNA(upper),
      start < Inf,
      start > -Inf,
      lower <= start,
      start <= upper
    )
  })
}

#' @export
is.copasi_parm <- function(x) {
  inherits(x, "copasi_parm")
}

#' @export
copasi_parm <- function(ref, start_value = (lower_bound + upper_bound) / 2, lower_bound = 1e-6, upper_bound = 1e6) {
  new_copasi_parm(
    ref,
    start = start_value,
    lower = lower_bound,
    upper = upper_bound
  )
}

#' Define an optimization parameter
#' 
#' @param ref value reference
#' @param start_value start value
#' @param lower_bound lower value bound
#' @param upper_bound upper value bound
#' @return copasi_parm object for input into related functions
#' @seealso \code{\link{addOptimizationParameter}} \code{\link{clearOptimizationParameters}}
#' @family optimization
#' @export
defineOptimizationParameter <- copasi_parm

#' Add an optimization parameter
#' 
#' @param ... objects as returned by \code{\link{defineOptimizationParameter}}.
#' Alternatively, the same parameters as used by \code{\link{defineOptimizationParameter}}.
#' @param model a model object
#' @seealso \code{\link{defineOptimizationParameter}} \code{\link{clearOptimizationParameters}}
#' @family optimization
#' @export
addOptimizationParameter <- function(..., model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  # flatten all args into a single list
  # this list can be used to check if the user gave only copasi_parm
  arglist_compact <- rlang::squash(unname(list(...)))
  
  # if not all are copasi_parm, try handing the args to define... so we get copasi_parm
  if (!every(arglist_compact, is.copasi_parm))
    arglist_compact <- list(defineOptimizationParameter(...))
  
  walk(arglist_compact, validate_copasi_parm)
  
  cl_obj <-
    map_chr(arglist_compact, "ref") %>%
    map(xn_to_object, c_datamodel = c_datamodel)
  
  # Test if all refs are valid
  # This can probably be a more elaborate and safe test (by using dn_to_object(accepted_types))
  invalid_refs <- map_lgl(cl_obj, is.null)
  assert_that(
    !any(invalid_refs),
    msg = paste0("Given reference(s) ", paste0(which(invalid_refs), collapse = ", "), " are invalid for this model.")
  )
  
  c_task <- as(c_datamodel$getTask("Optimization"), "_p_COptTask")
  c_problem <- as(c_task$getProblem(), "_p_COptProblem")
  
  walk2(
    arglist_compact, cl_obj,
    ~ {
      c_optitem <- c_problem$addOptItem(.y$getCN())
      c_optitem$setStartValue(.x$start)
      c_optitem$setLowerBound(CCommonName(tolower(as.character(.x$lower))))
      c_optitem$setUpperBound(CCommonName(tolower(as.character(.x$upper))))
    }
  )
  
  invisible()
}

#' Clear all optimization parameters
#' 
#' @param model a model object
#' @seealso \code{\link{addOptimizationParameter}} \code{\link{defineOptimizationParameter}}
#' @family optimization
#' @export
clearOptimizationParameters <- function(model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  c_task <- as(c_datamodel$getTask("Optimization"), "_p_COptTask")
  c_problem <- as(c_task$getProblem(), "_p_COptProblem")
  
  walk(
    seq_len_0(c_problem$getOptItemSize()),
    ~ c_problem$removeOptItem(0L)
  )
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
opt_assemble_settings <- function(expression, maximize, subtask, randomize_start_values, calculate_statistics, update_model, executable) {
  assert_that(
    is.null(expression)             || is.scalar(expression)           && is.cexpression(expression) && noNA(expression),
    is.null(maximize)               || is.flag(maximize)               && noNA(maximize),
    is.null(subtask)                || is.string(subtask),
    is.null(randomize_start_values) || is.flag(randomize_start_values) && noNA(randomize_start_values),
    is.null(calculate_statistics)   || is.flag(calculate_statistics)   && noNA(calculate_statistics),
    is.null(update_model)           || is.flag(update_model)           && noNA(update_model),
    is.null(executable)             || is.flag(executable)             && noNA(executable)
  )
  
  if (!is.null(subtask)) {
    subtask <- rlang::arg_match(subtask, task_enum)
  }
  
  if (!is.null(expression))
    expression <- to_cexpr(expression)
  
  list(
    expression             = expression,
    maximize               = maximize,
    subtask                = subtask,
    randomize_start_values = randomize_start_values,
    calculate_statistics   = calculate_statistics,
    update_model           = update_model,
    executable             = executable
  ) %>%
    discard(is.null)
}

# does assertions
# returns a list of method settings
opt_assemble_method <- function(method, c_task) {
  if (is.null(method))
    return(list())
  
  assert_that(
    is.string(method) || is.list(method) && (is_empty(method) || !is.null(names(method))),
    msg = "method must be a string (a length one character vector) or a named list."
  )
  
  if (is_scalar_character(method))
    method <- list(method = method)
  
  if (hasName(method, "method")) {
    valid_methods <- names(.__E___CTaskEnum__Method)[c_task$getValidMethods() + 1L]
    # hack to get nice error message if method string is not accepted.
    method$method <- (function(method) rlang::arg_match(method, valid_methods))(method$method)
  }
  
  method
}

# gets full list of settings
opt_get_settings <- function(c_task) {
  c_datamodel <- c_task$getObjectDataModel()
  c_problem <- as(c_task$getProblem(), "_p_COptProblem")
  
  list(
    expression             = read_expr(c_problem$getObjectiveFunction(), c_datamodel),
    maximize               = as.logical(c_problem$maximize()),
    subtask                = c_problem$getSubtaskType(),
    randomize_start_values = as.logical(c_problem$getRandomizeStartValues()),
    calculate_statistics   = as.logical(c_problem$getCalculateStatistics()),
    update_model           = as.logical(c_task$isUpdateModel()),
    executable             = as.logical(c_task$isScheduled())
  )
}

# sets all settings given in list
opt_set_settings <- function(data, c_task) {
  if (is_empty(data))
    return()
  
  c_problem <- as(c_task$getProblem(), "_p_COptProblem")
  
  if (!is.null(data$expression)) {
    c_datamodel <- c_task$getObjectDataModel()
    assert_that(
      grab_msg(c_problem$setObjectiveFunction(write_expr(data$expression, c_datamodel))),
      msg = "Error when setting optimization expression."
    )
  }
  
  if (!is.null(data$maximize))
    c_problem$setMaximize(data$maximize)
  
  if (!is.null(data$subtask))
    c_problem$setSubtaskType(data$subtask)
  
  if (!is.null(data$randomize_start_values))
    c_problem$setRandomizeStartValues(data$randomize_start_values)
  
  if (!is.null(data$calculate_statistics))
    c_problem$setCalculateStatistics(data$calculate_statistics)
  
  if (!is.null(data$update_model))
    c_task$setUpdateModel(data$update_model)
  
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
      "Objective Value"          = c_problem$getSolutionValue(),
      "Function Evaluations"     = evals,
      "CPU Time [s]"             = evaltime,
      "Evaluations/second [1/s]" = evals / evaltime
    ) %>%
    transform_names()
  
  parameters <-
    tibble::tibble(
      "Parameter"   = get_key(cl_items),
      "Lower Bound" = map_swig_dbl(cl_items, "getLowerBoundValue"),
      "Start Value" = map_swig_dbl(cl_items, "getStartValue"),
      "Value"       = get_cv(c_problem$getSolutionVariables()),
      "Upper Bound" = map_swig_dbl(cl_items, "getUpperBoundValue"),
      "Gradient"    = get_cv(c_problem$getVariableGradients())
    ) %>%
    transform_names()
  
  protocol <- c_method$getMethodLog()$getPlainLog()
  
  list(
    settings   = settings,
    main       = main,
    parameters = parameters,
    protocol   = protocol
  )
}
