#' Run a time course
#'
#' \code{runTimeCourse} runs a time course and returns the results in a list.
#'
#' @param duration number
#' @param dt number
#' @param intervals count
#' @param suppressOutputBefore flag
#' @param outputEvents flag
#' @param saveResultInMemory flag
#' @param startInSteadyState flag
#' @param updateModel flag
#' @param executable flag
#' @param method string or list
#' @param model a model object
#' @return a list of results
#' @export
runTimeCourse <- function(duration = NULL, dt = NULL, intervals = NULL, suppressOutputBefore = NULL, outputEvents = NULL, saveResultInMemory = NULL, startInSteadyState = NULL, updateModel = NULL, executable = NULL, method = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  # does assertions
  settings <- tc_assemble_settings(
    duration = duration,
    dt = dt,
    intervals = intervals,
    suppressOutputBefore = suppressOutputBefore,
    outputEvents = outputEvents,
    saveResultInMemory = saveResultInMemory,
    startInSteadyState = startInSteadyState,
    updateModel = updateModel,
    executable = executable
  )
  
  c_task <- as(c_datamodel$getTask("Time-Course"), "_p_CTrajectoryTask")
  
  # does assertions
  method_settings <- tc_assemble_method(method, c_task)
  
  # try to avoid doing changes for performance reasons
  do_settings <- !is_empty(settings)
  do_method <- !is_empty(method_settings)
  
  tryCatch({
    # save all previous settings
    if (do_settings)
      pre_settings <- tc_get_settings(c_task)
    if (do_method) {
      # keep track of the originally set method
      pre_method <- c_task$getMethod()$getSubType()
      # change the method first, then save the settings for the new method
      if (!is.null(method_settings$method))
        c_task$setMethodType(method_settings$method)
      c_method <- as(c_task$getMethod(), "_p_CTrajectoryMethod")
      pre_method_settings <- get_method_settings(c_method, with_name = TRUE)
    } else {
      c_method <- as(c_task$getMethod(), "_p_CTrajectoryMethod")
    }
    
    # apply settings
    if (do_settings)
      tc_set_settings(settings, c_task)
    if (do_method)
      set_method_settings(method_settings, c_method)
    
    # initialize task
    assert_that(
      grab_msg(c_task$initializeRaw(OUTPUTFLAG)),
      msg = "Initializing the task failed."
    )
    
    # run task and save current settings
    full_settings <- tc_get_settings(c_task)
    full_settings$method <- get_method_settings(c_method, with_name = TRUE)
    assert_that(
      grab_msg(c_task$processRaw(TRUE)),
      msg = "Processing the task failed."
    )
    
    # get results
    ret <- tc_get_results(c_task, full_settings)
  },
  finally = {
    # revert all settings
    if (do_settings)
      tc_set_settings(pre_settings, c_task)
    if (do_method) {
      set_method_settings(pre_method_settings, c_method)
      c_task$setMethodType(pre_method)
    }
  })
  
  ret
}

#' Set time course settings
#'
#' \code{setTimeCourseSettings} sets time course task settings including method options.
#'
#' @param duration number
#' @param dt number
#' @param intervals count
#' @param suppressOutputBefore flag
#' @param outputEvents flag
#' @param saveResultInMemory flag
#' @param startInSteadyState flag
#' @param updateModel flag
#' @param executable flag
#' @param method string or list
#' @param model a model object
#' @export
setTimeCourseSettings <- function(duration = NULL, dt = NULL, intervals = NULL, suppressOutputBefore = NULL, outputEvents = NULL, saveResultInMemory = NULL, startInSteadyState = NULL, updateModel = NULL, executable = NULL, method = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  # does assertions
  settings <- tc_assemble_settings(
    duration = duration,
    dt = dt,
    intervals = intervals,
    suppressOutputBefore = suppressOutputBefore,
    outputEvents = outputEvents,
    saveResultInMemory = saveResultInMemory,
    startInSteadyState = startInSteadyState,
    updateModel = updateModel,
    executable = executable
  )
  
  c_task <- as(c_datamodel$getTask("Time-Course"), "_p_CTrajectoryTask")
  
  # does assertions
  method_settings <- tc_assemble_method(method, c_task)
  
  # switch to given method
  if (!is.null(method_settings$method))
    c_task$setMethodType(method_settings$method)
  
  c_method <- as(c_task$getMethod(), "_p_CTrajectoryMethod")
  
  tc_set_settings(settings, c_task)
  set_method_settings(method_settings, c_method)
  
  invisible()
}

#' Get time course settings
#'
#' \code{getTimeCourseSettings} gets time course task settings including method options.
#'
#' @param model a model object
#' @return A list of time course task settings including method options.
#' @export
getTimeCourseSettings <- function(model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  c_task <- as(c_datamodel$getTask("Time-Course"), "_p_CTrajectoryTask")
  c_method <- as(c_task$getMethod(), "_p_CTrajectoryMethod")
  
  ret <- tc_get_settings(c_task)
  ret$method <- get_method_settings(c_method, with_name = TRUE)
  
  ret
}

#' @rdname runTimeCourse
#' @export
runTC <- runTimeCourse
#' @rdname setTimeCourseSettings
#' @export
setTC <- setTimeCourseSettings
#' @rdname getTimeCourseSettings
#' @export
getTC <- getTimeCourseSettings

new_copasi_ts <- function(settings, unit_time, unit_conc, result) {
  assert_that(
    is.list(settings),
    is.string(unit_time),
    is.string(unit_conc),
    is.data.frame(result)
  )
  
  structure(
    list(
      settings = settings,
      units = list(time = unit_time, concentration = unit_conc),
      result = result
    ),
    class = "copasi_ts"
  )
}

#' @export
validate_copasi_ts <- function(x) {
  assert_that(
    is.list(x$settings),
    is.list(x$units),
    is.string(x$units$time),
    is.string(x$units$concentration),
    is.data.frame(x$result)
  )
}

#' @export
is.copasi_ts <- function(x) {
  inherits(x, "copasi_ts")
}

# The following functions should be the basis for implementation of any task
# They should allow for a common workflow with most tasks

# does assertions
# returns a list of settings
tc_assemble_settings <- function(duration, dt, intervals, suppressOutputBefore, outputEvents, saveResultInMemory, startInSteadyState, updateModel, executable) {
  assert_that(
    is.null(duration)             || is.number(duration)             && noNA(duration) && duration >= 0,
    is.null(dt)                   || is.number(dt)                   && noNA(dt)       && dt >= 0,
    is.null(intervals)            || is.count(intervals)             && noNA(intervals),
    is.null(suppressOutputBefore) || is.number(suppressOutputBefore) && noNA(suppressOutputBefore),
    is.null(outputEvents)         || is.flag(outputEvents)           && noNA(outputEvents),
    is.null(saveResultInMemory)   || is.flag(saveResultInMemory)     && noNA(saveResultInMemory),
    is.null(startInSteadyState)   || is.flag(startInSteadyState)     && noNA(startInSteadyState),
    is.null(updateModel)          || is.flag(updateModel)            && noNA(updateModel),
    is.null(executable)           || is.flag(executable)             && noNA(executable)
  )
  
  if (!is.null(dt) && !is.null(intervals))
    stop("Only one of dt and intervals can be given")
  
  list(
    duration = duration,
    dt = dt,
    intervals = intervals,
    suppressOutputBefore = suppressOutputBefore,
    outputEvents = outputEvents,
    saveResultInMemory = saveResultInMemory,
    startInSteadyState = startInSteadyState,
    updateModel = updateModel,
    executable = executable
  ) %>%
    discard(is.null)
}

# does assertions
# returns a list of method settings
tc_assemble_method <- function(method, c_task) {
  if (is.null(method))
    return(list())
  
  assert_that(is.string(method) || is.list(method))
  
  if (is_scalar_character(method))
    method <- list(method = method)
  
  if (has_name(method, "method"))
    # hack to get nice error message if method string is not accepted.
    method$method <- map_chr(method$method, function(method) {rlang::arg_match(method, names(.__E___CTaskEnum__Method)[c_task$getValidMethods() + 1L])})
  
  method
}

# gets full list of settings
tc_get_settings <- function(c_task) {
  c_problem <- as(c_task$getProblem(), "_p_CTrajectoryProblem")
  
  list(
    duration             = c_problem$getDuration(),
    dt                   = c_problem$getStepSize(),
    intervals            = c_problem$getStepNumber(),
    suppressOutputBefore = c_problem$getOutputStartTime(),
    outputEvents         = as.logical(c_problem$getOutputEvent()),
    saveResultInMemory   = as.logical(c_problem$timeSeriesRequested()),
    startInSteadyState   = as.logical(c_problem$getStartInSteadyState()),
    updateModel          = as.logical(c_task$isUpdateModel()),
    executable           = as.logical(c_task$isScheduled())
  )
}

# sets all settings given in list
tc_set_settings <- function(data, c_task) {
  if (is_empty(data))
    return()
  
  c_problem <- as(c_task$getProblem(), "_p_CTrajectoryProblem")
  
  if (!is.null(data$duration))
    c_problem$setDuration(data$duration)
  
  if (!is.null(data$dt))
    c_problem$setStepSize(data$dt)
  
  if (!is.null(data$intervals))
    c_problem$setStepNumber(data$intervals)
  
  if (!is.null(data$suppressOutputBefore))
    c_problem$setOutputStartTime(data$suppressOutputBefore)
  
  if (!is.null(data$outputEvents))
    c_problem$setOutputEvent(data$outputEvents)
  
  if (!is.null(data$saveResultInMemory))
    c_problem$setTimeSeriesRequested(data$saveResultInMemory)
  
  if (!is.null(data$startInSteadyState))
    c_problem$setStartInSteadyState(data$startInSteadyState)
  
  if (!is.null(data$updateModel))
    c_task$setUpdateModel(data$updateModel)
  
  if (!is.null(data$executable))
    c_task$setScheduled(data$executable)
}

# gathers all results
# needs to know settings results in memory are from current run
tc_get_results <- function(c_task, settings) {
  c_datamodel <- c_task$getObjectDataModel()
  c_timeseries <- c_task$getTimeSeries()
  recordedSteps <- c_timeseries$getRecordedSteps()
  
  ret <- NULL
  
  if (recordedSteps) {
    # Timecritical step optimization
    timeSeries_ref <- c_timeseries@ref
    R_swig_CTimeSeries_getConcentrationData <- getNativeSymbolInfo("R_swig_CTimeSeries_getConcentrationData", "COPASI")[["address"]]
    
    # assemble output dataframe
    # Iterates over all species/variables and all timepoints/steps
    # Inner loops creates numeric() wrapped in a named list
    # Outer loop creates list of columns for binding to data frame
    ret <-
      seq_len_0(c_timeseries$getNumVariables()) %>%
      map(function(i_var) {
        seq_len_0(recordedSteps) %>%
          map_dbl(function(i_step) {
            # Timecritical step optimization
            # timeSeries$getConcentrationData(i_step, i_var)
            # CTimeSeries_getConcentrationData(timeSeries, i_step, i_var)
            # args: self@ref, int, int, bool
            .Call(R_swig_CTimeSeries_getConcentrationData, timeSeries_ref, i_step, i_var, FALSE)
          }) %>%
          list() %>%
          # set_names(timeSeries$getTitle(i_var))
          set_names(CTimeSeries_getTitle(c_timeseries, i_var))
      }) %>%
      dplyr::bind_cols()
  } else if (!full_settings$saveResultInMemory)
    warning("No result generated because saveResultInMemory is set to FALSE in the model.")
  
  new_copasi_ts(
    settings = settings,
    unit_time = getTimeUnit(c_datamodel),
    unit_conc = paste0(getQuantityUnit(c_datamodel), " / ", getVolumeUnit(c_datamodel)),
    result = ret
  )
}
