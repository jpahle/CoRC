#' Run a time course
#'
#' \code{runTimeCourse} runs a time course and returns the results in a list.
#'
#' The \href{https://jpahle.github.io/CoRC/articles/task_management.html}{online article on managing tasks} provides some further context.
#'
#' @param duration The time course duration, as number.
#' @param dt The time course output step size, as number.
#' @param intervals The time course step count, as count.
#' Overwrites \code{dt} in case of conflict.
#' @param suppress_output_before Whether to suppress before a certain time point, as number.
#' @param output_events Whether to output events as additional steps, as flag.
#' @param save_result_in_memory Whether to generate an output data frame for the time course, as flag.
#' @param start_in_steady_state Whether to first go to steady state before running the time course, as flag.
#' @param update_model Whether to update the model with the state resulting from the time course, as flag.
#' @param executable flag
#' @eval rox_method_param("Time-Course", "_p_CTrajectoryTask")
#' @param soft_error Whether to divert task processing errors to the \code{task_error} result entry, as flag.
#' Allows for readout of incomplete result tables for failed time courses.
#' @param model A model object.
#' @return A list of results.
#' \itemize{
#'   \item \code{$result} is a data frame containing time course output as concentrations.
#'   \item \code{$result_number} is a data frame containing time course output as particle numbers.
#'   \item \code{$task_error} is \code{NULL} or, in case \code{soft_error} is set to \code{TRUE} and the time course failed, a string giving an error message.
#' }
#' \strong{Attention}: Both result data frames will be empty if \code{save_result_in_memory} is set to \code{FALSE}.
#' Test the current value of this parameter with: \code{getTimeCourseSettings()$save_result_in_memory}.
#' @family time course
#' @export
runTimeCourse <- function(duration = NULL, dt = NULL, intervals = NULL, suppress_output_before = NULL, output_events = NULL, save_result_in_memory = NULL, start_in_steady_state = NULL, update_model = NULL, executable = NULL, method = NULL, soft_error = FALSE, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(is.flag(soft_error))
  
  # does assertions
  settings <- tc_assemble_settings(
    duration               = duration,
    dt                     = dt,
    intervals              = intervals,
    suppress_output_before = suppress_output_before,
    output_events          = output_events,
    save_result_in_memory  = save_result_in_memory,
    start_in_steady_state  = start_in_steady_state,
    update_model           = update_model,
    executable             = executable
  )
  
  c_task <- as(c_datamodel$getTask("Time-Course"), "_p_CTrajectoryTask")
  
  # does assertions
  method_settings <- tc_assemble_method(method, c_task)
  
  # try to avoid doing changes for performance reasons
  do_settings <- !is_empty(settings)
  do_method <- !is_empty(method_settings)
  
  c_model <- c_datamodel$getModel()
  
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
    
    compile_and_check(c_model)
    
    # initialize task
    assert_that(
      grab_msg(c_task$initializeRaw(OUTPUTFLAG)),
      msg = "Initializing the task failed."
    )
    
    # save current settings
    full_settings <- tc_get_settings(c_task)
    full_settings$method <- get_method_settings(c_method, with_name = TRUE)
    
    # run task
    task_error <- process_task(c_task, soft_error)
    
    # get results
    ret <- tc_get_results(c_task, full_settings)
    # append a task_error entry (NULL if no error)
    ret["task_error"] <- list(task_error)
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
#' The \href{https://jpahle.github.io/CoRC/articles/task_management.html}{online article on managing tasks} provides some further context.
#'
#' @param duration number
#' @param dt number
#' @param intervals count
#' @param suppress_output_before flag
#' @param output_events flag
#' @param save_result_in_memory flag
#' @param start_in_steady_state flag
#' @param update_model flag
#' @param executable flag
#' @eval rox_method_param("Time-Course", "_p_CTrajectoryTask")
#' @param model a model object
#' @family time course
#' @export
setTimeCourseSettings <- function(duration = NULL, dt = NULL, intervals = NULL, suppress_output_before = NULL, output_events = NULL, save_result_in_memory = NULL, start_in_steady_state = NULL, update_model = NULL, executable = NULL, method = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  # does assertions
  settings <- tc_assemble_settings(
    duration               = duration,
    dt                     = dt,
    intervals              = intervals,
    suppress_output_before = suppress_output_before,
    output_events          = output_events,
    save_result_in_memory  = save_result_in_memory,
    start_in_steady_state  = start_in_steady_state,
    update_model           = update_model,
    executable             = executable
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
#' The \href{https://jpahle.github.io/CoRC/articles/task_management.html}{online article on managing tasks} provides some further context.
#'
#' @param model a model object
#' @return A list of time course task settings including method options.
#' @family time course
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

new_copasi_ts <- function(settings, column_keys, unit_time, unit_conc, result, result_number) {
  assert_that(
    is.list(settings),
    is.character(column_keys),
    is.string(unit_time),
    is.string(unit_conc),
    is.data.frame(result),
    is.data.frame(result_number)
  )
  
  structure(
    list(
      settings      = settings,
      column_keys   = column_keys,
      units         = list(
        time          = unit_time,
        concentration = unit_conc
      ),
      result        = result,
      result_number = result_number
    ),
    class = "copasi_ts"
  )
}

#' @export
validate_copasi_ts <- function(x) {
  assert_that(
    is.list(x$settings),
    is.character(x$column_keys),
    is.list(x$units),
    is.string(x$units$time),
    is.string(x$units$concentration),
    is.data.frame(x$result),
    is.data.frame(x$result_number)
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
tc_assemble_settings <- function(duration, dt, intervals, suppress_output_before, output_events, save_result_in_memory, start_in_steady_state, update_model, executable) {
  assert_that(
    is.null(duration)               || is.number(duration)               && noNA(duration) && duration >= 0,
    is.null(dt)                     || is.number(dt)                     && noNA(dt)       && dt >= 0,
    is.null(intervals)              || is.count(intervals)               && noNA(intervals),
    is.null(suppress_output_before) || is.number(suppress_output_before) && noNA(suppress_output_before),
    is.null(output_events)          || is.flag(output_events)            && noNA(output_events),
    is.null(save_result_in_memory)  || is.flag(save_result_in_memory)    && noNA(save_result_in_memory),
    is.null(start_in_steady_state)  || is.flag(start_in_steady_state)    && noNA(start_in_steady_state),
    is.null(update_model)           || is.flag(update_model)             && noNA(update_model),
    is.null(executable)             || is.flag(executable)               && noNA(executable)
  )
  
  if (!is.null(dt) && !is.null(intervals))
    warning("Both `dt` and `intervals` was given. Applying `intervals` by preferentially.")
  
  list(
    duration               = duration,
    dt                     = dt,
    intervals              = intervals,
    suppress_output_before = suppress_output_before,
    output_events          = output_events,
    save_result_in_memory  = save_result_in_memory,
    start_in_steady_state  = start_in_steady_state,
    update_model           = update_model,
    executable             = executable
  ) %>%
    discard(is.null)
}

# does assertions
# returns a list of method settings
tc_assemble_method <- function(method, c_task) {
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
    method$method <- args_match(method$method, name = "method", valid_methods)
  }
  
  method
}

# gets full list of settings
tc_get_settings <- function(c_task) {
  c_problem <- as(c_task$getProblem(), "_p_CTrajectoryProblem")
  
  list(
    duration               = c_problem$getDuration(),
    dt                     = c_problem$getStepSize(),
    intervals              = c_problem$getStepNumber(),
    suppress_output_before = c_problem$getOutputStartTime(),
    output_events          = as.logical(c_problem$getOutputEvent()),
    save_result_in_memory  = as.logical(c_problem$timeSeriesRequested()),
    start_in_steady_state  = as.logical(c_problem$getStartInSteadyState()),
    update_model           = as.logical(c_task$isUpdateModel()),
    executable             = as.logical(c_task$isScheduled())
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
  
  if (!is.null(data$suppress_output_before))
    c_problem$setOutputStartTime(data$suppress_output_before)
  
  if (!is.null(data$output_events))
    c_problem$setOutputEvent(data$output_events)
  
  if (!is.null(data$save_result_in_memory))
    c_problem$setTimeSeriesRequested(data$save_result_in_memory)
  
  if (!is.null(data$start_in_steady_state))
    c_problem$setStartInSteadyState(data$start_in_steady_state)
  
  if (!is.null(data$update_model))
    c_task$setUpdateModel(data$update_model)
  
  if (!is.null(data$executable))
    c_task$setScheduled(data$executable)
}

# gathers all results
tc_get_results <- function(c_task, settings) {
  # c_datamodel is used in exported functions so make sure its safe
  c_datamodel <- make_dm_safe(c_task$getObjectDataModel())
  c_timeseries <- c_task$getTimeSeries()
  
  col_count <- c_timeseries$getNumVariables()
  row_count <- c_timeseries$getRecordedSteps()
  
  cl_col_objects <-
    seq_len_0(col_count) %>%
    map_chr(c_timeseries$getKey) %>%
    cop_key_to_obj()
  
  col_types <- map_swig_chr(cl_col_objects, "getObjectType")
  
  are_time_col <- col_types == "Model"
  are_metab_col <- col_types == "Metabolite"
  are_other_col <- !(are_time_col | are_metab_col)
  
  col_names <- map_chr(seq_len_0(col_count), c_timeseries$getTitle)
  
  col_keys <- character(col_count)
  col_keys[are_time_col] <- map_chr(which(are_time_col) - 1L, c_timeseries$getTitle)
  col_keys[are_metab_col] <- get_key(cl_col_objects[are_metab_col], is_species = TRUE)
  col_keys[are_other_col] <- get_key(cl_col_objects[are_other_col])
  
  # Timecritical step optimization
  timeSeries_ref <- c_timeseries@ref
  R_swig_CTimeSeries_getData <- getNativeSymbolInfo("R_swig_CTimeSeries_getData", "COPASI")[["address"]]
  R_swig_CTimeSeries_getConcentrationData <- getNativeSymbolInfo("R_swig_CTimeSeries_getConcentrationData", "COPASI")[["address"]]
  
  # read out all values
  data_val <-
    map(
      seq_len_0(col_count),
      function(i_col) {
        map_dbl(
          seq_len_0(row_count),
          function(i_row) {
            # Timecritical step optimization
            # timeSeries$getData(i_row, i_col)
            # CTimeSeries_getData(timeSeries, i_row, i_col)
            # args: self@ref, int, int, bool
            .Call(R_swig_CTimeSeries_getData, timeSeries_ref, i_row, i_col, FALSE)
          }
        )
      }
    )
  
  # read out all concentration data seperately
  data_conc_subset <-
    map(
      which(are_metab_col) - 1L,
      function(i_col) {
        map_dbl(
          seq_len_0(row_count),
          function(i_row) {
            # Timecritical step optimization
            # timeSeries$getConcentrationData(i_row, i_col)
            # CTimeSeries_getConcentrationData(timeSeries, i_row, i_col)
            # args: self@ref, int, int, bool
            .Call(R_swig_CTimeSeries_getConcentrationData, timeSeries_ref, i_row, i_col, FALSE)
          }
        )
      }
    )
    
  names(data_val) <- col_names
  
  data_val <- tibble::tibble(!!!data_val, .name_repair = "minimal")
  
  data_conc <- replace(data_val, are_metab_col, data_conc_subset)
  
  new_copasi_ts(
    settings      = settings,
    column_keys   = col_keys,
    unit_time     = getTimeUnit(c_datamodel),
    unit_conc     = paste0(getQuantityUnit(c_datamodel), " / ", getVolumeUnit(c_datamodel)),
    result        = data_conc,
    result_number = data_val
  )
}
