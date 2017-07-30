#' Run a time course
#'
#' \code{runTimeCourse} runs a time course and returns the time course data as a data frame.
#'
#' @param duration numeric time course duration
#' @param dt numeric
#' @param intervals integer
#' @param suppressOutputBefore boolean
#' @param outputEvents boolean
#' @param saveResultInMemory boolean
#' @param startInSteadyState boolean
#' @param updateModel boolean
#' @param method character or list
#' @param datamodel a model object
#' @return a data frame with a time column and value columns
#' @export
runTimeCourse <- function(duration = NULL, dt = NULL, intervals = NULL, suppressOutputBefore = NULL, outputEvents = NULL, saveResultInMemory = NULL, startInSteadyState = NULL, updateModel = NULL, method = NULL, datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  
  # use the worker function to apply all given arguments
  # the worker function returns all args needed to restore previous settings
  restorationCall <- tc_settings_worker(
    .type = "temporary",
    duration = duration,
    dt = dt,
    intervals = intervals,
    suppressOutputBefore = suppressOutputBefore,
    outputEvents = outputEvents,
    saveResultInMemory = saveResultInMemory,
    startInSteadyState = startInSteadyState,
    updateModel = updateModel,
    method = method,
    datamodel = datamodel
  )
  
  task <- as(datamodel$getTask("Time-Course"), "_p_CTrajectoryTask")
  
  success <- grab_msg(task$initializeRaw(OUTPUTFLAG))
  if (success)
    success <- grab_msg(task$processRaw(TRUE))
  if (success)
    ret <- tc_result_worker(datamodel, saveResultInMemory)

  # Call the worker again to restore previous settings.
  do.call(tc_settings_worker, restorationCall)
  
  # Assertions only after restoration of settings
  assert_that(
    success,
    msg = paste0("Processing the task failed.")
  )
  
  ret
}

#' Set time course settings
#'
#' \code{setTimeCourseSettings} sets time course settings including method options.
#'
#' @param duration numeric time course duration
#' @param dt numeric
#' @param intervals integer
#' @param suppressOutputBefore boolean
#' @param outputEvents boolean
#' @param saveResultInMemory boolean
#' @param startInSteadyState boolean
#' @param updateModel boolean
#' @param executable boolean
#' @param method character or list
#' @param datamodel a model object
#' @export
setTimeCourseSettings <- function(duration = NULL, dt = NULL, intervals = NULL, suppressOutputBefore = NULL, outputEvents = NULL, saveResultInMemory = NULL, startInSteadyState = NULL, updateModel = NULL, executable = NULL, method = NULL, datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  assert_that(is.null(executable) || is.flag(executable) && !is.na(executable))
  
  # Call the worker to set most settings
  tc_settings_worker(
    .type = "permanent",
    duration = duration,
    dt = dt,
    intervals = intervals,
    suppressOutputBefore = suppressOutputBefore,
    outputEvents = outputEvents,
    saveResultInMemory = saveResultInMemory,
    startInSteadyState = startInSteadyState,
    updateModel = updateModel,
    method = method,
    datamodel = datamodel
  )
  
  task <- as(datamodel$getTask("Time-Course"), "_p_CTrajectoryTask")
  
  if (!is.null(executable)) {
    task$setScheduled(executable)
  }
  
  invisible()
}

# .type can help in some situations to determine assertions etc
# can be "temporary", "permanent" or "restore"
tc_settings_worker <- function(.type, duration = NULL, dt = NULL, intervals = NULL, suppressOutputBefore = NULL, outputEvents = NULL, saveResultInMemory = NULL, startInSteadyState = NULL, updateModel = NULL, method = NULL, method_old = NULL, datamodel) {
  task <- as(datamodel$getTask("Time-Course"), "_p_CTrajectoryTask")
  problem <- as(task$getProblem(), "_p_CTrajectoryProblem")
  
  assert_that(
    is.null(duration)             || is.number(duration)             && duration >= 0,
    is.null(dt)                   || is.number(dt)                   && dt >= 0,
    is.null(intervals)            || is.count(intervals),
    is.null(suppressOutputBefore) || is.number(suppressOutputBefore) && !is.na(suppressOutputBefore),
    is.null(outputEvents)         || is.flag(outputEvents)           && !is.na(outputEvents),
    is.null(saveResultInMemory)   || is.flag(saveResultInMemory)     && !is.na(saveResultInMemory),
    is.null(startInSteadyState)   || is.flag(startInSteadyState)     && !is.na(startInSteadyState),
    is.null(updateModel)          || is.flag(updateModel)            && !is.na(updateModel),
    is.null(method)               || is.string(method)               && !is.na(method) || is.list(method) && is.string(method$method) && !is.na(method$method)
  )
  assert_that(.type == "restore" || !(!is.null(dt) && !is.null(intervals)), msg = "Only one of dt and intervals can be given")
  
  if (!is.null(method)) {
    if (is_scalar_character(method)) method <- list(method = method)
    # hack to get nice error message if method string is not accepted.
    with(method, rlang::arg_match(method, names(.__E___CTaskEnum__Method)[task$getValidMethods() + 1L]))
  }
  
  errors <- FALSE
  
  restorationCall <- list(
    .type = "restore",
    datamodel = datamodel
  )
  
  if (!is.null(duration)) {
    restorationCall$duration <- problem$getDuration()
    problem$setDuration(duration)
  }
  
  if (!is.null(dt)) {
    restorationCall$dt <- problem$getStepSize()
    problem$setStepSize(dt)
  }
  
  if (!is.null(intervals)) {
    restorationCall$intervals <- problem$getStepNumber()
    problem$setStepNumber(intervals)
  }
  
  if (!is.null(suppressOutputBefore)) {
    restorationCall$suppressOutputBefore <- problem$getOutputStartTime()
    problem$setOutputStartTime(suppressOutputBefore)
  }
  
  if (!is.null(outputEvents)) {
    restorationCall$outputEvents <- as.logical(problem$getOutputEvent())
    problem$setOutputEvent(outputEvents)
  }
  
  if (!is.null(saveResultInMemory)) {
    restorationCall$saveResultInMemory <- as.logical(problem$timeSeriesRequested())
    problem$setTimeSeriesRequested(saveResultInMemory)
  }
  
  if (!is.null(startInSteadyState)) {
    restorationCall$startInSteadyState <- as.logical(problem$getStartInSteadyState())
    problem$setStartInSteadyState(startInSteadyState)
  }
  
  if (!is.null(updateModel)) {
    restorationCall$updateModel <- as.logical(task$isUpdateModel())
    task$setUpdateModel(updateModel)
  }
  
  if (!is.null(method)) {
    # We need to keep track of the previously set method
    restorationCall$method_old = task$getMethod()$getSubType()
    
    task$setMethodType(method$method)
    restorationCall$method <- list(method = method$method)
    method_cop = as(task$getMethod(), "_p_CTrajectoryMethod")
    
    method <- method[names(method) != "method"]
    
    if (!is_empty(method)) {
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
        dplyr::filter(!is.na(rowid), map_lgl(value, negate(is_null))) %>%
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
      if (nrow(method) != 0L) {
        restorationCall$method <- 
          append(restorationCall$method, method %>% dplyr::select(name, oldval) %>% tibble::deframe())
      }
      
      # Give complete feedback error.
      errmsg <- ""
      if (!is_empty(bad_names)) errmsg <- paste0(errmsg, "Method parameter(s) \"", paste0(bad_names, collapse = "\", \""), "\" invalid. Should be one of : \"", paste0(methodstruct$name, collapse = "\", \""), "\"\n")
      if (nrow(forbidden) != 0L) errmsg <- paste0(errmsg, "Method parameter(s) \"", paste0(forbidden$name, collapse = "\", \""), "\" have to be of type(s) ", paste0(forbidden$type, collapse = "\", \""), ".\n")
      if (nrow(failures) != 0L) errmsg <- paste0(errmsg, "Method parameter(s) \"", paste0(failures$name, collapse = "\", \""), "\" could not be set.\n")
      if (nchar(errmsg) != 0L) {
        try(stop(errmsg))
        errors <- TRUE
      }
    }
  }
  
  # method_old is only set if the purpose of calling the function was a restorationCall
  if (!is.null(method_old)) {
    task$setMethodType(method_old)
  }
  
  if (errors && .type != "restore") {
    do.call(tc_settings_worker, restorationCall)
    stop("Rolled back task due to errors during setup.")
  }
  
  restorationCall
}

tc_result_worker <- function(datamodel, saveResultInMemory) {
  task <- as(datamodel$getTask("Time-Course"), "_p_CTrajectoryTask")
  timeSeries <- task$getTimeSeries()
  recordedSteps <- timeSeries$getRecordedSteps()
  
  ret <- NULL
  
  if (recordedSteps) {
    # Timecritical step optimization
    timeSeries_ref <- timeSeries@ref
    R_swig_CTimeSeries_getConcentrationData <- getNativeSymbolInfo("R_swig_CTimeSeries_getConcentrationData", "COPASI")[["address"]]
    
    # assemble output dataframe
    # Iterates over all species/variables and all timepoints/steps
    # Inner loops creates numeric() wrapped in a named list
    # Outer loop binds all lists to a data frame
    ret <-
      seq_len_0(timeSeries$getNumVariables()) %>%
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
          set_names(CTimeSeries_getTitle(timeSeries, i_var))
      }) %>%
      dplyr::bind_cols() %>%
      rlang::set_attrs(class = prepend(class(.), "copasi_ts"))
  } else if (is.null(saveResultInMemory))
    warning("No results generated because saveResultInMemory is set to FALSE in the model. Explicitly set the argument to silence this warning.")
  
  ret
}