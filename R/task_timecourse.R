#' Run a time course
#'
#' \code{runTimeCourse} runs a time course and returns the species data as a data frame
#'
#' @param duration numeric time course duration
#' @param dt numeric
#' @param intervals integer
#' @param suppressOutputBefore boolean
#' @param outputEvents boolean
#' @param saveResultInMemory boolean
#' @param startInSteadyState boolean
#' @param updateModel not yet implemented
#' @param method character or list
#' @param datamodel a model object
#' @return a data frame with a time column and species concentration columns
#' @export
runTimeCourse <- function(duration = NULL, dt = NULL, intervals = NULL, suppressOutputBefore = NULL, outputEvents = NULL, saveResultInMemory = NULL, startInSteadyState = NULL, updateModel = NULL, method = NULL, datamodel = pkg_env$curr_dm) {
  assert_that(!(!is.null(dt) && !is.null(intervals)), msg = "Only one of dt and intervals can be given")
  
  restorationCall <- do.call(set_tcs_worker, tail(as.list(sys.call()), -1))
  
  task <- as(datamodel$getTask("Time-Course"), "_p_CTrajectoryTask")
  
  problem <- as(task$getProblem(), "_p_CTrajectoryProblem")

  # datamodel$getModel()$setInitialTime(initialtime)

  task$process(TRUE)

  if (problem$timeSeriesRequested()) {
    timeSeries <- task$getTimeSeries()

    recordedSteps <- timeSeries$getRecordedSteps()
    # assemble output dataframe
    ret <-
      0L:(timeSeries$getNumVariables() - 1L) %>%
      map(function(i_var) {
        0L:(recordedSteps - 1L) %>%
          map_dbl(function(i_step) {
            # timeSeries$getConcentrationData(i_step, i_var)
            CTimeSeries_getConcentrationData(timeSeries, i_step, i_var)
          }) %>%
          list() %>%
          # set_names(timeSeries$getTitle(i_var))
          set_names(CTimeSeries_getTitle(timeSeries, i_var))
      }) %>%
      dplyr::bind_cols()
    
    class(ret) <- prepend(class(ret), "copasi_ts")
  } else {
    ret <- NULL
    if (is.null(saveResultInMemory)) warning("No results generated because saveResultInMemory is set to FALSE in the model. Explicitly set the argument to silence this warning.")
  }

  do.call(set_tcs_worker, restorationCall)

  ret
}

#' Set time course settings
#'
#' \code{setTimeCourseSettings} sets time course settings including method options
#'
#' @param duration numeric time course duration
#' @param dt numeric
#' @param intervals integer
#' @param suppressOutputBefore boolean
#' @param outputEvents boolean
#' @param saveResultInMemory boolean
#' @param startInSteadyState boolean
#' @param updateModel not yet implemented
#' @param method character or list
#' @param datamodel a model object
#' @export
setTimeCourseSettings <- function(duration = NULL, dt = NULL, intervals = NULL, suppressOutputBefore = NULL, outputEvents = NULL, saveResultInMemory = NULL, startInSteadyState = NULL, updateModel = NULL, method = NULL, datamodel = pkg_env$curr_dm) {
  assert_that(!(!is.null(dt) && !is.null(intervals)), msg = "Only one of dt and intervals can be given")
  
  do.call(set_tcs_worker, tail(as.list(sys.call()), -1))
  
  invisible()
}

set_tcs_worker <- function(duration = NULL, dt = NULL, intervals = NULL, suppressOutputBefore = NULL, outputEvents = NULL, saveResultInMemory = NULL, startInSteadyState = NULL, updateModel = NULL, method = NULL, method_old = NULL, datamodel = pkg_env$curr_dm) {
  assert_that(confirmDatamodel(datamodel))
  
  task <- as(datamodel$getTask("Time-Course"), "_p_CTrajectoryTask")
  
  problem <- as(task$getProblem(), "_p_CTrajectoryProblem")
  
  assert_that(
    is.null(duration)             || is_scalar_numeric(duration)                  && duration >= 0,
    is.null(dt)                   || is_scalar_numeric(dt)                        && dt >= 0,
    is.null(intervals)            || rlang::is_scalar_integerish(intervals)       && intervals > 0,
    is.null(suppressOutputBefore) || is_scalar_numeric(suppressOutputBefore)      && !is.na(suppressOutputBefore),
    is.null(outputEvents)         || is_scalar_logical(outputEvents)              && !is.na(outputEvents),
    is.null(saveResultInMemory)   || is_scalar_logical(saveResultInMemory)        && !is.na(saveResultInMemory),
    is.null(startInSteadyState)   || is_scalar_logical(startInSteadyState)        && !is.na(startInSteadyState),
    is.null(updateModel)          || is_scalar_logical(updateModel)               && !is.na(updateModel),
    is.null(method)               || is_scalar_character(method)                  && !is.na(method) || is_list(method) && is_scalar_character(method$method) && !is.na(method$method)
  )
  
  if (!is.null(method)) {
    if (is_scalar_character(method)) method <- list(method = method)
    with(method, rlang::arg_match(method, names(.__E___CTaskEnum__Method)[task$getValidMethods() + 1L]))
  }
  
  restorationCall <- list(datamodel = datamodel)
  
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
    restorationCall$outputEvents <- problem$getOutputEvent()
    problem$setOutputEvent(outputEvents)
  }
  
  if (!is.null(saveResultInMemory)) {
    restorationCall$saveResultInMemory <- problem$timeSeriesRequested()
    problem$setTimeSeriesRequested(saveResultInMemory)
  }
  
  if (!is.null(startInSteadyState)) {
    restorationCall$startInSteadyState <- problem$getStartInSteadyState()
    problem$setStartInSteadyState(startInSteadyState)
  }
  
  if (!is.null(updateModel)) warning("updateModel not yet implemented")
  
  if (!is.null(method)) {
    restorationCall$method_old = task$getMethod()$getSubType()
    
    task$setMethodType(method$method)
    restorationCall$method <- list(method = method$method)
    method_cop = as(task$getMethod(), "_p_CTrajectoryMethod")
    
    method <- method[names(method) != "method"]
    
    if (!is_empty(method)) {
      validstruct <- methodstructure(method_cop)
      
      positions <- pmatch(names(method), names(validstruct))
      bad_names <- names(method)[is.na(positions)]
      
      validmethods <- !(is.na(positions) | map_lgl(method, is_null))
      method <- method[validmethods]
      positions <- positions[validmethods]
      
      status <-
        seq_along(method) %>%
        map(~{
          curr_pos <- positions[.x]
          curr_param <- method_cop$getParameter(curr_pos - 1L)
          curr_type <- validstruct[[curr_pos]]
          list(
            param = names(validstruct)[curr_pos],
            oldval = cparameter_get_functions[[curr_type]](curr_param, method[[.x]]),
            success = cparameter_set_functions[[curr_type]](curr_param, method[[.x]])
          )
        })
      
      failures <- status[!map_lgl(status, "success")]
      
      status <- transpose(status[map_lgl(status, "success")])
      
      if (!is_empty(status)) {
        restorationCall$method <-
          restorationCall$method %>%
          append(set_names(status$oldval, flatten_chr(status$param)))
      }
      
      if (!is_empty(bad_names) || !is_empty(failures)) {
        do.call(set_tcs_worker, restorationCall)
        errmsg <- ""
        if (!is_empty(bad_names)) errmsg <- paste0(errmsg, "Method parameter(s) \"", paste0(bad_names, collapse = "\", \""), "\" invalid. Should be one of : \"", paste0(names(validstruct), collapse = "\", \""), "\"\n")
        if (!is_empty(failures)) errmsg <- paste0(errmsg, "Method parameter(s) \"", paste0(map_chr(failures, "param"), collapse = "\", \""), "\" could not be set.\n")
        stop(errmsg)
      }
    }
  }
  
  if (!is.null(method_old)) {
    task$setMethodType(method_old)
  }
  
  restorationCall
}

cparameter_set_functions <-
  list(
    DOUBLE = CCopasiParameter_setDblValue,
    UDOUBLE = CCopasiParameter_setUDblValue,
    INT = CCopasiParameter_setIntValue,
    UINT = CCopasiParameter_setUIntValue,
    BOOL = CCopasiParameter_setBoolValue,
    STRING = CCopasiParameter_setStringValue,
    GROUP = NULL, # setGroupValue
    CN = NULL, # setCNValue
    KEY = NULL, # setKeyValue
    FILE = NULL, # setFileValue
    EXPRESSION = NULL,
    INVALID = NULL
  )

cparameter_get_functions <-
  list(
    DOUBLE = CCopasiParameter_getDblValue,
    UDOUBLE = CCopasiParameter_getUDblValue,
    INT = CCopasiParameter_getIntValue,
    UINT = CCopasiParameter_getUIntValue,
    BOOL = CCopasiParameter_getBoolValue,
    STRING = CCopasiParameter_getStringValue,
    GROUP = NULL, # setGroupValue
    CN = NULL, # setCNValue
    KEY = NULL, # setKeyValue
    FILE = NULL, # setFileValue
    EXPRESSION = NULL,
    INVALID = NULL
  )

methodstructure <- function(method) {
  names <- seq_along_cv(method) %>% map_chr(~ method$getParameter(.x)$getObjectName()) %>% make.names(unique = TRUE)
  
  types <- seq_along_cv(method) %>% map_chr(~ method$getParameter(.x)$getType())
  if (contains(types, NULL)) warning("Unknown type found with parameters: ", paste(names[map_lgl(types, is.null)], collapse = "; "))

  types %>% set_names(names)
}
