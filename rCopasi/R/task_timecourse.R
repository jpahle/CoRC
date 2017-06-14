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
#' @param method character
#' @param datamodel a model object
#' @return a data frame with a time column and species concentration columns
#' @export
runTimeCourse <- function(duration = NULL, dt = NULL, intervals = NULL, suppressOutputBefore = NULL, outputEvents = NULL, saveResultInMemory = NULL, startInSteadyState = NULL, updateModel = NULL, method = NULL, datamodel = pkg_env$curr_dm) {
  assert_that(confirmDatamodel(datamodel))
  
  confirmTimeCourseSettings()
  confirmTimeCourseMethod()

  task <- as(datamodel$getTask("Time-Course"), "_p_CTrajectoryTask")

  problem <- as(task$getProblem(), "_p_CTrajectoryProblem")

  restoreCall <-
    setTimeCourseSettings(
      problem = problem,
      duration = duration,
      dt = dt,
      intervals = intervals,
      suppressOutputBefore = suppressOutputBefore,
      outputEvents = outputEvents,
      saveResultInMemory = saveResultInMemory,
      startInSteadyState = startInSteadyState,
      updateModel = updateModel
    )
  
  setTimeCourseMethod(
    task = task,
    method = method
  )

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

  do.call(setTimeCourseSettings, restoreCall)

  ret
}

setupTimeCourse <- function(duration = NULL, dt = NULL, intervals = NULL, suppressOutputBefore = NULL, outputEvents = NULL, saveResultInMemory = NULL, startInSteadyState = NULL, updateModel = NULL, method = NULL, datamodel = pkg_env$curr_dm) {
  assert_that(confirmDatamodel(datamodel))
  
  confirmTimeCourseSettings()
  confirmTimeCourseMethod()
  
  task <- as(datamodel$getTask("Time-Course"), "_p_CTrajectoryTask")
  
  problem <- as(task$getProblem(), "_p_CTrajectoryProblem")
  
  setTimeCourseSettings(
    problem = problem,
    duration = duration,
    dt = dt,
    intervals = intervals,
    suppressOutputBefore = suppressOutputBefore,
    outputEvents = outputEvents,
    saveResultInMemory = saveResultInMemory,
    startInSteadyState = startInSteadyState,
    updateModel = updateModel
  )
  
  setTimeCourseMethod(
    task = task,
    method = method
  )
  
  invisible()
}

confirmTimeCourseSettings <- function() {
  evalq(
    envir = 1,
    {
      assert_that(
        is.null(duration)             || is_scalar_numeric(duration)                              && duration >= 0,
        is.null(dt)                   || is_scalar_numeric(dt)                                    && dt >= 0,
        is.null(intervals)            || rlang::is_scalar_integerish(intervals)                   && intervals > 0,
        is.null(suppressOutputBefore) || is_scalar_numeric(suppressOutputBefore)                  && !is.na(suppressOutputBefore),
        is.null(outputEvents)         || is_scalar_logical(outputEvents)                          && !is.na(outputEvents),
        is.null(saveResultInMemory)   || is_scalar_logical(saveResultInMemory)                    && !is.na(saveResultInMemory),
        is.null(startInSteadyState)   || is_scalar_logical(startInSteadyState)                    && !is.na(startInSteadyState),
        is.null(updateModel)          || is_scalar_logical(updateModel)                           && !is.na(updateModel)
      )
      assert_that(!(!is.null(dt) && !is.null(intervals)), msg = "Only one of dt and intervals can be given")
    }
  )
}

setTimeCourseSettings <- function(problem, duration = NULL, dt = NULL, intervals = NULL, suppressOutputBefore = NULL, outputEvents = NULL, saveResultInMemory = NULL, startInSteadyState = NULL, updateModel = NULL) {
  restoreCall <- list(problem = problem)

  if (!is.null(duration)) {
    restoreCall$duration <- problem$getDuration()
    problem$setDuration(duration)
  }

  if (!is.null(dt)) {
    restoreCall$dt <- problem$getStepSize()
    problem$setStepSize(dt)
  }

  if (!is.null(intervals)) {
    restoreCall$intervals <- problem$getStepNumber()
    problem$setStepNumber(intervals)
  }

  if (!is.null(suppressOutputBefore)) {
    restoreCall$suppressOutputBefore <- problem$getOutputStartTime()
    problem$setOutputStartTime(suppressOutputBefore)
  }

  if (!is.null(outputEvents)) {
    restoreCall$outputEvents <- problem$getOutputEvent()
    problem$setOutputEvent(outputEvents)
  }

  if (!is.null(saveResultInMemory)) {
    restoreCall$saveResultInMemory <- problem$timeSeriesRequested()
    problem$setTimeSeriesRequested(saveResultInMemory)
  }

  if (!is.null(startInSteadyState)) {
    restoreCall$startInSteadyState <- problem$getStartInSteadyState()
    problem$setStartInSteadyState(startInSteadyState)
  }

  if (!is.null(updateModel)) stop("updateModel not yet implemented")

  restoreCall
}

confirmTimeCourseMethod <- function() {
  evalq(
    envir = 1,
    {
      assert_that(is.null(method) || (is_scalar_character(method) || is_scalar_list(method)) && !rlang::is_na(method))
    }
  )
}

setTimeCourseMethod <- function(task, method = NULL) {
  validMethods <- names(.__E___CTaskEnum__Method)[task$getValidMethods()]

  if (!is.null(method)) {
    if (is_scalar_character(method)) {
      task$setMethodType(rlang::arg_match(method, validMethods))
    } else {
      stop("Detailed method definition not yet implemented.")
    }
  }
}
