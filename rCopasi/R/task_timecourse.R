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
runTimeCourse <- function(duration, dt, intervals, suppressOutputBefore, outputEvents, saveResultInMemory, startInSteadyState, updateModel, method, datamodel = pkg_env$curr_dm) {
  assert_that(
    confirmDatamodel(datamodel),
    missing(duration)             || is_scalar_numeric(duration)                              && duration >= 0,
    missing(dt)                   || is_scalar_numeric(dt)                                    && dt >= 0,
    missing(intervals)            || rlang::is_scalar_integerish(intervals)                   && intervals > 0,
    missing(suppressOutputBefore) || is_scalar_numeric(suppressOutputBefore)                  && !is.na(suppressOutputBefore),
    missing(outputEvents)         || is_scalar_logical(outputEvents)                          && !is.na(outputEvents),
    missing(saveResultInMemory)   || is_scalar_logical(saveResultInMemory)                    && !is.na(saveResultInMemory),
    missing(startInSteadyState)   || is_scalar_logical(startInSteadyState)                    && !is.na(startInSteadyState),
    missing(updateModel)          || is_scalar_logical(updateModel)                           && !is.na(updateModel),
    missing(method)               || (is_scalar_character(method) || is_scalar_list(method))  && !rlang::is_na(method)
  )

  assert_that(!(!missing(dt) && !missing(intervals)), msg = "Only one of dt and intervals can be given")

  task <- as(datamodel$getTask("Time-Course"), "_p_CTrajectoryTask")
  assert_that(!is_null(task))
  # if (is.null(task)) {
  #   # create a new one
  #   task <- CSteadyStateTask()
  #   # add the new task to the task list
  #   dataModel$getTaskList()$addAndOwn(task)
  # }

  problem <- as(task$getProblem(), "_p_CTrajectoryProblem")

  restoreCall <-
    setupTimeCourse(
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

  setupTimeCourseMethod(task = task, method = method)

  # Not sure if this is needed
  # problem$setModel(datamodel$getModel())

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
    if (missing(saveResultInMemory)) warning("No results generated because saveResultInMemory is set to FALSE in the model")
  }

  do.call(setupTimeCourse, restoreCall)

  ret
}

setupTimeCourse <- function(problem, duration, dt, intervals, suppressOutputBefore, outputEvents, saveResultInMemory, startInSteadyState, updateModel) {
  restoreCall <- list(problem = problem)

  if (!missing(duration)) {
    restoreCall$duration <- problem$getDuration()
    problem$setDuration(duration)
  }

  if (!missing(dt)) {
    restoreCall$dt <- problem$getStepSize()
    problem$setStepSize(dt)
  }

  if (!missing(intervals)) {
    restoreCall$intervals <- problem$getStepNumber()
    problem$setStepNumber(intervals)
  }

  if (!missing(suppressOutputBefore)) {
    restoreCall$suppressOutputBefore <- problem$getOutputStartTime()
    problem$setOutputStartTime(suppressOutputBefore)
  }

  if (!missing(outputEvents)) {
    restoreCall$outputEvents <- problem$getOutputEvent()
    problem$setOutputEvent(outputEvents)
  }

  if (!missing(saveResultInMemory)) {
    restoreCall$saveResultInMemory <- problem$timeSeriesRequested()
    problem$setTimeSeriesRequested(saveResultInMemory)
  }

  if (!missing(startInSteadyState)) {
    restoreCall$startInSteadyState <- problem$getStartInSteadyState()
    problem$setStartInSteadyState(startInSteadyState)
  }

  if (!missing(updateModel)) stop("updateModel not yet implemented")

  restoreCall
}

setupTimeCourseMethod <- function(task, method) {
  validMethods <- names(.__E___CTaskEnum__Method)[task$getValidMethods()]

  if (!missing(method)) {
    if (is_scalar_character(method)) {
      task$setMethodType(match.arg(method, validMethods))
    } else {
      stop("Detailed method definition not yet implemented.")
    }
  }
}
