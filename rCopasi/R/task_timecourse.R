#' Run a timecourse
#'
#' \code{runTimecourse} runs a timecourse and returns the species data as a data frame
#'
#' @param duration timecourse duration
#' @param datamodel a model object
#' @return a data frame with a time column and species concentration columns
#' @export
runTimecourse <- function(duration, steps = 1000, initialtime = datamodel$getModel()$getInitialTime(), datamodel = pkg_env$curr_dm) {
  assert_that(confirmDatamodel(datamodel))

  trajectoryTask <- as(datamodel$getTask("Time-Course"), "_p_CTrajectoryTask")
  assert_that(!is_null(trajectoryTask))
  # if (is.null(task)) {
  #   # create a new one
  #   task <- CSteadyStateTask()
  #   # add the new task to the task list
  #   dataModel$getTaskList()$addAndOwn(task)
  # }

  trajectoryTask$setMethodType("deterministic")

  problem <- as(trajectoryTask$getProblem(), "_p_CTrajectoryProblem")

  # Not sure if this is needed
  # problem$setModel(datamodel$getModel())

  problem$setDuration(duration)
  problem$setStepNumber(steps)
  datamodel$getModel()$setInitialTime(initialtime)
  problem$setTimeSeriesRequested(TRUE)

  trajectoryTask$process(TRUE)
  
  timeSeries <- trajectoryTask$getTimeSeries()

  # assemble output dataframe
  ret <-
    1L:timeSeries$getNumVariables() %>%
    map(function(i_var) {
      1L:timeSeries$getRecordedSteps() %>%
        map_dbl(function(i_step) {
          timeSeries$getConcentrationData(i_step - 1L, i_var - 1L)
        }) %>%
        list() %>%
        set_names(timeSeries$getTitle(i_var - 1L))
    }) %>%
    dplyr::bind_cols()
  
  class(ret) <- prepend(class(ret), "copasi_ts")
  
  ret
}
