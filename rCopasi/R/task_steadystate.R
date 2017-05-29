#' Run to stready state
#'
#' \code{runSteadyState} calculates the steady state and returns the species concentrations as a data frame
#'
#' @param datamodel a model object
#' @return a metabolite data frame
#' @export
runSteadyState <- function(datamodel = pkg_env$curr_dm) {
  assert_that(confirmDatamodel(datamodel))

  ssTask <- as(datamodel$getTask("Steady-State"), "_p_CSteadyStateTask")
  assert_that(!is_null(ssTask))
  # if (is.null(task)) {
  #   # create a new one
  #   task <- CSteadyStateTask()
  #   # add the new task to the task list
  #   dataModel$getTaskList()$addAndOwn(task)
  # }

  problem <- as(ssTask$getProblem(), "_p_CSteadyStateProblem")

  # Not sure if this is needed
  # problem$setModel(datamodel$getModel())

  ssTask$process(TRUE)

  metabs <- datamodel$getModel()$getMetabolites()

  # assemble output dataframe
  seq_along_cv(metabs) %>%
    map_df(~ {
      metab <- get_from_cv(metabs, .x)
      list(
        key = metab$getKey(),
        name = metab$getObjectName(),
        concentration = metab$getConcentration()
      )
    })
}
