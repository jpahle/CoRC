#' Get a model's name
#'
#' \code{getModelName} gives the name of the model.
#'
#' @param datamodel a model object
#' @return name
#' @export
getModelName <- function(datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  
  datamodel$getModel()$getObjectName()
}

#' Set a model's name
#'
#' \code{setModelName} sets the name of the model.
#'
#' @param name the new model name
#' @param datamodel a model object
#' @export
setModelName <- function(name, datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  assert_that(is.string(name))
  
  assert_that(
    datamodel$getModel()$setObjectName(name),
    msg = "Setting model name failed."
  )
  
  invisible()
}

#' Get the unit of time
#'
#' \code{getTimeUnit} gets the unit used for time.
#'
#' @param datamodel a model object
#' @return unit of time
#' @export
getTimeUnit <- function(datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  
  datamodel$getModel()$getTimeUnitName()
}

#' Set the unit of time
#'
#' \code{setTimeUnit} sets the unit used for time.
#'
#' @param unit string
#' @param datamodel a model object
#' @export
setTimeUnit <- function(unit, datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  assert_that(is.string(unit))
  
  cunit <- grab_msg(CUnit(unit))
  cunit$buildExpression()
  unit <- cunit$getExpression()
  accepted <- cunit$isUnitType("time")
  
  assert_that(accepted, msg = paste0(unit, " is not a valid time unit."))
  
  assert_that(
    grab_msg(datamodel$getModel()$setTimeUnitFromString(unit)),
    msg = "Setting time unit failed."
  )
  
  invisible()
}

#' Get the unit of volume
#'
#' \code{getTimeUnit} gets the unit used for volume.
#'
#' @param datamodel a model object
#' @return unit of volume
#' @export
getVolumeUnit <- function(datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  
  datamodel$getModel()$getVolumeUnitName()
}

#' Set the unit of volume
#'
#' \code{setVolumeUnit} sets the unit used for volume.
#'
#' @param unit string
#' @param datamodel a model object
#' @export
setVolumeUnit <- function(unit, datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  assert_that(is.string(unit))
  
  cunit <- grab_msg(CUnit(unit))
  cunit$buildExpression()
  unit <- cunit$getExpression()
  accepted <- cunit$isUnitType("volume")
  
  assert_that(accepted, msg = paste0(unit, " is not a valid volume unit."))
  
  assert_that(
    grab_msg(datamodel$getModel()$setVolumeUnitFromString(unit)),
    msg = "Setting volume unit failed."
  )
  
  invisible()
}

#' Get the unit of quantity
#'
#' \code{getQuantityUnit} gets the unit used for quantitiy.
#'
#' @param datamodel a model object
#' @return unit of quantity
#' @export
getQuantityUnit <- function(datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  
  datamodel$getModel()$getQuantityUnitName()
}

#' Set the unit of quantity
#'
#' \code{setQuantityUnit} sets the unit used for quantity.
#'
#' @param unit string
#' @param datamodel a model object
#' @export
setQuantityUnit <- function(unit, datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  assert_that(is.string(unit))
  
  cunit <- grab_msg(CUnit(unit))
  cunit$buildExpression()
  unit <- cunit$getExpression()
  accepted <- cunit$isUnitType("quantity")
  
  assert_that(accepted, msg = paste0(unit, " is not a valid quantity unit."))
  
  assert_that(
    grab_msg(datamodel$getModel()$setQuantityUnitFromString(unit)),
    msg = "Setting quantity unit failed."
  )
  
  invisible()
}


#' Get the model's inital time
#'
#' \code{getInitialTime} gets the initial time of the model.
#'
#' @param datamodel a model object
#' @export
getInitialTime <- function(datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  
  datamodel$getModel()$getInitialTime()
}

#' Set the model's initial time
#'
#' \code{setInitialTime} sets the initial time of the model.
#'
#' @param time numeric
#' @param datamodel a model object
#' @export
setInitialTime <- function(time, datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  assert_that(is.number(time), time >= 0)
  
  model <- datamodel$getModel()
  
  assert_that(
    !model$isAutonomous(),
    msg = "Can't set initial time for autonomous models."
  )
  
  model$setInitialTime(time)
  
  invisible()
}
