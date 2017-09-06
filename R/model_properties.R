#' Get the model's name
#'
#' \code{getModelName} gives the name of the model.
#'
#' @param model a model object
#' @return name
#' @export
getModelName <- function(model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  c_datamodel$getModel()$getObjectName()
}

#' Set a model's name
#'
#' \code{setModelName} sets the name of the model.
#'
#' @param name the new model name
#' @param model a model object
#' @export
setModelName <- function(name, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(is.string(name))
  
  assert_that(
    c_datamodel$getModel()$setObjectName(name),
    msg = "Setting model name failed."
  )
  
  invisible()
}

#' Get the unit of time
#'
#' \code{getTimeUnit} gets the unit used for time.
#'
#' @param model a model object
#' @return unit of time
#' @export
getTimeUnit <- function(model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  c_datamodel$getModel()$getTimeUnitName()
}

#' Set the unit of time
#'
#' \code{setTimeUnit} sets the unit used for time.
#'
#' @param unit string
#' @param model a model object
#' @export
setTimeUnit <- function(unit, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(is.string(unit))
  
  c_unit <- grab_msg(CUnit(unit))
  c_unit$buildExpression()
  unit <- c_unit$getExpression()
  accepted <- c_unit$isUnitType("time")
  
  assert_that(accepted, msg = paste0(unit, " is not a valid time unit."))
  
  assert_that(
    grab_msg(c_datamodel$getModel()$setTimeUnitFromString(unit)),
    msg = "Setting time unit failed."
  )
  
  invisible()
}

#' Get the unit of volume
#'
#' \code{getTimeUnit} gets the unit used for volume.
#'
#' @param model a model object
#' @return unit of volume
#' @export
getVolumeUnit <- function(model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  c_datamodel$getModel()$getVolumeUnitName()
}

#' Set the unit of volume
#'
#' \code{setVolumeUnit} sets the unit used for volume.
#'
#' @param unit string
#' @param model a model object
#' @export
setVolumeUnit <- function(unit, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(is.string(unit))
  
  c_unit <- grab_msg(CUnit(unit))
  c_unit$buildExpression()
  unit <- c_unit$getExpression()
  accepted <- c_unit$isUnitType("volume")
  
  assert_that(accepted, msg = paste0(unit, " is not a valid volume unit."))
  
  assert_that(
    grab_msg(c_datamodel$getModel()$setVolumeUnitFromString(unit)),
    msg = "Setting volume unit failed."
  )
  
  invisible()
}

#' Get the unit of quantity
#'
#' \code{getQuantityUnit} gets the unit used for quantitiy.
#'
#' @param model a model object
#' @return unit of quantity
#' @export
getQuantityUnit <- function(model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  c_datamodel$getModel()$getQuantityUnitName()
}

#' Set the unit of quantity
#'
#' \code{setQuantityUnit} sets the unit used for quantity.
#'
#' @param unit string
#' @param model a model object
#' @export
setQuantityUnit <- function(unit, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(is.string(unit))
  
  c_unit <- grab_msg(CUnit(unit))
  c_unit$buildExpression()
  unit <- c_unit$getExpression()
  accepted <- c_unit$isUnitType("quantity")
  
  assert_that(accepted, msg = paste0(unit, " is not a valid quantity unit."))
  
  assert_that(
    grab_msg(c_datamodel$getModel()$setQuantityUnitFromString(unit)),
    msg = "Setting quantity unit failed."
  )
  
  invisible()
}


#' Get the model's inital time
#'
#' \code{getInitialTime} gets the initial time of the model.
#'
#' @param model a model object
#' @export
getInitialTime <- function(model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  c_datamodel$getModel()$getInitialTime()
}

#' Set the model's initial time
#'
#' \code{setInitialTime} sets the initial time of the model.
#'
#' @param time numeric
#' @param model a model object
#' @export
setInitialTime <- function(time, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(is.number(time), time >= 0)
  
  c_model <- c_datamodel$getModel()
  
  assert_that(
    !c_model$isAutonomous(),
    msg = "Can't set initial time for autonomous models."
  )
  
  c_model$setInitialTime(time)
  
  invisible()
}

#' @export
getStoichiometryMatrix <- function(model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  c_model <- c_datamodel$getModel()
  
  # c_model$buildStoi()
  
  # mb should be integer
  get_annotated_matrix(
    c_model$getStoiAnnotation()
  )
}

# #' @export
# getReducedStoichiometryMatrix <- function(model = getCurrentModel()) {
#   c_datamodel <- assert_datamodel(model)
#   
#   c_model <- c_datamodel$getModel()
#   
#   c_model$buildRedStoi()
#   
#   get_annotated_matrix(
#     c_model$getRedStoiAnnotation()
#   )
# }

# #' @export
# getLinkMatrix <- function(model = getCurrentModel()) {
#   c_datamodel <- assert_datamodel(model)
#   
#   c_model <- c_datamodel$getModel()
#   
#   get_annotated_matrix(
#     c_model$getLinkAnnotation()
#   )
# }
