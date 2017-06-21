#' Get a model's name
#'
#' \code{getModelName} gives the name of the currently active model.
#'
#' @param datamodel a model object
#' @export
getModelName <- function(datamodel = pkg_env$curr_dm) {
  assert_that(confirmDatamodel(datamodel))
  
  datamodel$getModel()$getObjectName()
}

#' Set a model's name
#'
#' \code{setModelName} .
#'
#' @param name the new model name
#' @param datamodel a model object
#' @export
setModelName <- function(name, datamodel = pkg_env$curr_dm) {
  assert_that(confirmDatamodel(datamodel), is_scalar_character(name))
  
  assert_that(
    datamodel$getModel()$setObjectName(name),
    msg = "Setting model name failed."
  )
}

#' Get the unit of time
#'
#' \code{getTimeUnit} gets the unit used for time.
#'
#' @param datamodel a model object
#' @export
getTimeUnit <- function(datamodel = pkg_env$curr_dm) {
  assert_that(confirmDatamodel(datamodel))
  
  datamodel$getModel()$getTimeUnitName()
}

#' Set
#'
#' \code{setT} .
#'
#' @param x 
#' @param datamodel a model object
##' @export
setTimeUnit <- function(unit, datamodel = pkg_env$curr_dm) {
  assert_that(confirmDatamodel(datamodel), is_scalar_character(unit))
  
  cunit <- CUnit(unit)
  
  assert_that(
    cunit$isUnitType("time"),
    msg = paste0(unit, "is not a valid time unit.")
  )
  
  delete(cunit)
  
  assert_that(
    datamodel$getModel()$setTimeUnitFromString(unit),
    msg = "Setting time unit failed."
  )
}

#' #' Get 
#' #'
#' #' \code{getT} .
#' #'
#' #' @param datamodel a model object
#' ##' @export
#' getT <- function(datamodel = pkg_env$curr_dm) {
#'   assert_that(confirmDatamodel(datamodel))
#' }
#' 
#' #' Set
#' #'
#' #' \code{setT} .
#' #'
#' #' @param x 
#' #' @param datamodel a model object
#' ##' @export
#' setT <- function(x, datamodel = pkg_env$curr_dm) {
#'   assert_that(confirmDatamodel(datamodel), )
#'   
#'   assert_that(
#'     do(x),
#'     msg = "Setting x failed."
#'   )
#' }
#' 
