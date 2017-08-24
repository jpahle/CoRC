#' Create a new species
#'
#' \code{createSpecies} creates a new species.
#'
#' @param datamodel a model object
#' @export
createSpecies <- function(name = NULL, compartment = NULL, type = c("fixed", "assignment", "reactions", "ode"), initial.concentration = 1, expression = NULL, datamodel = getCurrentModel()) {
  assert_datamodel(datamodel)
  assert_that(
    is.string(name),
    is.null(compartment) || is.string(compartment),
    is.number(initial.concentration), initial.concentration >= 0,
    is.null(expression) || is.string(expression)
  )
  
  c_model <- datamodel$getModel()
  
  if (is_null(c_compartment)) {
    c_compartments <- c_model$getCompartments()
    # Even if multiple compartments exist, use the first as default
    c_compartment <- c_compartments$get(0L)
    if (c_compartments$size() > 1L) warning("No compartment given, using default: ", c_compartment$getObjectName())
  } else {
    c_compartment <- compartment_obj(compartment, datamodel = datamodel)[[1]]
  }
  
  # .__E___CModelEntity__Status has other weird entries
  type <- stringr::str_to_upper(rlang::arg_match(type))
  
  c_metab <- c_model$createMetabolite(name, c_compartment$getObjectName(), initial.concentration, type)
  
  assert_that(inherits(c_metab, "_p_CMetab"), msg = "Species creation failed.")
  
  c_metab$getObjectDisplayName()
}

#' @export
removeSpecies <- function(key = NULL, datamodel = getCurrentModel()) {
  assert_datamodel(datamodel)
  
  c_species <- species_obj(key, datamodel = datamodel)
  
  c_model <- datamodel$getModel()
  
  c_species %>%
    unique() %>%
    walk(~ c_model$removeMetabolite(.x))
  
  invisible()
}

#' @export
createGlobalQuantity <- function(name = NULL, type = c("fixed", "assignment", "ode"), initial.value = 1, datamodel = getCurrentModel()) {
  assert_datamodel(datamodel)
  assert_that(
    is.string(name),
    is.number(initial.value)
  )
  
  c_model <- datamodel$getModel()
  
  # .__E___CModelEntity__Status has other weird entries
  type <- stringr::str_to_upper(rlang::arg_match(type))
  
  # type is missing
  c_quantity <- c_model$createModelValue(name, initial.value)
  
  assert_that(inherits(c_quantity, "_p_CModelValue"), msg = "Global quantity creation failed.")
  
  c_quantity$getObjectDisplayName()
}

#' @export
removeGlobalQuantity <- function(key = NULL, datamodel = getCurrentModel()) {
  assert_datamodel(datamodel)
  
  c_quantity <- quantity_obj(key, datamodel = datamodel)
  
  c_model <- datamodel$getModel()
  
  c_quantity %>%
    unique() %>%
    walk(~ c_model$removeModelValue(.x))
  
  invisible()
}

#' @export
createCompartment <- function(name = NULL, type = c("fixed", "assignment", "ode"), initial.volume = 1, datamodel = getCurrentModel()) {
  assert_datamodel(datamodel)
  assert_that(
    is.string(name),
    is.number(initial.volume), initial.volume >= 0
  )
  
  c_model <- datamodel$getModel()
  
  # .__E___CModelEntity__Status has other weird entries
  type <- stringr::str_to_upper(rlang::arg_match(type))
  
  # type is missing
  c_compartment <- c_model$createCompartment(name, initial.volume)
  
  assert_that(inherits(c_compartment, "_p_CCompartment"), msg = "Compartment creation failed.")
  
  c_compartment$getObjectDisplayName()
}

#' @export
removeCompartment <- function(key = NULL, datamodel = getCurrentModel()) {
  assert_datamodel(datamodel)
  
  c_compartments <- compartment_obj(key, datamodel = datamodel)
  
  c_model <- datamodel$getModel()
  
  c_compartments %>%
    unique() %>%
    walk(~ c_model$removeCompartment(.x))
  
  invisible()
}

createReaction <- function(name = NULL, scheme = NULL, datamodel = getCurrentModel()) {
  assert_datamodel(datamodel)
  assert_that(
    is.string(name),
    is.string(scheme)
  )
  
  c_model <- datamodel$getModel()
  
  c_reaction <- c_model$createReaction(name)
  assert_that(inherits(c_reaction, "_p_CReaction"), msg = "Reaction creation failed")
  
  success <- grab_msg(c_reaction$setReactionScheme(scheme))
  assert_that(success, msg = "Reaction scheme invalid")
  
  c_reaction$getObjectDisplayName()
}

createReaction <- function(name, kinetic = NULL, ..., datamodel = getCurrentModel()) {
  assert_datamodel(datamodel)
  assert_that(
    is.string(name),
    is.string(kinetic)
  )
}