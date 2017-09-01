#' Create a new species
#'
#' \code{createSpecies} creates a new species.
#'
#' @param model a model object
#' @export
createSpecies <- function(name = NULL, compartment = NULL, type = c("fixed", "assignment", "reactions", "ode"), initial.concentration = 1, expression = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(
    is.string(name),
    is.null(compartment) || is.string(compartment),
    is.number(initial.concentration), initial.concentration >= 0,
    is.null(expression) || is.string(expression)
  )
  
  # .__E___CModelEntity__Status has other weird entries
  type <- rlang::arg_match(type)
  
  expression <- write_expr(expression, c_datamodel)
  
  c_model <- c_datamodel$getModel()
  
  if (is.null(compartment)) {
    cl_compartments <- c_model$getCompartments()
    # Even if multiple compartments exist, use the first as default
    c_compartment <- cl_compartments$get(0L)
    if (cl_compartments$size() > 1L) warning("No compartment given, using default: ", c_compartment$getObjectName())
  } else {
    c_compartment <- compartment_obj(compartment, c_datamodel)[[1]]
  }
  
  c_metab <- c_model$createMetabolite(name, c_compartment$getObjectName(), initial.concentration, stringr::str_to_upper(type))
  
  assert_that(inherits(c_metab, "_p_CMetab"), msg = "Species creation failed.")
  
  success <- grab_msg(c_metab$setExpression(expression)$isSuccess())
  
  if (!success) {
    c_model$removeMetabolite(c_metab)
    stop("Species creation failed when applying the expression.")
  }
  
  c_metab$getObjectDisplayName()
}

#' @export
removeSpecies <- function(key = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  cl_species <- species_obj(key, c_datamodel)
  
  c_model <- c_datamodel$getModel()
  
  cl_species %>%
    unique() %>%
    walk(~ c_model$removeMetabolite(.x))
  
  invisible()
}

#' @export
createGlobalQuantity <- function(name = NULL, type = c("fixed", "assignment", "ode"), initial.value = 1, expression = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(
    is.string(name),
    is.number(initial.value),
    is.null(expression) || is.string(expression)
  )
  
  # .__E___CModelEntity__Status has other weird entries
  type <- rlang::arg_match(type)
  
  expression <- write_expr(expression, c_datamodel)
  
  c_model <- c_datamodel$getModel()
  
  # type is missing
  c_quantity <- c_model$createModelValue(name, initial.value)
  
  assert_that(inherits(c_quantity, "_p_CModelValue"), msg = "Global quantity creation failed.")
  
  c_quantity$setStatus(stringr::str_to_upper(type))
  
  success <- grab_msg(c_quantity$setExpression(expression)$isSuccess())
  
  if (!success) {
    c_model$removeModelValue(c_quantity)
    stop("Global quantity creation failed when applying the expression.")
  }
  
  c_quantity$getObjectDisplayName()
}

#' @export
removeGlobalQuantity <- function(key = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  cl_quantity <- quantity_obj(key, c_datamodel)
  
  c_model <- c_datamodel$getModel()
  
  cl_quantity %>%
    unique() %>%
    walk(~ c_model$removeModelValue(.x))
  
  invisible()
}

#' @export
createCompartment <- function(name = NULL, type = c("fixed", "assignment", "ode"), initial.volume = 1, expression = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(
    is.string(name),
    is.number(initial.volume), initial.volume >= 0,
    is.null(expression) || is.string(expression)
  )
  
  # .__E___CModelEntity__Status has other weird entries
  type <- stringr::str_to_upper(rlang::arg_match(type))
  
  expression <- write_expr(expression, c_datamodel)
  
  c_model <- c_datamodel$getModel()
  
  # type is missing
  c_compartment <- c_model$createCompartment(name, initial.volume)
  
  assert_that(inherits(c_compartment, "_p_CCompartment"), msg = "Compartment creation failed.")
  
  c_compartment$setStatus(stringr::str_to_upper(type))
  
  success <- grab_msg(c_compartment$setExpression(expression)$isSuccess())
  
  if (!success) {
    c_model$removeCompartment(c_compartment)
    stop("Compartment creation failed when applying the expression.")
  }
  
  c_compartment$getObjectDisplayName()
}

#' @export
removeCompartment <- function(key = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  cl_compartments <- compartment_obj(key, c_datamodel)
  
  c_model <- c_datamodel$getModel()
  
  cl_compartments %>%
    unique() %>%
    walk(~ c_model$removeCompartment(.x))
  
  invisible()
}

createReaction <- function(name = NULL, scheme = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(
    is.string(name),
    is.string(scheme)
  )
  
  c_model <- c_datamodel$getModel()
  
  c_reaction <- c_model$createReaction(name)
  assert_that(inherits(c_reaction, "_p_CReaction"), msg = "Reaction creation failed")
  
  success <- grab_msg(c_reaction$setReactionScheme(scheme))
  assert_that(success, msg = "Reaction scheme invalid")
  
  c_reaction$getObjectDisplayName()
}

createReaction <- function(name, kinetic = NULL, ..., model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(
    is.string(name),
    is.string(kinetic)
  )
}