#' Create a new species
#'
#' \code{newSpecies} creates a new species.
#'
#' @param model a model object
#' @export
newSpecies <- function(name, compartment = NULL, type = c("fixed", "assignment", "reactions", "ode"), initial.concentration = 1, expression = NULL, model = getCurrentModel()) {
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
    cl_comps <- c_model$getCompartments()
    # Even if multiple compartments exist, use the first as default
    c_comp <- cl_comps$get(0L)
    
    if (cl_comps$size() > 1L)
      warning("No compartment given, using default: ", c_comp$getObjectName())
  } else {
    c_comp <- compartment_obj(compartment, c_datamodel)[[1]]
  }
  
  c_metab <- c_model$createMetabolite(name, c_comp$getObjectName(), initial.concentration, stringr::str_to_upper(type))
  
  assert_that(inherits(c_metab, "_p_CMetab"), msg = "Species creation failed.")
  
  success <- grab_msg(c_metab$setExpression(expression)$isSuccess())
  
  if (!success) {
    c_model$removeMetabolite(c_metab)
    stop("Species creation failed when applying the expression.")
  }
  
  c_metab$getObjectDisplayName()
}

#' @export
deleteSpecies <- function(key, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  cl_metabs <- species_obj(key, c_datamodel)
  
  c_model <- c_datamodel$getModel()
  
  cl_metabs %>%
    unique() %>%
    walk(~ c_model$removeMetabolite(.x))
  
  invisible()
}

#' @export
newGlobalQuantity <- function(name, type = c("fixed", "assignment", "ode"), initial.value = 1, expression = NULL, model = getCurrentModel()) {
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
  c_quant <- c_model$createModelValue(name, initial.value)
  
  assert_that(inherits(c_quant, "_p_CModelValue"), msg = "Global quantity creation failed.")
  
  c_quant$setStatus(stringr::str_to_upper(type))
  
  success <- grab_msg(c_quant$setExpression(expression)$isSuccess())
  
  if (!success) {
    c_model$removeModelValue(c_quant)
    stop("Global quantity creation failed when applying the expression.")
  }
  
  c_quant$getObjectDisplayName()
}

#' @export
deleteGlobalQuantity <- function(key, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  cl_quants <- quantity_obj(key, c_datamodel)
  
  c_model <- c_datamodel$getModel()
  
  cl_quants %>%
    unique() %>%
    walk(~ c_model$removeModelValue(.x))
  
  invisible()
}

#' @export
newCompartment <- function(name, type = c("fixed", "assignment", "ode"), initial.volume = 1, expression = NULL, model = getCurrentModel()) {
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
  c_comp <- c_model$createCompartment(name, initial.volume)
  
  assert_that(inherits(c_comp, "_p_CCompartment"), msg = "Compartment creation failed.")
  
  c_comp$setStatus(stringr::str_to_upper(type))
  
  success <- grab_msg(c_comp$setExpression(expression)$isSuccess())
  
  if (!success) {
    c_model$removeCompartment(c_comp)
    stop("Compartment creation failed when applying the expression.")
  }
  
  c_comp$getObjectDisplayName()
}

#' @export
deleteCompartment <- function(key, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  cl_comps <- compartment_obj(key, c_datamodel)
  
  c_model <- c_datamodel$getModel()
  
  cl_comps %>%
    unique() %>%
    walk(~ c_model$removeCompartment(.x))
  
  invisible()
}

#' @export
newReaction <- function(scheme, name = scheme, fun = NULL, mappings = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(
    is.string(name),
    is.string(scheme)
  )
  
  c_model <- c_datamodel$getModel()
  
  c_react <- c_model$createReaction(name)
  assert_that(inherits(c_react, "_p_CReaction"), msg = "Reaction creation failed")
  
  success <- grab_msg(c_react$setReactionScheme(scheme))
  assert_that(success, msg = "Reaction scheme invalid")
  
  dn <- c_react$getObjectDisplayName()
  
  # apply fun and mapping via successive functions
  tryCatch(
    {
      if (!is.null(fun))
        setReactionFunction(
          key = dn,
          fun = fun,
          model = c_datamodel
        )
      if (!is.null(mappings))
        setReactionMappings(
          key = dn,
          mappings = mappings,
          model = c_datamodel
        )
    },
    error = function(e) {
      c_model$removeReaction(c_react)
      base::stop(e)
    }
  )
  
  dn
}

#' @export
deleteReaction <- function(key, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  cl_reacts <- reaction_obj(key, c_datamodel)
  
  c_model <- c_datamodel$getModel()
  
  cl_reacts %>%
    unique() %>%
    walk(~ c_model$removeReaction(.x))
  
  invisible()
}

getValidReactionFunctions <- function(key, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(
    is.scalar(key)
  )
  
  c_react <- reaction_obj(key, c_datamodel)[[1]]
  
  c_model <- c_datamodel$getModel()
  c_reacti <- CReactionInterface(c_model)
  c_reacti$initFromReaction(c_react)
  
  c_flist <- new(
    "_p_std__vectorT_std__string_std__allocatorT_std__string_t_t",
    ref = c_reacti$getListOfPossibleFunctions()
  )
  
  get_sv(c_flist)
}

setReactionFunction <- function(key, fun, mappings = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(
    is.scalar(key),
    is.string(fun)
  )
  
  c_react <- reaction_obj(key, c_datamodel)[[1]]
  
  c_model <- c_datamodel$getModel()
  c_reacti <- CReactionInterface(c_model)
  c_reacti$initFromReaction(c_react)
  
  valid_funs <-
    get_sv(
      new(
        "_p_std__vectorT_std__string_std__allocatorT_std__string_t_t",
        ref = c_reacti$getListOfPossibleFunctions()
      )
    )
  
  fun <- rlang::arg_match(fun, valid_funs)
  
  c_reacti$setFunctionAndDoMapping(fun)
  
  if (!is.null(mappings))
    set_react_mapping(c_model, c_reacti, mappings)
  
  c_reacti$writeBackToReaction(c_react)
}

getReactionMappings <- function(key, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(is.scalar(key))
  
  c_react <- reaction_obj(key, c_datamodel)[[1]]
  
  c_model <- c_datamodel$getModel()
  c_reacti <- CReactionInterface(c_model)
  c_reacti$initFromReaction(c_react)
  
  params <- seq_along_v(c_reacti)
  names(params) <- map_chr(params, ~ c_reacti$getParameterName(.x))
  
  params_is_local <- map_lgl(params, ~ c_reacti$isLocalValue(.x))
  
  params %>%
    map_if(
      params_is_local,
      ~ c_reacti$getLocalValue(.x)
    ) %>%
    map_if(
      !params_is_local,
      ~ get_sv(c_reacti$getMappings(.x))
    )
}

setReactionMappings <- function(key, mappings, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(is.scalar(key))
  
  c_react <- reaction_obj(key, c_datamodel)[[1]]
  
  c_model <- c_datamodel$getModel()
  
  c_reacti <- CReactionInterface(c_model)
  c_reacti$initFromReaction(c_react)
  
  set_react_mapping(c_model, c_reacti, mappings)
  
  c_reacti$writeBackToReaction(c_react)
}

set_react_mapping <- function(c_model, c_reacti, mappings) {
  assert_that(is.list(mappings), !is.null(names(mappings)), noNA(names(mappings)))
  
  params <- seq_along_v(c_reacti)
  names(params) <- map_chr(params, ~ c_reacti$getParameterName(.x))
  
  names(mappings) <- map_chr(names(mappings), function(parameter) {rlang::arg_match(parameter, names(params))})
  
  iwalk(mappings, ~ {
    set_rparam_mapping(c_model, c_reacti, i = params[[.y]], value = .x)
  })
  
  assert_that(c_reacti$isValid(), msg = "Result of mapping is invalid.")
}

set_rparam_mapping <- function(c_model, c_reacti, i, value) {
  type <- c_reacti$getUsage(i)
  
  assert_that(is.scalar(value), msg = paste0("Parameter `", c_reacti$getParameterName(i), '` must be scalar.'))
  
  valid_vals <- NULL
  if (type %in% c("SUBSTRATE", "PRODUCT", "MODIFIER")) {
    key <- species_strict(value, model = c_datamodel)
    
    valid_vals <-
      c_reacti$getListOfMetabs(type) %>%
      get_sv() %>%
      species_strict(model = c_datamodel)
    
    assert_that(
      key %in% valid_vals,
      msg = paste0("Parameter `", c_reacti$getParameterName(i), '` should be one of: "', paste0(valid_vals, collapse = '", '), '".')
    )
    
    c_reacti$setMapping(i, key)
  } else if (type == "VOLUME") {
    # CReactionInterface allows mapping by ObjectName but CoRC uses ObjectDisplayName so do a bit of translation.
    c_comp <- compartment_obj(value, c_datamodel)[[1]]
    valid_vals <- compartment(model = c_datamodel)
    
    assert_that(
      c_comp$getObjectDisplayName() %in% valid_vals,
      msg = paste0("Parameter `", c_reacti$getParameterName(i), '` should be one of: "', paste0(valid_vals, collapse = '", '), '".')
    )
    
    c_reacti$setMapping(i, c_comp$getObjectName())
  } else if (type == "TIME") {
    warning("Parameter `", c_reacti$getParameterName(i), "` is a time parameter and cannot be mapped. It has been skipped.")
  } else {
    # If just any mapping I guess we go for local value or global quantity
    if (is.number(value)) {
      c_reacti$setLocalValue(i, value)
    } else {
      # CReactionInterface allows mapping by ObjectName but CoRC uses ObjectDisplayName so do a bit of translation.
      c_quant <- quantity_obj(value, c_datamodel)[[1]]
      valid_vals <- quantity(model = c_datamodel)
      
      assert_that(
        c_quant$getObjectDisplayName() %in% valid_vals,
        msg = paste0("Parameter `", c_reacti$getParameterName(i), '` should be one of: "', paste0(valid_vals, collapse = '", '), '" or a number.')
      )
      
      c_reacti$setMapping(i, c_quant$getObjectName())
    }
  }
}
