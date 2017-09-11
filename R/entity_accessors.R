#' Get species
#'
#' \code{getSpecies} returns species information as a data frame.
#'
#' @param key a character vector uniquely identifying species
#' @param rawExpression a flag on whether expressions should be raw (not converted to readable format)
#' @param model a model object
#' @return a data frame with species and associated information
#' @export
getSpecies <- function(key = NULL, rawExpressions = FALSE, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(is.flag(rawExpressions))
  
  if (is_empty(key))
    cl_metabs <- get_cdv(c_datamodel$getModel()$getMetabolites())
  else
    cl_metabs <- species_obj(key, c_datamodel)
  
  # assemble output dataframe
  tibble::tibble(
    key                     = map_swig_chr(cl_metabs, "getObjectDisplayName"),
    "Name"                  = map_swig_chr(cl_metabs, "getObjectName"),
    "Compartment"           = cl_metabs %>% map_swig("getCompartment") %>% map_swig_chr("getObjectName"),
    "Type"                  = cl_metabs %>% map_swig_chr("getStatus") %>% stringr::str_to_lower(),
    "Initial Concentration" = map_swig_dbl(cl_metabs, "getInitialConcentration"),
    "Initial Number"        = map_swig_dbl(cl_metabs, "getInitialValue"),
    "Concentration"         = map_swig_dbl(cl_metabs, "getInitialConcentration"),
    "Number"                = map_swig_dbl(cl_metabs, "getInitialValue"),
    "Expression"            = map_chr(cl_metabs, expr_to_str, c_datamodel = c_datamodel, raw = rawExpressions)
  ) %>%
    transform_names()
}

#' Get species references
#'
#' \code{getSpeciesReferences} returns species attribute references as a data frame.
#'
#' @param key a character vector uniquely identifying species
#' @param model a model object
#' @return a data frame with species and associated references
#' @export
getSpeciesReferences <- function(key = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  if (is_empty(key))
    cl_metabs <- get_cdv(c_datamodel$getModel()$getMetabolites())
  else
    cl_metabs <- species_obj(key, c_datamodel)
  
  # assemble output dataframe
  tibble::tibble(
    key                     = map_swig_chr(cl_metabs, "getObjectDisplayName"),
    "Name"                  = map_swig_chr(cl_metabs, "getObjectName"),
    "Compartment"           = cl_metabs %>% map_swig("getCompartment") %>% map_swig_chr("getObjectName"),
    "Type"                  = cl_metabs %>% map_swig_chr("getStatus") %>% stringr::str_to_lower(),
    "Initial Concentration" = cl_metabs %>% map_swig("getInitialConcentrationReference") %>% as_ref(c_datamodel),
    "Initial Number"        = cl_metabs %>% map_swig("getInitialValueReference") %>% as_ref(c_datamodel),
    "Concentration"         = cl_metabs %>% map_swig("getConcentrationReference") %>% as_ref(c_datamodel),
    "Number"                = cl_metabs %>% map_swig("getValueReference") %>% as_ref(c_datamodel),
    "Expression"            = map_chr(cl_metabs, expr_to_ref_str, c_datamodel = c_datamodel)
  ) %>%
    transform_names()
}

#' Set species
#'
#' \code{setSpecies} applies given values to species of the model depending on the 'key' parameter.
#'
#' @param key a character vector uniquely identifying species
#' @param name a character vector of names to set
#' @param type a character vector of types ("fixed", "assignment", "reactions", "ode").
#' @param initial.concentration a numeric vector of concentrations to set
#' @param initial.number a numeric vector of particle numbers to set
#' @param expression a character vector of expressions to set
#' @param data a data frame as given by \code{getSpecies} which will be applied before the other arguments.
#' @param model a model object
#' @export
setSpecies <- function(key = NULL, name = NULL, type = NULL, initial.concentration = NULL, initial.number = NULL, expression = NULL,data = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(
    is.null(key)                   || is.character(key)                 && noNA(key),
    is.null(name)                  || is.character(name)                && length(name) == length(key),
    is.null(type)                  || is.character(type)                && length(type) == length(key),
    is.null(initial.concentration) || is.numeric(initial.concentration) && length(initial.concentration) == length(key),
    is.null(initial.number)        || is.numeric(initial.number)        && length(initial.number) == length(key),
    is.null(expression)            || is.character(expression)          && length(expression) == length(key),
    is.null(data)                  || is.data.frame(data)
  )
  
  if (!is.null(type))
    type <- map_chr(type, function(type) {rlang::arg_match(type, c(NA_character_, "fixed", "assignment", "reactions", "ode"))})
  
  if (!is.null(expression))
    expression[!is.na(expression)] <- write_expr(expression[!is.na(expression)], c_datamodel)
  
  # Do this as assertion before we start changing values
  cl_metabs <- species_obj(key %||% character(), c_datamodel)
  
  # if data is provided with the data arg, run a recursive call
  # needs to be kept up to date with the function args
  if (!is.null(data))
    do.call(setSpecies, data[names(data) %in% c("key", "name", "type", "initial.concentration", "initial.number", "expression")])
  
  if (is_empty(cl_metabs)) return(invisible())
  
  c_vals_to_update <- ObjectStdVector()
  
  # apply names
  if (!is.null(name)) {
    walk2(
      cl_metabs, name,
      ~ if (!is.na(.y)) .x$setObjectName(.y)
    )
  }
  
  # apply types
  if (!is.null(type)) {
    type_to_c <- stringr::str_to_upper(type)
    walk2(
      cl_metabs, type_to_c,
      ~ if (!is.na(.y)) .x$setStatus(.y)
    )
  }
  
  # apply concentrations
  if (!is.null(initial.concentration)) {
    walk2(
      cl_metabs, initial.concentration,
      ~ {
        if (!is.na(.y)) {
          .x$setInitialConcentration(.y)
          c_vals_to_update$push_back(.x$getInitialConcentrationReference())
        }
      }
    )
  }
  
  # apply particlenum
  if (!is.null(initial.number)) {
    walk2(
      cl_metabs, initial.number,
      ~ {
        if (!is.na(.y)) {
          .x$setInitialValue(.y)
          c_vals_to_update$push_back(.x$getInitialValueReference())
        }
      }
    )
  }
  
  # apply expressions
  if (!is.null(expression)) {
    walk2(
      cl_metabs, expression,
      ~ {
        if (!is.na(.y)) {
          assert_that(
            grab_msg(.x$setExpression(.y)$isSuccess()),
            msg = "Failed when applying an expression."
          )
        }
      }
    )
  }
  
  c_model <- c_datamodel$getModel()
  
  c_model$updateInitialValues(c_vals_to_update)
  
  c_model$compileIfNecessary()
  
  # model$initializeMetabolites()
  
  invisible()
}

#'  Get global quantities
#'
#' \code{getGlobalQuantities} returns global quantities as a data frame.
#'
#' @param key a character vector uniquely identifying global quantities
#' @param rawExpression a flag on whether expressions should be raw (not converted to readable format)
#' @param model a model object
#' @return a data frame with global quantities and associated information
#' @export
getGlobalQuantities <- function(key = NULL, rawExpressions = FALSE, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(is.flag(rawExpressions))
  
  if (is_empty(key))
    cl_quants <- get_cdv(c_datamodel$getModel()$getModelValues())
  else
    cl_quants <- quantity_obj(key, c_datamodel)

  # assemble output dataframe
  tibble::tibble(
    key             = map_swig_chr(cl_quants, "getObjectDisplayName"),
    "Name"          = map_swig_chr(cl_quants, "getObjectName"),
    "Type"          = cl_quants %>% map_swig_chr("getStatus") %>% stringr::str_to_lower(),
    "Initial Value" = map_swig_dbl(cl_quants, "getInitialValue"),
    "Value"         = map_swig_dbl(cl_quants, "getValue"),
    "Expression"    = map_chr(cl_quants, expr_to_str, c_datamodel = c_datamodel, raw = rawExpressions)
  ) %>%
    transform_names()
}

#'  Get global quantitiy references
#'
#' \code{getGlobalQuantityReferences} returns global quantity attribute references as a data frame.
#'
#' @param key a character vector uniquely identifying global quantities
#' @param model a model object
#' @return a data frame with global quantities and associated references
#' @export
getGlobalQuantityReferences <- function(key = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  if (is_empty(key))
    cl_quants <- get_cdv(c_datamodel$getModel()$getModelValues())
  else
    cl_quants <- quantity_obj(key, c_datamodel)
  
  # assemble output dataframe
  tibble::tibble(
    key             = map_swig_chr(cl_quants, "getObjectDisplayName"),
    "Name"          = map_swig_chr(cl_quants, "getObjectName"),
    "Type"          = cl_quants %>% map_swig_chr("getStatus") %>% stringr::str_to_lower(),
    "Initial Value" = cl_quants %>% map_swig("getInitialValueReference") %>% as_ref(c_datamodel),
    "Value"         = cl_quants %>% map_swig("getValueReference") %>% as_ref(c_datamodel),
    "Expression"    = map_chr(cl_quants, expr_to_ref_str, c_datamodel = c_datamodel)
  ) %>%
    transform_names()
}

#' Set global quantities
#'
#' \code{setGlobalQuantities} applies given values to global quantities of the model depending on the 'key' parameter.
#'
#' @param key a character vector uniquely identifying global quantities
#' @param name a character vector of names to set
#' @param type a character vector of types ("fixed", "assignment", "ode").
#' @param initial.volume a numeric vector of values to set
#' @param expression a character vector of expressions to set
#' @param data a data frame as given by \code{getGlobalQuantities} which will be applied before the other arguments.
#' @param model a model object
#' @export
setGlobalQuantities <- function(key = NULL, name = NULL, type = NULL, initial.value = NULL, expression = NULL, data = NULL, c_datamodel = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(
    is.null(key)           || is.character(key)         && noNA(key),
    is.null(name)          || is.character(name)        && length(name) == length(key),
    is.null(type)          || is.character(type)        && length(type) == length(key),
    is.null(initial.value) || is.numeric(initial.value) && length(initial.value) == length(key),
    is.null(expression)    || is.character(expression)  && length(expression) == length(key),
    is.null(data)          || is.data.frame(data)
  )
  
  if (!is.null(type))
    type <- map_chr(type, function(type) {rlang::arg_match(type, c(NA_character_, "fixed", "assignment", "ode"))})
  
  if (!is.null(expression))
    expression[!is.na(expression)] <- write_expr(expression[!is.na(expression)], c_datamodel)
  
  # Do this as assertion before we start changing values
  c_quants <- quantity_obj(key %||% character(), c_datamodel)
  
  # if data is provided with the data arg, run a recursive call
  # needs to be kept up to date with the function args
  if (!is.null(data))
    do.call(setGlobalQuantities, data[names(data) %in% c("key", "name", "type", "initial.value", "expression")])
  
  if (is_empty(c_quants)) return(invisible())
  
  c_vals_to_update <- ObjectStdVector()
  
  # apply names
  if (!is.null(name)) {
    walk2(
      cl_quants, name,
      ~ if (!is.na(.y)) .x$setObjectName(.y)
    )
  }
  
  # apply types
  if (!is.null(type)) {
    type_to_c <- stringr::str_to_upper(type)
    walk2(
      cl_quants, type_to_c,
      ~ if (!is.na(.y)) .x$setStatus(.y)
    )
  }
  
  # apply value
  if (!is.null(initial.value)) {
    walk2(
      cl_quants, initial.value,
      ~ {
        if (!is.na(.y)) {
          .x$setInitialValue(.y)
          c_vals_to_update$push_back(.x$getInitialValueReference())
        }
      }
    )
  }
  
  # apply expressions
  if (!is.null(expression)) {
    walk2(
      cl_quants, expression,
      ~ {
        if (!is.na(.y)) {
          assert_that(
            grab_msg(.x$setExpression(.y)$isSuccess()),
            msg = "Failed when applying an expression."
          )
        }
      }
    )
  }
  
  c_model <- c_datamodel$getModel()
  
  c_model$updateInitialValues(c_vals_to_update)
  
  c_model$compileIfNecessary()
  
  # model$initializeMetabolites()
  
  invisible()
}

#'  Get compartments
#'
#' \code{getCompartments} returns compartments as a data frame.
#'
#' @param key a character vector uniquely identifying compartments
#' @param rawExpression a flag on whether expressions should be raw (not converted to readable format)
#' @param model a model object
#' @return a data frame with compartments and associated information
#' @export
getCompartments <- function(key = NULL, rawExpressions = FALSE, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(is.flag(rawExpressions))
  
  if (is_empty(key))
    cl_comps <- get_cdv(c_datamodel$getModel()$getCompartments())
  else
    cl_comps <- compartment_obj(key, c_datamodel)
  
  # assemble output dataframe
  tibble::tibble(
    key              = map_swig_chr(cl_comps, "getObjectDisplayName"),
    "Name"           = map_swig_chr(cl_comps, "getObjectName"),
    "Type"           = cl_comps %>% map_swig_chr("getStatus") %>% stringr::str_to_lower(),
    "Initial Volume" = map_swig_dbl(cl_comps, "getInitialValue"),
    "Expression"     = map_chr(cl_comps, expr_to_str, c_datamodel = c_datamodel, raw = rawExpressions)
  ) %>%
    transform_names()
}

#'  Get compartment references
#'
#' \code{getCompartmentReferences} returns compartment attribute references as a data frame.
#'
#' @param key a character vector uniquely identifying compartments
#' @param model a model object
#' @return a data frame with compartments and associated references
#' @export
getCompartmentReferences <- function(key = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  if (is_empty(key))
    cl_comps <- get_cdv(c_datamodel$getModel()$getCompartments())
  else
    cl_comps <- compartment_obj(key, c_datamodel)
  
  # assemble output dataframe
  tibble::tibble(
    key              = map_swig_chr(cl_comps, "getObjectDisplayName"),
    "Name"           = map_swig_chr(cl_comps, "getObjectName"),
    "Type"           = cl_comps %>% map_swig_chr("getStatus") %>% stringr::str_to_lower(),
    "Initial Volume" = cl_comps %>% map_swig("getInitialValueReference") %>% as_ref(c_datamodel),
    "Expression"     = map_chr(cl_comps, expr_to_ref_str, c_datamodel = c_datamodel)
  ) %>%
    transform_names()
}

#' Set compartments
#'
#' \code{setCompartments} applies given values to compartments of the model depending on the 'key' parameter.
#'
#' @param key a character vector uniquely identifying compartments
#' @param name a character vector of names to set
#' @param type a character vector of species types ("fixed", "assignment", "ode").
#' @param initial.volume a numeric vector of values to set
#' @param expression a character vector of expressions to set
#' @param data a data frame as given by \code{getCompartments} which will be applied before the other arguments.
#' @param model a model object
#' @export
setCompartments <- function(key = NULL, name = NULL, type = NULL, initial.volume = NULL, expression = NULL, data = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(
    is.null(key)            || is.character(key)          && noNA(key),
    is.null(name)           || is.character(name)         && length(name) == length(key),
    is.null(type)           || is.character(type)         && length(type) == length(key),
    is.null(initial.volume) || is.numeric(initial.volume) && length(initial.volume) == length(key),
    is.null(expression)     || is.character(expression)   && length(expression) == length(key),
    is.null(data)           || is.data.frame(data)
  )
  
  if (!is.null(type))
    type <- map_chr(type, function(type) {rlang::arg_match(type, c(NA_character_, "fixed", "assignment", "ode"))})
  
  if (!is.null(expression))
    expression[!is.na(expression)] <- write_expr(expression[!is.na(expression)], c_datamodel)
  
  # Do this as assertion before we start changing values
  cl_comps <- compartment_obj(key %||% character(), c_datamodel)
  
  # if data is provided with the data arg, run a recursive call
  # needs to be kept up to date with the function args
  if (!is.null(data))
    do.call(setCompartments, data[names(data) %in% c("key", "name", "type", "initial.volume", "expression")])
  
  if (is_empty(cl_comps)) return(invisible())
  
  # apparently I need to give changedObjects because I cant update initial values without
  c_vals_to_update <- ObjectStdVector()
  
  # apply names
  if (!is.null(name)) {
    walk2(
      cl_comps, name,
      ~ if (!is.na(.y)) .x$setObjectName(.y)
    )
  }
  
  # apply types
  if (!is.null(type)) {
    type_to_c <- stringr::str_to_upper(type)
    walk2(
      cl_quants, type_to_c,
      ~ if (!is.na(.y)) .x$setStatus(.y)
    )
  }
  
  # apply volume
  if (!is.null(initial.volume)) {
    walk2(
      cl_comps, initial.volume,
      ~ {
        if (!is.na(.y)) {
          .x$setInitialValue(.y)
          c_vals_to_update$push_back(.x$getInitialValueReference())
        }
      }
    )
  }
  
  # apply expressions
  if (!is.null(expression)) {
    walk2(
      cl_quants, expression,
      ~ {
        if (!is.na(.y)) {
          assert_that(
            grab_msg(.x$setExpression(.y)$isSuccess()),
            msg = "Failed when applying an expression."
          )
        }
      }
    )
  }
  
  c_model <- c_datamodel$getModel()
  
  c_model$updateInitialValues(c_vals_to_update)
  
  c_model$compileIfNecessary()
  
  # model$initializeMetabolites()
  
  invisible()
}

#'  Get reactions
#'
#' \code{getReactions} returns reactions as a data frame.
#'
#' @param key a character vector uniquely identifying reactions
#' @param model a model object
#' @return a data frame with reactions and associated information
#' @export
getReactions <- function(key = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  if (is_empty(key))
    cl_reacts <- get_cdv(c_datamodel$getModel()$getReactions())
  else
    cl_reacts <- reaction_obj(key, c_datamodel)

  # assemble output dataframe
  tibble::tibble(
    key    = map_swig_chr(cl_reacts, "getObjectDisplayName"),
    "Name" = map_swig_chr(cl_reacts, "getObjectName"),
    "Scheme" = map_swig_chr(cl_reacts, "getReactionScheme")
  ) %>%
    transform_names()
}

#' Set reactions
#'
#' \code{setReactions} applies given values to reactions of the model depending on the 'key' parameter.
#'
#' @param key a character vector uniquely identifying reactions
#' @param name a character vector of names to set
#' @param data a data frame as given by \code{getReactions} which will be applied before the other arguments.
#' @param model a model object
#' @export
setReactions <- function(key = NULL, name = NULL, data = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(
    is.null(key)       || is.character(key)  && noNA(key),
    is.null(name)      || is.character(name) && length(name) == length(key),
    is.null(data)      || is.data.frame(data)
  )
  
  # Do this as assertion before we start changing values
  cl_reacts <- reaction_obj(key %||% character(), c_datamodel)
  
  # if data is provided with the data arg, run a recursive call
  # needs to be kept up to date with the function args
  if (!is.null(data))
    do.call(setReactions, data[names(data) %in% c("key", "name")])
  
  if (is_empty(cl_reacts)) return(invisible())
  
  # apply names
  if (!is.null(name)) {
    walk2(
      cl_reacts, name,
      ~ if (!is.na(.y)) .x$setObjectName(.y)
    )
  }
  
  invisible()
}

#'  Get reaction parameters
#'
#' \code{getParameters} returns reaction parameters as a data frame.
#'
#' @param key a character vector uniquely identifying reactions parameters
#' @param model a model object
#' @return a data frame with reaction parameters and associated information
#' @export
getParameters <- function(key = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  c_keyfactory <- CRootContainer_getKeyFactory()
  
  if (is_empty(key))
    cl_params <-
      get_cdv(c_datamodel$getModel()$getReactions()) %>%
      map_swig("getParameters") %>%
      map(function(paramgrp) {
        seq_along_v(paramgrp) %>% map(~ paramgrp$getParameter(.x))
      }) %>%
      flatten()
  else
    cl_params <- parameter_obj(key, c_datamodel)
  
  cl_reacts <- map_swig(cl_params, "getObjectAncestor", "Reaction") %>% map(as, "_p_CReaction")
  
  names <- map_swig_chr(cl_params, "getObjectName")
  
  are_local <- map2_lgl(names, cl_reacts, ~ .y$isLocalParameter(.x))
  
  values <- rep(NA_real_, length(cl_params))
  values[are_local] <-
    cl_params[are_local] %>%
    map_swig_dbl("getDblValue")
  
  mappings <- rep(NA_character_, length(cl_params))
  mappings[!are_local] <- 
    map2_chr(names[!are_local], cl_reacts[!are_local],
      function(name, c_react) {
        val <- get_sv(c_react$getParameterMapping(name))
        
        # For now don't support multiple mappings
        if (length(val) > 1)
          return("<MULTIPLE>")
  
        c_keyfactory$get(val)$getObjectDisplayName()
      }
    )
  
  # assemble output dataframe
  tibble::tibble(
    key        = map_swig_chr(cl_params, "getObjectDisplayName"),
    "Name"     = names,
    "Reaction" = cl_reacts %>% map_swig_chr("getObjectName"),
    "Value"    = values,
    "Mapping"  = mappings
  ) %>%
    transform_names()
}

#'  Get reaction parameter references
#'
#' \code{getParameterReferences} returns reaction parameters as a data frame.
#'
#' @param key a character vector uniquely identifying reactions parameters
#' @param model a model object
#' @return a data frame with reaction parameters and associated references
#' @export
getParameterReferences <- function(key = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  key <- parameter(key = key %||% character(), model = c_datamodel)
  
  if (is_empty(key))
    cl_params <-
      get_cdv(c_datamodel$getModel()$getReactions()) %>%
      map_swig("getParameters") %>%
      map(function(paramgrp) {
        seq_along_v(paramgrp) %>% map(~ paramgrp$getParameter(.x))
      }) %>%
      flatten()
  else
    # no dn_to_object here because it doesnt work for parameters
    # cl_params <- map(key, dn_to_object, c_datamodel, "_p_CCopasiParameter")
    cl_params <- parameter_obj(key, c_datamodel)
  
  cl_reacts <- map_swig(cl_params, "getObjectAncestor", "Reaction") %>% map(as, "_p_CReaction")
  
  names <- map_swig_chr(cl_params, "getObjectName")
  
  # find out what the parameters are mapped to
  mappings <- map2_chr(names, cl_reacts,
    function(name, c_react) {
      if (c_react$isLocalParameter(name))
        return(NA_character_)
    
      val <- get_sv(c_react$getParameterMapping(name))
    
      # For now don't support multiple mappings
      if (length(val) > 1)
        return("<MULTIPLE>")
    
      c_keyfactory$get(val)$getObjectDisplayName()
    }
  )
  # assemble output dataframe
  tibble::tibble(
    key        = map_swig_chr(cl_params, "getObjectDisplayName"),
    "Name"     = names,
    "Reaction" = cl_params %>% map_swig("getObjectParent") %>% map_swig("getObjectParent") %>% map_swig_chr("getObjectName"),
    "Type"     = cl_params %>% map_swig_chr("getType") %>% stringr::str_to_lower(),
    "Value"    = cl_params %>% map_swig("getValueReference") %>% as_ref(c_datamodel),
    "Mapping"  = mappings
  ) %>%
    transform_names()
}

#' Set reaction parameters
#'
#' \code{setParameters} applies given values to reaction parameters of the model depending on the 'key' parameter.
#'
#' @param key a character vector uniquely identifying reaction parameters
#' @param name a character vector of names to set
#' @param data a data frame as given by \code{getParameters} which will be applied before the other arguments.
#' @param model a model object
#' @export
setParameters <- function(key = NULL, name = NULL, value = NULL, mapping = NULL, data = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(
    is.null(key)      || is.character(key)     && noNA(key),
    is.null(name)     || is.character(name)    && length(name) == length(key),
    is.null(value)    || is.numeric(value)     && length(value) == length(key),
    is.null(mapping)  || is.character(mapping) && length(mapping) == length(key),
    is.null(data)     || is.data.frame(data)
  )
  
  # Do this as assertion before we start changing values
  cl_params <- parameter_obj(key %||% character(), c_datamodel)
  
  # Do this as assertion before we start changing values
  # Makes sure mapping is either NA or a copasi key
  if (!is.null(mapping)) {
    mapping[!is.na(mapping)] <-
      mapping[!is.na(mapping)] %>%
      quantity_obj(c_datamodel) %>%
      map_swig_chr("getKey")
  }
  
  # if data is provided with the data arg, run a recursive call
  # needs to be kept up to date with the function args
  if (!is.null(data))
    do.call(setParameters, data[names(data) %in% c("key", "name", "value", "mapping")])
  
  if (is_empty(cl_params)) return(invisible())
  
  # apply names
  if (!is.null(name)) {
    walk2(
      cl_params, name,
      ~ if (!is.na(.y)) .x$setObjectName(.y)
    )
  }
  
  # Parameters are only those of type "PARAMETER I think.
  # So I can safely set a value and make them local or set a mapping
  # as long as that mapping is a global quantity.
  # Changing the parameters directly seems to be unsafe.
  # The safe method seems to be to go back to the reaction and do manipulations from there.
  
  # apply values
  if (!is.null(value) || !is.null(mapping)) {
    cl_reacts <- map_swig(cl_params, "getObjectAncestor", "Reaction") %>% map(as, "_p_CReaction")
    names <- map_swig_chr(cl_params, "getObjectName")
    
    if (!is.null(value))
      pwalk(
        list(cl_reacts, names, value),
        function(c_react, name, value) {
          if (!is.na(value))
            c_react$setParameterValue(name, value)
        }
      )
    
    if (!is.null(mapping))
      pwalk(
        list(cl_reacts, names, mapping),
        function(c_react, name, value) {
          if (!is.na(value))
            c_react$setParameterMapping(name, value)
        }
      )
    
    cl_reacts %>%
      unique() %>%
      walk_swig("compile")
  }
  
  c_datamodel$getModel()$compileIfNecessary()
  
  invisible()
}
