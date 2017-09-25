#' Get species
#'
#' \code{getSpecies} returns species information as a data frame.
#'
#' @param key a character vector uniquely identifying species
#' @param raw_expressions a flag on whether expressions should be raw (not converted to readable format)
#' @param model a model object
#' @return a data frame with species and associated information
#' @seealso \code{\link{getSpeciesReferences}} \code{\link{setSpecies}}
#' @family species functions
#' @export
getSpecies <- function(key = NULL, raw_expressions = FALSE, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(is.flag(raw_expressions))
  
  if (is_empty(key))
    cl_metabs <- get_cdv(c_datamodel$getModel()$getMetabolites())
  else
    cl_metabs <- species_obj(key, c_datamodel)
  
  # assemble output dataframe
  tibble::tibble(
    key                     = map_swig_chr(cl_metabs, "getObjectDisplayName"),
    "Name"                  = map_swig_chr(cl_metabs, "getObjectName"),
    "Compartment"           = cl_metabs %>% map_swig("getCompartment") %>% map_swig_chr("getObjectName"),
    "Type"                  = cl_metabs %>% map_swig_chr("getStatus") %>% tolower(),
    "Initial Concentration" = map_swig_dbl(cl_metabs, "getInitialConcentration"),
    "Initial Number"        = map_swig_dbl(cl_metabs, "getInitialValue"),
    "Concentration"         = map_swig_dbl(cl_metabs, "getConcentration"),
    "Number"                = map_swig_dbl(cl_metabs, "getValue"),
    "Rate"                  = map_swig_dbl(cl_metabs, "getConcentrationRate"),
    "Number Rate"           = map_swig_dbl(cl_metabs, "getRate"),
    "Initial Expression"    = map_chr(cl_metabs, iexpr_to_str, c_datamodel = c_datamodel, raw = raw_expressions),
    "Expression"            = map_chr(cl_metabs, expr_to_str, c_datamodel = c_datamodel, raw = raw_expressions)
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
#' @seealso \code{\link{getSpecies}} \code{\link{setSpecies}}
#' @family species functions
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
    "Type"                  = cl_metabs %>% map_swig_chr("getStatus") %>% tolower(),
    "Initial Concentration" = cl_metabs %>% map_swig("getInitialConcentrationReference") %>% as_ref(c_datamodel),
    "Initial Number"        = cl_metabs %>% map_swig("getInitialValueReference") %>% as_ref(c_datamodel),
    "Concentration"         = cl_metabs %>% map_swig("getConcentrationReference") %>% as_ref(c_datamodel),
    "Number"                = cl_metabs %>% map_swig("getValueReference") %>% as_ref(c_datamodel),
    "Rate"                  = cl_metabs %>% map_swig("getConcentrationRateReference") %>% as_ref(c_datamodel),
    "Number Rate"           = cl_metabs %>% map_swig("getRateReference") %>% as_ref(c_datamodel),
    "Initial Expression"    = map_chr(cl_metabs, iexpr_to_ref_str, c_datamodel = c_datamodel),
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
#' @param initial_concentration a numeric vector of concentrations to set
#' @param initial_number a numeric vector of particle numbers to set
#' @param expression a character vector of expressions to set
#' @param data a data frame as given by \code{\link{getSpecies}} which will be applied before the other arguments.
#' @param model a model object
#' @seealso \code{\link{getSpecies}} \code{\link{getSpeciesReferences}}
#' @family species functions
#' @export
setSpecies <- function(key = NULL, name = NULL, compartment = NULL, type = NULL, initial_concentration = NULL, initial_number = NULL, expression = NULL, data = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(
    is.null(name)                  || is.character(name)                && length(name) == length(key),
    is.null(type)                  || is.character(type)                && length(type) == length(key),
    is.null(compartment)           || is.character(compartment)         && length(compartment)  == length(key),
    is.null(initial_concentration) || is.numeric(initial_concentration) && length(initial_concentration) == length(key),
    is.null(initial_number)        || is.numeric(initial_number)        && length(initial_number) == length(key),
    is.null(expression)            || is.character(expression)          && length(expression) == length(key),
    is.null(data)                  || is.data.frame(data)
  )
  
  # Do this as assertion before we start changing values
  cl_metabs <- species_obj(key %||% character(), c_datamodel)
  
  # assemble compartments
  if (!is.null(compartment)) {
    comps_to_write <- !is.na(compartment)
    
    cl_comps_new <- vector("list", length(key))
    cl_comps_new[comps_to_write] <- compartment_obj(compartment[comps_to_write], c_datamodel)
  }
  
  if (!is.null(type))
    type <- map_chr(type, function(type) rlang::arg_match(type, c(NA_character_, "fixed", "assignment", "reactions", "ode")))
  
  if (!is.null(expression))
    expression[!is.na(expression)] <- write_expr(expression[!is.na(expression)], c_datamodel)
  
  # if data is provided with the data arg, run a recursive call
  # needs to be kept up to date with the function args
  if (!is.null(data))
    do.call(setSpecies, data[names(data) %in% c("key", "name", "type", "initial_concentration", "initial_number", "expression")])
  
  if (is_empty(cl_metabs)) return(invisible())
  
  c_model <- c_datamodel$getModel()
  
  # apply names
  if (!is.null(name)) {
    walk2(
      cl_metabs, name,
      ~ if (!is.na(.y)) .x$setObjectName(.y)
    )
  }
  
  # apply compartments
  if (!is.null(compartment)) {
    comp_changed <- FALSE
    pwalk(
      list(c_metab = cl_metabs, write = comps_to_write, c_comp_new = cl_comps_new),
      function(c_metab, write, c_comp_new) {
        if (write && c_metab$getCompartment()$getKey() != c_comp_new$getKey()) {
          assert_that(
            grab_msg(c_comp_new$addMetabolite(c_metab)),
            msg = "Failed to move species."
          )
          # remove from old compartment by name (by pointer is not exported by swig.)
          # looks like there is some method that will remove the metab when adding it to another comp.
          # c_comp_old$getMetabolites()$removeByName(c_metab$getObjectName())
          comp_changed <<- TRUE
        }
      }
    )
    
    if (comp_changed) {
      c_model$initializeMetabolites()
      c_model$setCompileFlag()
    }
  }
  
  # apply types
  if (!is.null(type)) {
    walk2(
      cl_metabs, toupper(type),
      ~ if (!is.na(.y)) .x$setStatus(.y)
    )
  }
  
  # apply concentrations
  if (!is.null(initial_concentration)) {
    walk2(
      cl_metabs, initial_concentration,
      ~ if (!is.na(.y)) .x$setInitialConcentration(.y)
    )
    c_model$updateInitialValues("Concentration")
  }
  
  # apply particlenum
  if (!is.null(initial_number)) {
    walk2(
      cl_metabs, initial_number,
      ~ if (!is.na(.y)) .x$setInitialValue(.y)
    )
    c_model$updateInitialValues("ParticleNumbers")
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
  
  c_model$compileIfNecessary()
  
  invisible()
}

#' Get global quantities
#'
#' \code{getGlobalQuantities} returns global quantities as a data frame.
#'
#' @param key a character vector uniquely identifying global quantities
#' @param raw_expressions a flag on whether expressions should be raw (not converted to readable format)
#' @param model a model object
#' @return a data frame with global quantities and associated information
#' @seealso \code{\link{getGlobalQuantityReferences}} \code{\link{setGlobalQuantities}}
#' @family global quantity functions
#' @export
getGlobalQuantities <- function(key = NULL, raw_expressions = FALSE, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(is.flag(raw_expressions))
  
  if (is_empty(key))
    cl_quants <- get_cdv(c_datamodel$getModel()$getModelValues())
  else
    cl_quants <- quantity_obj(key, c_datamodel)

  # assemble output dataframe
  tibble::tibble(
    key                  = map_swig_chr(cl_quants, "getObjectDisplayName"),
    "Name"               = map_swig_chr(cl_quants, "getObjectName"),
    "Type"               = cl_quants %>% map_swig_chr("getStatus") %>% tolower(),
    "Initial Value"      = map_swig_dbl(cl_quants, "getInitialValue"),
    "Value"              = map_swig_dbl(cl_quants, "getValue"),
    "Rate"               = map_swig_dbl(cl_quants, "getRate"),
    "Initial Expression" = map_chr(cl_quants, iexpr_to_str, c_datamodel = c_datamodel, raw = raw_expressions),
    "Expression"         = map_chr(cl_quants, expr_to_str, c_datamodel = c_datamodel, raw = raw_expressions)
  ) %>%
    transform_names()
}

#' Get global quantitiy references
#'
#' \code{getGlobalQuantityReferences} returns global quantity attribute references as a data frame.
#'
#' @param key a character vector uniquely identifying global quantities
#' @param model a model object
#' @return a data frame with global quantities and associated references
#' @seealso \code{\link{getGlobalQuantities}} \code{\link{setGlobalQuantities}}
#' @family global quantity functions
#' @export
getGlobalQuantityReferences <- function(key = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  if (is_empty(key))
    cl_quants <- get_cdv(c_datamodel$getModel()$getModelValues())
  else
    cl_quants <- quantity_obj(key, c_datamodel)
  
  # assemble output dataframe
  tibble::tibble(
    key                  = map_swig_chr(cl_quants, "getObjectDisplayName"),
    "Name"               = map_swig_chr(cl_quants, "getObjectName"),
    "Type"               = cl_quants %>% map_swig_chr("getStatus") %>% tolower(),
    "Initial Value"      = cl_quants %>% map_swig("getInitialValueReference") %>% as_ref(c_datamodel),
    "Value"    = cl_quants %>% map_swig("getValueReference") %>% as_ref(c_datamodel),
    "Rate"               = cl_quants %>% map_swig("getRateReference") %>% as_ref(c_datamodel),
    "Initial Expression" = map_chr(cl_quants, iexpr_to_ref_str, c_datamodel = c_datamodel),
    "Expression"         = map_chr(cl_quants, expr_to_ref_str, c_datamodel = c_datamodel)
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
#' @param data a data frame as given by \code{\link{getGlobalQuantities}} which will be applied before the other arguments.
#' @param model a model object
#' @seealso \code{\link{getGlobalQuantities}} \code{\link{getGlobalQuantityReferences}}
#' @family global quantity functions
#' @export
setGlobalQuantities <- function(key = NULL, name = NULL, type = NULL, initial_value = NULL, expression = NULL, data = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(
    is.null(name)          || is.character(name)        && length(name) == length(key),
    is.null(type)          || is.character(type)        && length(type) == length(key),
    is.null(initial_value) || is.numeric(initial_value) && length(initial_value) == length(key),
    is.null(expression)    || is.character(expression)  && length(expression) == length(key),
    is.null(data)          || is.data.frame(data)
  )
  
  # Do this as assertion before we start changing values
  c_quants <- quantity_obj(key %||% character(), c_datamodel)
  
  if (!is.null(type))
    type <- map_chr(type, function(type) rlang::arg_match(type, c(NA_character_, "fixed", "assignment", "ode")))
  
  if (!is.null(expression))
    expression[!is.na(expression)] <- write_expr(expression[!is.na(expression)], c_datamodel)
  
  # if data is provided with the data arg, run a recursive call
  # needs to be kept up to date with the function args
  if (!is.null(data))
    do.call(setGlobalQuantities, data[names(data) %in% c("key", "name", "type", "initial_value", "expression")])
  
  if (is_empty(c_quants)) return(invisible())
  
  c_model <- c_datamodel$getModel()
  
  # apply names
  if (!is.null(name)) {
    walk2(
      cl_quants, name,
      ~ if (!is.na(.y)) .x$setObjectName(.y)
    )
  }
  
  # apply types
  if (!is.null(type)) {
    walk2(
      cl_quants, toupper(type),
      ~ if (!is.na(.y)) .x$setStatus(.y)
    )
  }
  
  # apply value
  if (!is.null(initial_value)) {
    walk2(
      cl_quants, initial_value,
      ~ if (!is.na(.y)) .x$setInitialValue(.y)
    )
    c_model$updateInitialValues("ParticleNumbers")
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
  
  c_model$compileIfNecessary()
  
  invisible()
}

#' Get compartments
#'
#' \code{getCompartments} returns compartments as a data frame.
#'
#' @param key a character vector uniquely identifying compartments
#' @param raw_expressions a flag on whether expressions should be raw (not converted to readable format)
#' @param model a model object
#' @return a data frame with compartments and associated information
#' @seealso \code{\link{getCompartmentReferences}} \code{\link{setCompartments}}
#' @family compartment functions
#' @export
getCompartments <- function(key = NULL, raw_expressions = FALSE, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(is.flag(raw_expressions))
  
  if (is_empty(key))
    cl_comps <- get_cdv(c_datamodel$getModel()$getCompartments())
  else
    cl_comps <- compartment_obj(key, c_datamodel)
  
  # assemble output dataframe
  tibble::tibble(
    key                  = map_swig_chr(cl_comps, "getObjectDisplayName"),
    "Name"               = map_swig_chr(cl_comps, "getObjectName"),
    "Type"               = cl_comps %>% map_swig_chr("getStatus") %>% tolower(),
    "Initial Size"       = map_swig_dbl(cl_comps, "getInitialValue"),
    "Size"               = map_swig_dbl(cl_comps, "getValue"),
    "Rate"               = map_swig_dbl(cl_comps, "getRate"),
    "Initial Expression" = map_chr(cl_comps, iexpr_to_str, c_datamodel = c_datamodel, raw = raw_expressions),
    "Expression"         = map_chr(cl_comps, expr_to_str, c_datamodel = c_datamodel, raw = raw_expressions)
  ) %>%
    transform_names()
}

#' Get compartment references
#'
#' \code{getCompartmentReferences} returns compartment attribute references as a data frame.
#'
#' @param key a character vector uniquely identifying compartments
#' @param model a model object
#' @return a data frame with compartments and associated references
#' @seealso \code{\link{getCompartments}} \code{\link{setCompartments}}
#' @family compartment functions
#' @export
getCompartmentReferences <- function(key = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  if (is_empty(key))
    cl_comps <- get_cdv(c_datamodel$getModel()$getCompartments())
  else
    cl_comps <- compartment_obj(key, c_datamodel)
  
  # assemble output dataframe
  tibble::tibble(
    key                  = map_swig_chr(cl_comps, "getObjectDisplayName"),
    "Name"               = map_swig_chr(cl_comps, "getObjectName"),
    "Type"               = cl_comps %>% map_swig_chr("getStatus") %>% tolower(),
    "Initial Size"       = cl_comps %>% map_swig("getInitialValueReference") %>% as_ref(c_datamodel),
    "Size"               = cl_comps %>% map_swig("getValueReference") %>% as_ref(c_datamodel),
    "Rate"               = cl_comps %>% map_swig("getRateReference") %>% as_ref(c_datamodel),
    "Initial Expression" = map_chr(cl_comps, iexpr_to_ref_str, c_datamodel = c_datamodel),
    "Expression"         = map_chr(cl_comps, expr_to_ref_str, c_datamodel = c_datamodel)
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
#' @param initial_size a numeric vector of values to set
#' @param expression a character vector of expressions to set
#' @param data a data frame as given by \code{\link{getCompartments}} which will be applied before the other arguments.
#' @param model a model object
#' @seealso \code{\link{getCompartments}} \code{\link{getCompartmentReferences}}
#' @family compartment functions
#' @export
setCompartments <- function(key = NULL, name = NULL, type = NULL, initial_size = NULL, expression = NULL, data = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(
    is.null(name)           || is.character(name)         && length(name) == length(key),
    is.null(type)           || is.character(type)         && length(type) == length(key),
    is.null(initial_size)   || is.numeric(initial_size)   && length(initial_size) == length(key),
    is.null(expression)     || is.character(expression)   && length(expression) == length(key),
    is.null(data)           || is.data.frame(data)
  )
  
  # Do this as assertion before we start changing values
  cl_comps <- compartment_obj(key %||% character(), c_datamodel)
  
  if (!is.null(type))
    type <- map_chr(type, function(type) rlang::arg_match(type, c(NA_character_, "fixed", "assignment", "ode")))
  
  if (!is.null(expression))
    expression[!is.na(expression)] <- write_expr(expression[!is.na(expression)], c_datamodel)
  
  # if data is provided with the data arg, run a recursive call
  # needs to be kept up to date with the function args
  if (!is.null(data))
    do.call(setCompartments, data[names(data) %in% c("key", "name", "type", "initial_size", "expression")])
  
  if (is_empty(cl_comps)) return(invisible())
  
  c_model <- c_datamodel$getModel()
  
  # apply names
  if (!is.null(name)) {
    walk2(
      cl_comps, name,
      ~ if (!is.na(.y)) .x$setObjectName(.y)
    )
  }
  
  # apply types
  if (!is.null(type)) {
    walk2(
      cl_quants, toupper(type),
      ~ if (!is.na(.y)) .x$setStatus(.y)
    )
  }
  
  # apply volume
  if (!is.null(initial_size)) {
    walk2(
      cl_comps, initial_size,
      ~ if (!is.na(.y)) .x$setInitialValue(.y)
    )
    c_model$updateInitialValues("ParticleNumbers")
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
  
  c_model$compileIfNecessary()
  
  invisible()
}

#' Get reactions
#'
#' \code{getReactions} returns reactions as a data frame.
#'
#' @param key a character vector uniquely identifying reactions
#' @param model a model object
#' @return a data frame with reactions and associated information
#' @seealso \code{\link{getReactionReferences}} \code{\link{setReactions}}
#' @family reaction functions
#' @export
getReactions <- function(key = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  if (is_empty(key))
    cl_reacts <- get_cdv(c_datamodel$getModel()$getReactions())
  else
    cl_reacts <- reaction_obj(key, c_datamodel)
  
  # assemble output dataframe
  tibble::tibble(
    key           = map_swig_chr(cl_reacts, "getObjectDisplayName"),
    "Name"        = map_swig_chr(cl_reacts, "getObjectName"),
    "Reaction"    = map_swig_chr(cl_reacts, "getReactionScheme"),
    "Rate Law"    = cl_reacts %>% map_swig("getFunction") %>% map_swig_chr("getObjectDisplayName"),
    "Flux"        = map_swig_dbl(cl_reacts, "getFlux"),
    "Number Flux" = map_swig_dbl(cl_reacts, "getParticleFlux")
  ) %>%
    transform_names()
}

#' Get reaction references
#'
#' \code{getReactions} returns reactions attribute references as a data frame.
#'
#' @param key a character vector uniquely identifying reactions
#' @param model a model object
#' @return a data frame with reactions and associated references
#' @seealso \code{\link{getReactions}} \code{\link{setReactions}}
#' @family reaction functions
#' @export
getReactionReferences <- function(key = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  if (is_empty(key))
    cl_reacts <- get_cdv(c_datamodel$getModel()$getReactions())
  else
    cl_reacts <- reaction_obj(key, c_datamodel)
  
  # assemble output dataframe
  tibble::tibble(
    key           = map_swig_chr(cl_reacts, "getObjectDisplayName"),
    "Name"        = map_swig_chr(cl_reacts, "getObjectName"),
    "Reaction"    = map_swig_chr(cl_reacts, "getReactionScheme"),
    "Rate Law"    = cl_reacts %>% map_swig("getFunction") %>% map_swig_chr("getObjectDisplayName"),
    "Flux"        = cl_reacts %>% map_swig("getFluxReference") %>% as_ref(c_datamodel),
    "Number Flux" = cl_reacts %>% map_swig("getParticleFluxReference") %>% as_ref(c_datamodel)
  ) %>%
    transform_names()
}

#' Set reactions
#'
#' \code{setReactions} applies given values to reactions of the model depending on the 'key' parameter.
#'
#' @param key a character vector uniquely identifying reactions
#' @param name a character vector of names to set
#' @param data a data frame as given by \code{\link{getReactions}} which will be applied before the other arguments.
#' @param model a model object
#' @seealso \code{\link{getReactions}} \code{\link{getReactionReferencess}}
#' @family reaction functions
#' @export
setReactions <- function(key = NULL, name = NULL, data = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(
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

#' Get valid function names for reaction
#' 
#' @param key reaction key
#' @param model a model object
#' @return vector of function names
#' @seealso \code{\link{setReactionFunction}}
#' @family reaction functions
#' @export
getValidReactionFunctions <- function(key, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(
    is.scalar(key)
  )
  
  c_react <- reaction_obj(key, c_datamodel)[[1]]
  
  c_model <- c_datamodel$getModel()
  c_reacti <- CReactionInterface(c_model)
  c_reacti$initFromReaction(c_react)
  
  # Workaround because the function vector is somehow only given as bare pointer
  # Coerce it to a string vector object
  c_fun_vector <- new(
    "_p_std__vectorT_std__string_std__allocatorT_std__string_t_t",
    ref = c_reacti$getListOfPossibleFunctions()
  )
  
  c_fun_vector %>%
    get_sv() %>%
    kinfunction_strict()
}

#' Set a reaction function
#' 
#' @param key reaction key
#' @param fun string
#' @param mappings list
#' @param model a model object
#' @seealso \code{\link{getValidReactionFunctions}}
#' @family reaction functions
#' @export
setReactionFunction <- function(key, fun, mappings = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(
    is.scalar(key),
    is.scalar(fun)
  )
  
  c_react <- reaction_obj(key, c_datamodel)[[1]]
  c_fun <- kinfunction_obj(fun)[[1]]
  fun <- c_fun$getObjectName()
  
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
    set_react_mapping(c_datamodel, c_reacti, mappings)
  
  c_reacti$writeBackToReaction(c_react)
  
  c_model$compileIfNecessary()
  
  invisible()
}

#' Get reaction parameter mappings
#' 
#' @param key reaction key
#' @param model a model object
#' @return list of parameter mappings
#' @seealso \code{\link{setReactionMappings}}
#' @family reaction functions
#' @export
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

#' Set reaction parameter mappings
#' 
#' @param key reaction key
#' @param mappings list
#' @param model a model object
#' @seealso \code{\link{getReactionMappings}}
#' @family reaction functions
#' @export
setReactionMappings <- function(key, mappings, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(is.scalar(key))
  
  c_react <- reaction_obj(key, c_datamodel)[[1]]
  
  c_model <- c_datamodel$getModel()
  
  c_reacti <- CReactionInterface(c_model)
  c_reacti$initFromReaction(c_react)
  
  set_react_mapping(c_datamodel, c_reacti, mappings)
  
  c_reacti$writeBackToReaction(c_react)
  
  c_model$compileIfNecessary()
  
  invisible()
}

set_react_mapping <- function(c_datamodel, c_reacti, mappings) {
  assert_that(is.list(mappings), !is.null(names(mappings)), noNA(names(mappings)))
  
  params <- seq_along_v(c_reacti)
  names(params) <- map_chr(params, ~ c_reacti$getParameterName(.x))
  
  names(mappings) <- map_chr(names(mappings), function(parameter) rlang::arg_match(parameter, names(params)))
  
  iwalk(mappings, ~ {
    set_rparam_mapping(c_datamodel, c_reacti, i = params[[.y]], value = .x)
  })
  
  assert_that(c_reacti$isValid(), msg = "Result of mapping is invalid.")
}

set_rparam_mapping <- function(c_datamodel, c_reacti, i, value) {
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
  } else if (type == "PARAMETER") {
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
  } else {
    warning("Parameter `", c_reacti$getParameterName(i), "` is of type `", tolower(type), "` and cannot be mapped through ", getPackageName(),". It has been skipped.")
  }
}

#' Get reaction parameters
#'
#' \code{getParameters} returns reaction parameters as a data frame.
#'
#' @param key a character vector uniquely identifying reactions parameters
#' @param model a model object
#' @return a data frame with reaction parameters and associated information
#' @seealso \code{\link{getParameterReferences}} \code{\link{setParameters}}
#' @family reaction functions
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

#' Get reaction parameter references
#'
#' \code{getParameterReferences} returns reaction parameters as a data frame.
#'
#' @param key a character vector uniquely identifying reactions parameters
#' @param model a model object
#' @return a data frame with reaction parameters and associated references
#' @seealso \code{\link{getParameters}} \code{\link{setParameters}}
#' @family reaction functions
#' @export
getParameterReferences <- function(key = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
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
    "Type"     = cl_params %>% map_swig_chr("getType") %>% tolower(),
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
#' @param data a data frame as given by \code{\link{getParameters}} which will be applied before the other arguments.
#' @param model a model object
#' @seealso \code{\link{getParameters}} \code{\link{getParameterReferences}}
#' @family reaction functions
#' @export
setParameters <- function(key = NULL, name = NULL, value = NULL, mapping = NULL, data = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(
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
