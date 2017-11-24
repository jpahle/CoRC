#' Get species
#'
#' \code{getSpecies} returns species information as a data frame.
#'
#' @param key Optionally, a character vector specifying which species to get.
#' @param raw_expressions Whether expressions should be raw (not converted to readable format), as flag.
#' @param model A model object.
#' @return Species and associated information, as data frame.
#' @seealso \code{\link{getSpeciesReferences}} \code{\link{setSpecies}}
#' @family species functions
#' @export
getSpecies <- function(key = NULL, raw_expressions = FALSE, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(is.flag(raw_expressions), !is.na(raw_expressions))
  
  if (is_empty(key))
    cl_metabs <- get_cdv(c_datamodel$getModel()$getMetabolites())
  else
    cl_metabs <- species_obj(key, c_datamodel)
  
  types <- map_swig_chr(cl_metabs, "getStatus")
  has_init_expression <- types != "ASSIGNMENT"
  has_expression <- !has_init_expression | (types == "ODE")
  
  initial_expressions <- rep_along(cl_metabs, NA_character_)
  initial_expressions[has_init_expression] <-
    map_chr(cl_metabs[has_init_expression], iexpr_to_str, c_datamodel = c_datamodel, raw = raw_expressions)
  
  expressions <- rep_along(cl_metabs, NA_character_)
  expressions[has_expression] <-
    map_chr(cl_metabs[has_expression], expr_to_str, c_datamodel = c_datamodel, raw = raw_expressions)
  
  # assemble output dataframe
  tibble::tibble(
    key                     = get_key(cl_metabs, is_species = TRUE),
    "Name"                  = map_swig_chr(cl_metabs, "getObjectName"),
    "Compartment"           = cl_metabs %>% map_swig("getCompartment") %>% map_swig_chr("getObjectName"),
    "Type"                  = tolower(types),
    "Initial Concentration" = map_swig_dbl(cl_metabs, "getInitialConcentration"),
    "Initial Number"        = map_swig_dbl(cl_metabs, "getInitialValue"),
    "Concentration"         = map_swig_dbl(cl_metabs, "getConcentration"),
    "Number"                = map_swig_dbl(cl_metabs, "getValue"),
    "Rate"                  = map_swig_dbl(cl_metabs, "getConcentrationRate"),
    "Number Rate"           = map_swig_dbl(cl_metabs, "getRate"),
    "Initial Expression"    = initial_expressions,
    "Expression"            = expressions
  ) %>%
    transform_names()
}

#' Get species references
#'
#' \code{getSpeciesReferences} returns species attribute references as a data frame.
#'
#' @param key Optionally, a character vector specifying which species to get.
#' @param model A model object.
#' @return Species and associated references, as data frame.
#' @seealso \code{\link{getSpecies}} \code{\link{setSpecies}}
#' @family species functions
#' @export
getSpeciesReferences <- function(key = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  if (is_empty(key))
    cl_metabs <- get_cdv(c_datamodel$getModel()$getMetabolites())
  else
    cl_metabs <- species_obj(key, c_datamodel)
  
  types <- map_swig_chr(cl_metabs, "getStatus")
  has_init_expression <- types != "ASSIGNMENT"
  has_expression <- !has_init_expression | (types == "ODE")
  
  initial_expressions <- rep_along(cl_metabs, NA_character_)
  initial_expressions[has_init_expression] <-
    map_chr(cl_metabs[has_init_expression], iexpr_to_ref_str, c_datamodel = c_datamodel)
  
  expressions <- rep_along(cl_metabs, NA_character_)
  expressions[has_expression] <-
    map_chr(cl_metabs[has_expression], expr_to_ref_str, c_datamodel = c_datamodel)
  
  # assemble output dataframe
  tibble::tibble(
    key                     = get_key(cl_metabs, is_species = TRUE),
    "Name"                  = map_swig_chr(cl_metabs, "getObjectName"),
    "Compartment"           = cl_metabs %>% map_swig("getCompartment") %>% map_swig_chr("getObjectName"),
    "Type"                  = tolower(types),
    "Initial Concentration" = cl_metabs %>% map_swig("getInitialConcentrationReference") %>% as_ref(c_datamodel),
    "Initial Number"        = cl_metabs %>% map_swig("getInitialValueReference") %>% as_ref(c_datamodel),
    "Concentration"         = cl_metabs %>% map_swig("getConcentrationReference") %>% as_ref(c_datamodel),
    "Number"                = cl_metabs %>% map_swig("getValueReference") %>% as_ref(c_datamodel),
    "Rate"                  = cl_metabs %>% map_swig("getConcentrationRateReference") %>% as_ref(c_datamodel),
    "Number Rate"           = cl_metabs %>% map_swig("getRateReference") %>% as_ref(c_datamodel),
    "Initial Expression"    = initial_expressions,
    "Expression"            = expressions
  ) %>%
    transform_names()
}

#' Set species
#'
#' \code{setSpecies} applies given values to species of the model depending on the \code{key} argument.
#' 
#' Use the \code{key} argument to specify which species to modify and any of the other arguments to specify the value to set.
#' The function is fully vectorized.
#' If a \code{NA} value is supplied, the model value is kept unchanged.
#'
#' @param key Identify which species to edit by specifying it's key, as string.
#' Also supports fragments of keys, if uniquely matching one species.
#' @param name Name to set, as string.
#' @param compartment Key of new compartment to set, as string.
#' @param type Type ("fixed", "assignment", "reactions", "ode") to set, as string.
#' @param initial_concentration Initial concentration to set, as numeric.
#' @param initial_number Initial particle number to set, as numeric.
#' @param initial_expression Initial expression to set, as string, finite numeric, or logical.
#' @param expression Expression to set, as string, finite numeric, or logical.
#' @param data A data frame as given by \code{\link{getSpecies}} which will be applied before the other arguments.
#' @param model A model object.
#' @seealso \code{\link{getSpecies}} \code{\link{getSpeciesReferences}}
#' @family species functions
#' @export
setSpecies <- function(key = NULL, name = NULL, compartment = NULL, type = NULL, initial_concentration = NULL, initial_number = NULL, initial_expression = NULL, expression = NULL, data = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(
    is.null(name)                  || is.character(name)                 && length(name) == length(key),
    is.null(type)                  || is.character(type)                 && length(type) == length(key),
    is.null(compartment)           || is.character(compartment)          && length(compartment)  == length(key),
    is.null(initial_concentration) || is.numeric(initial_concentration)  && length(initial_concentration) == length(key),
    is.null(initial_number)        || is.numeric(initial_number)         && length(initial_number) == length(key),
    is.null(initial_expression)    || is.cexpression(initial_expression) && length(initial_expression) == length(key),
    is.null(expression)            || is.cexpression(expression)         && length(expression) == length(key),
    is.null(data)                  || is.data.frame(data)
  )
  
  # Do this as assertion before we start changing values
  cl_metabs <- species_obj(key %||% character(), c_datamodel)
  
  # gather vectors of what to actually do work on
  false_vec <- rep_along(cl_metabs, FALSE)
  do_name                  <- if (is.null(name))                  false_vec else !is.na(name)
  do_type                  <- if (is.null(type))                  false_vec else !is.na(type)
  do_compartment           <- if (is.null(compartment))           false_vec else !is.na(compartment)
  do_initial_concentration <- if (is.null(initial_concentration)) false_vec else !is.na(initial_concentration)
  do_initial_number        <- if (is.null(initial_number))        false_vec else !is.na(initial_number)
  do_initial_expression    <- if (is.null(initial_expression))    false_vec else !is.na(initial_expression)
  do_expression            <- if (is.null(expression))            false_vec else !is.na(expression)

  # cut pointless actions
  do_initial_concentration <- do_initial_concentration & !do_initial_number     & !do_initial_expression
  do_initial_number        <- do_initial_number        & !do_initial_expression
  
  # assemble compartments
  if (any(do_compartment)) {
    cl_comps_new <- list_along(cl_metabs)
    cl_comps_new[do_compartment] <- compartment_obj(compartment[do_compartment], c_datamodel)
  }
  
  if (any(do_type))
    type <-
      type %>%
      map_chr(function(type) rlang::arg_match(type, c(NA_character_, "fixed", "assignment", "reactions", "ode"))) %>%
      toupper()
  
  if (any(do_initial_expression)) {
    initial_expression[do_initial_expression] <-
      initial_expression[do_initial_expression] %>%
      to_cexpr() %>%
      write_expr(c_datamodel)
  }
  
  if (any(do_expression)) {
    expression[do_expression] <-
      expression[do_expression] %>%
      to_cexpr() %>%
      write_expr(c_datamodel)
  }
  
  # if data is provided with the data arg, run a recursive call
  # needs to be kept up to date with the function args
  if (!is.null(data))
    do.call(setSpecies, data[names(data) %in% c("key", "name", "type", "initial_concentration", "initial_number", "initial_expression", "expression")])
  
  if (is_empty(cl_metabs))
    return(invisible())
  
  c_model <- c_datamodel$getModel()
  
  # apply names
  for (i in which(do_name))
    cl_metabs[[i]]$setObjectName(name[i])
  
  # apply compartments
  if (any(do_compartment)) {
    comp_old_key <-
      cl_metabs[do_compartment] %>%
      map_swig("getCompartment") %>%
      map_swig_chr("getKey")
    
    comp_new_key <-
      cl_comps_new[do_compartment] %>%
      map_swig_chr("getKey")
    
    comp_changed <- rep_along(cl_metabs, FALSE)
    comp_changed[do_compartment] <- comp_old_key != comp_new_key
    
    if (any(comp_changed)) {
      for (i in which(comp_changed))
        assert_that(
          grab_msg(cl_comps_new[[i]]$addMetabolite(cl_metabs[[i]])),
          msg = "Failed to move species."
        )
        # remove from old compartment by name (by pointer is not exported by swig.)
        # looks like there is some method that will remove the metab when adding it to another comp.
        # cl_comps_old[[i]]$getMetabolites()$removeByName(cl_metabs[[i]]$getObjectName())
      
      c_model$initializeMetabolites()
      c_model$setCompileFlag()
    }
  }
  
  # apply types
  for (i in which(do_type))
    cl_metabs[[i]]$setStatus(type[i])
  
  # apply initial concentrations
  if (any(do_initial_concentration)) {
    # TODO
    # this is a hacky solution
    # clear initial expression because they are in the way
    walk_swig(cl_metabs[do_initial_concentration], "setInitialExpression", "")
    c_model$updateInitialValues("Concentration")
    
    for (i in which(do_initial_concentration))
      cl_metabs[[i]]$setInitialConcentration(initial_concentration[i])
    
    c_model$updateInitialValues("Concentration")
  }
  
  # apply initial particlenum
  if (any(do_initial_number)) {
    # TODO
    # this is a hacky solution
    # clear initial expression because they are in the way
    walk_swig(cl_metabs[do_initial_number], "setInitialExpression", "")
    c_model$updateInitialValues("ParticleNumbers")
    
    for (i in which(do_initial_number))
      cl_metabs[[i]]$setInitialValue(initial_number[i])
    
    c_model$updateInitialValues("ParticleNumbers")
  }
  
  # apply initial expressions
  for (i in which(do_initial_expression))
    assert_that(
      grab_msg(cl_metabs[[i]]$setInitialExpression(initial_expression[i])$isSuccess()),
      msg = "Failed when applying an initial expression."
    )
  
  # apply expressions
  for (i in which(do_expression))
    assert_that(
      grab_msg(cl_metabs[[i]]$setExpression(expression[i])$isSuccess()),
      msg = "Failed when applying an expression."
    )
  
  compile_and_check(c_model)
  
  invisible()
}

#' Get global quantities
#'
#' \code{getGlobalQuantities} returns global quantities as a data frame.
#'
#' @param key Optionally, a character vector specifying which global quantities to get.
#' @param raw_expressions Whether expressions should be raw (not converted to readable format), as flag.
#' @param model A model object.
#' @return Global quantities and associated information, as data frame.
#' @seealso \code{\link{getGlobalQuantityReferences}} \code{\link{setGlobalQuantities}}
#' @family global quantity functions
#' @export
getGlobalQuantities <- function(key = NULL, raw_expressions = FALSE, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(is.flag(raw_expressions), !is.na(raw_expressions))
  
  if (is_empty(key))
    cl_quants <- get_cdv(c_datamodel$getModel()$getModelValues())
  else
    cl_quants <- quantity_obj(key, c_datamodel)
  
  types <- map_swig_chr(cl_quants, "getStatus")
  has_init_expression <- types != "ASSIGNMENT"
  has_expression <- !has_init_expression | (types == "ODE")
  
  initial_expressions <- rep_along(cl_quants, NA_character_)
  initial_expressions[has_init_expression] <-
    map_chr(cl_quants[has_init_expression], iexpr_to_str, c_datamodel = c_datamodel, raw = raw_expressions)
  
  expressions <- rep_along(cl_quants, NA_character_)
  expressions[has_expression] <-
    map_chr(cl_quants[has_expression], expr_to_str, c_datamodel = c_datamodel, raw = raw_expressions)
  
  # assemble output dataframe
  tibble::tibble(
    key                  = get_key(cl_quants),
    "Name"               = map_swig_chr(cl_quants, "getObjectName"),
    "Type"               = tolower(types),
    "Initial Value"      = map_swig_dbl(cl_quants, "getInitialValue"),
    "Value"              = map_swig_dbl(cl_quants, "getValue"),
    "Rate"               = map_swig_dbl(cl_quants, "getRate"),
    "Initial Expression" = initial_expressions,
    "Expression"         = expressions
  ) %>%
    transform_names()
}

#' Get global quantitiy references
#'
#' \code{getGlobalQuantityReferences} returns global quantity attribute references as a data frame.
#'
#' @param key Optionally, a character vector specifying which global quantities to get.
#' @param model A model object.
#' @return Global quantities and associated references, as data frame.
#' @seealso \code{\link{getGlobalQuantities}} \code{\link{setGlobalQuantities}}
#' @family global quantity functions
#' @export
getGlobalQuantityReferences <- function(key = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  if (is_empty(key))
    cl_quants <- get_cdv(c_datamodel$getModel()$getModelValues())
  else
    cl_quants <- quantity_obj(key, c_datamodel)
  
  types <- map_swig_chr(cl_quants, "getStatus")
  has_init_expression <- types != "ASSIGNMENT"
  has_expression <- !has_init_expression | (types == "ODE")
  
  initial_expressions <- rep_along(cl_quants, NA_character_)
  initial_expressions[has_init_expression] <-
    map_chr(cl_quants[has_init_expression], iexpr_to_ref_str, c_datamodel = c_datamodel)
  
  expressions <- rep_along(cl_quants, NA_character_)
  expressions[has_expression] <-
    map_chr(cl_quants[has_expression], expr_to_ref_str, c_datamodel = c_datamodel)
  
  # assemble output dataframe
  tibble::tibble(
    key                  = get_key(cl_quants),
    "Name"               = map_swig_chr(cl_quants, "getObjectName"),
    "Type"               = tolower(types),
    "Initial Value"      = cl_quants %>% map_swig("getInitialValueReference") %>% as_ref(c_datamodel),
    "Value"              = cl_quants %>% map_swig("getValueReference") %>% as_ref(c_datamodel),
    "Rate"               = cl_quants %>% map_swig("getRateReference") %>% as_ref(c_datamodel),
    "Initial Expression" = initial_expressions,
    "Expression"         = expressions
  ) %>%
    transform_names()
}

#' Set global quantities
#'
#' \code{setGlobalQuantities} applies given values to global quantities of the model depending on the \code{key} argument.
#'
#' Use the \code{key} argument to specify which global quantity to modify and any of the other arguments to specify the value to set.
#' The function is fully vectorized.
#' If a \code{NA} value is supplied, the model value is kept unchanged.
#' 
#' @param key Identify which global quantity to edit by specifying it's key, as string.
#' Also supports fragments of keys, if uniquely matching one global quantity.
#' @param name Name to set, as string.
#' @param type Type ("fixed", "assignment", "ode") to set, as string.
#' @param initial_value Initial value to set, as numeric.
#' @param initial_expression Initial expression to set, as string, finite numeric, or logical.
#' @param expression Expression to set, as string, finite numeric, or logical.
#' @param data A data frame as given by \code{\link{getGlobalQuantities}} which will be applied before the other arguments.
#' @param model A model object.
#' @seealso \code{\link{getGlobalQuantities}} \code{\link{getGlobalQuantityReferences}}
#' @family global quantity functions
#' @export
setGlobalQuantities <- function(key = NULL, name = NULL, type = NULL, initial_value = NULL, initial_expression = NULL, expression = NULL, data = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(
    is.null(name)               || is.character(name)                 && length(name) == length(key),
    is.null(type)               || is.character(type)                 && length(type) == length(key),
    is.null(initial_value)      || is.numeric(initial_value)          && length(initial_value) == length(key),
    is.null(initial_expression) || is.cexpression(initial_expression) && length(initial_expression) == length(key),
    is.null(expression)         || is.cexpression(expression)         && length(expression) == length(key),
    is.null(data)               || is.data.frame(data)
  )
  
  # Do this as assertion before we start changing values
  cl_quants <- quantity_obj(key %||% character(), c_datamodel)
  
  # gather vectors of what to actually do work on
  false_vec <- rep_along(cl_quants, FALSE)
  do_name               <- if (is.null(name))               false_vec else !is.na(name)
  do_type               <- if (is.null(type))               false_vec else !is.na(type)
  do_initial_value      <- if (is.null(initial_value))      false_vec else !is.na(initial_value)
  do_initial_expression <- if (is.null(initial_expression)) false_vec else !is.na(initial_expression)
  do_expression         <- if (is.null(expression))         false_vec else !is.na(expression)
  
  # cut pointless actions
  do_initial_value <- do_initial_value & !do_initial_expression
  
  if (any(do_type))
    type <-
      type %>%
      map_chr(function(type) rlang::arg_match(type, c(NA_character_, "fixed", "assignment", "ode"))) %>%
      toupper()
  
  if (any(do_initial_expression)) {
    initial_expression[do_initial_expression] <-
      initial_expression[do_initial_expression] %>%
      to_cexpr() %>%
      write_expr(c_datamodel)
  }
  
  if (any(do_expression)) {
    expression[do_expression] <-
      expression[do_expression] %>%
      to_cexpr() %>%
      write_expr(c_datamodel)
  }
  
  # if data is provided with the data arg, run a recursive call
  # needs to be kept up to date with the function args
  if (!is.null(data))
    do.call(setGlobalQuantities, data[names(data) %in% c("key", "name", "type", "initial_value", "initial_expression", "expression")])
  
  if (is_empty(cl_quants))
    return(invisible())
  
  c_model <- c_datamodel$getModel()
  
  # apply names
  for (i in which(do_name))
    cl_quants[[i]]$setObjectName(name[i])
  
  # apply types
  for (i in which(do_type))
    cl_quants[[i]]$setStatus(type[i])
  
  # apply initial value
  if (any(do_initial_value)) {
    # TODO
    # this is a hacky solution
    # clear initial expression because they are in the way
    walk_swig(cl_quants[do_initial_value], "setInitialExpression", "")
    
    for (i in which(do_initial_value))
      cl_quants[[i]]$setInitialValue(initial_value[i])
    
    c_model$updateInitialValues("ParticleNumbers")
  }
  
  # apply initial expressions
  for (i in which(do_initial_expression))
    assert_that(
      grab_msg(cl_quants[[i]]$setInitialExpression(initial_expression[i])$isSuccess()),
      msg = "Failed when applying an initial expression."
    )
  
  # apply expressions
  for (i in which(do_expression))
    assert_that(
      grab_msg(cl_quants[[i]]$setExpression(expression[i])$isSuccess()),
      msg = "Failed when applying an expression."
    )
  
  compile_and_check(c_model)
  
  invisible()
}

#' Get compartments
#'
#' \code{getCompartments} returns compartments as a data frame.
#'
#' @param key Optionally, a character vector specifying which compartments to get.
#' @param raw_expressions Whether expressions should be raw (not converted to readable format), as flag.
#' @param model A model object.
#' @return Compartments and associated information, as data frame.
#' @seealso \code{\link{getCompartmentReferences}} \code{\link{setCompartments}}
#' @family compartment functions
#' @export
getCompartments <- function(key = NULL, raw_expressions = FALSE, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(is.flag(raw_expressions), !is.na(raw_expressions))
  
  if (is_empty(key))
    cl_comps <- get_cdv(c_datamodel$getModel()$getCompartments())
  else
    cl_comps <- compartment_obj(key, c_datamodel)
  
  types <- map_swig_chr(cl_comps, "getStatus")
  has_init_expression <- types != "ASSIGNMENT"
  has_expression <- !has_init_expression | (types == "ODE")
  
  initial_expressions <- rep_along(cl_comps, NA_character_)
  initial_expressions[has_init_expression] <-
    map_chr(cl_comps[has_init_expression], iexpr_to_str, c_datamodel = c_datamodel, raw = raw_expressions)
  
  expressions <- rep_along(cl_comps, NA_character_)
  expressions[has_expression] <-
    map_chr(cl_comps[has_expression], expr_to_str, c_datamodel = c_datamodel, raw = raw_expressions)
  
  # assemble output dataframe
  tibble::tibble(
    key                  = get_key(cl_comps),
    "Name"               = map_swig_chr(cl_comps, "getObjectName"),
    "Type"               = tolower(types),
    "Initial Size"       = map_swig_dbl(cl_comps, "getInitialValue"),
    "Size"               = map_swig_dbl(cl_comps, "getValue"),
    "Rate"               = map_swig_dbl(cl_comps, "getRate"),
    "Initial Expression" = initial_expressions,
    "Expression"         = expressions
  ) %>%
    transform_names()
}

#' Get compartment references
#'
#' \code{getCompartmentReferences} returns compartment attribute references as a data frame.
#'
#' @param key Optionally, a character vector specifying which compartments to get.
#' @param model A model object.
#' @return Compartments and associated references, as data frame.
#' @seealso \code{\link{getCompartments}} \code{\link{setCompartments}}
#' @family compartment functions
#' @export
getCompartmentReferences <- function(key = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  if (is_empty(key))
    cl_comps <- get_cdv(c_datamodel$getModel()$getCompartments())
  else
    cl_comps <- compartment_obj(key, c_datamodel)
  
  types <- map_swig_chr(cl_comps, "getStatus")
  has_init_expression <- types != "ASSIGNMENT"
  has_expression <- !has_init_expression | (types == "ODE")
  
  initial_expressions <- rep_along(cl_comps, NA_character_)
  initial_expressions[has_init_expression] <-
    map_chr(cl_comps[has_init_expression], iexpr_to_ref_str, c_datamodel = c_datamodel)
  
  expressions <- rep_along(cl_comps, NA_character_)
  expressions[has_expression] <-
    map_chr(cl_comps[has_expression], expr_to_ref_str, c_datamodel = c_datamodel)
  
  # assemble output dataframe
  tibble::tibble(
    key                  = get_key(cl_comps),
    "Name"               = map_swig_chr(cl_comps, "getObjectName"),
    "Type"               = tolower(types),
    "Initial Size"       = cl_comps %>% map_swig("getInitialValueReference") %>% as_ref(c_datamodel),
    "Size"               = cl_comps %>% map_swig("getValueReference") %>% as_ref(c_datamodel),
    "Rate"               = cl_comps %>% map_swig("getRateReference") %>% as_ref(c_datamodel),
    "Initial Expression" = initial_expressions,
    "Expression"         = expressions
  ) %>%
    transform_names()
}

#' Set compartments
#'
#' \code{setCompartments} applies given values to compartments of the model depending on the \code{key} argument.
#'
#' Use the \code{key} argument to specify which compartment to modify and any of the other arguments to specify the value to set.
#' The function is fully vectorized.
#' If a \code{NA} value is supplied, the model value is kept unchanged.
#' 
#' @param key Identify which compartment to edit by specifying it's key, as string.
#' Also supports fragments of keys, if uniquely matching one compartment.
#' @param name Name to set, as string.
#' @param type Type ("fixed", "assignment", "ode") to set, as string.
#' @param initial_size Initial size to set, as string.
#' @param initial_expression Initial expression to set, as string, finite numeric, or logical.
#' @param expression Expression to set, as string, finite numeric, or logical.
#' @param data A data frame as given by \code{\link{getCompartments}} which will be applied before the other arguments.
#' @param model A model object.
#' @seealso \code{\link{getCompartments}} \code{\link{getCompartmentReferences}}
#' @family compartment functions
#' @export
setCompartments <- function(key = NULL, name = NULL, type = NULL, initial_size = NULL, initial_expression = NULL, expression = NULL, data = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(
    is.null(name)               || is.character(name)                 && length(name) == length(key),
    is.null(type)               || is.character(type)                 && length(type) == length(key),
    is.null(initial_size)       || is.numeric(initial_size)           && length(initial_size) == length(key),
    is.null(initial_expression) || is.cexpression(initial_expression) && length(initial_expression) == length(key),
    is.null(expression)         || is.cexpression(expression)         && length(expression) == length(key),
    is.null(data)               || is.data.frame(data)
  )
  
  # Do this as assertion before we start changing values
  cl_comps <- compartment_obj(key %||% character(), c_datamodel)
  
  # gather vectors of what to actually do work on
  false_vec <- rep_along(cl_comps, FALSE)
  do_name               <- if (is.null(name))               false_vec else !is.na(name)
  do_type               <- if (is.null(type))               false_vec else !is.na(type)
  do_initial_size       <- if (is.null(initial_size))       false_vec else !is.na(initial_size)
  do_initial_expression <- if (is.null(initial_expression)) false_vec else !is.na(initial_expression)
  do_expression         <- if (is.null(expression))         false_vec else !is.na(expression)
  
  # cut pointless actions
  do_initial_size <- do_initial_size & !do_initial_expression
  
  if (any(do_type))
    type <-
      type %>%
      map_chr(function(type) rlang::arg_match(type, c(NA_character_, "fixed", "assignment", "ode"))) %>%
      toupper()
  
  if (any(do_initial_expression)) {
    initial_expression[do_initial_expression] <-
      initial_expression[do_initial_expression] %>%
      to_cexpr() %>%
      write_expr(c_datamodel)
  }
  
  if (any(do_expression)) {
    expression[do_expression] <-
      expression[do_expression] %>%
      to_cexpr() %>%
      write_expr(c_datamodel)
  }
  
  # if data is provided with the data arg, run a recursive call
  # needs to be kept up to date with the function args
  if (!is.null(data))
    do.call(setCompartments, data[names(data) %in% c("key", "name", "type", "initial_size", "initial_expression", "expression")])
  
  if (is_empty(cl_comps))
    return(invisible())
  
  c_model <- c_datamodel$getModel()
  
  # apply names
  for (i in which(do_name))
    cl_comps[[i]]$setObjectName(name[i])
  
  # apply types
  for (i in which(do_type))
    cl_comps[[i]]$setStatus(type[i])
  
  # apply initial size
  if (any(do_initial_size)) {
    # TODO
    # this is a hacky solution
    # clear initial expression because they are in the way
    walk_swig(cl_comps[do_initial_size], "setInitialExpression", "")
    
    for (i in which(do_initial_size))
      cl_comps[[i]]$setInitialValue(initial_size[i])
    
    c_model$updateInitialValues("ParticleNumbers")
  }
  
  # apply initial expressions
  for (i in which(do_initial_expression))
    assert_that(
      grab_msg(cl_comps[[i]]$setInitialExpression(initial_expression[i])$isSuccess()),
      msg = "Failed when applying an initial expression."
    )
  
  # apply expressions
  for (i in which(do_expression))
    assert_that(
      grab_msg(cl_comps[[i]]$setExpression(expression[i])$isSuccess()),
      msg = "Failed when applying an expression."
    )
  
  compile_and_check(c_model)
  
  invisible()
}

#' Get reactions
#'
#' \code{getReactions} returns reactions as a data frame.
#'
#' @param key Optionally, a character vector specifying which reactions to get.
#' @param model A model object.
#' @return Reactions and associated information, as data frame.
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
    key           = get_key(cl_reacts),
    "Name"        = map_swig_chr(cl_reacts, "getObjectName"),
    "Reaction"    = map_swig_chr(cl_reacts, "getReactionScheme"),
    "Rate Law"    = cl_reacts %>% map_swig("getFunction") %>% get_key(),
    "Flux"        = map_swig_dbl(cl_reacts, "getFlux"),
    "Number Flux" = map_swig_dbl(cl_reacts, "getParticleFlux")
  ) %>%
    transform_names()
}

#' Get reaction references
#'
#' \code{getReactions} returns reactions attribute references as a data frame.
#'
#' @param key Optionally, a character vector specifying which reactions to get.
#' @param model A model object.
#' @return Reactions and associated references, as data frame.
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
    key           = get_key(cl_reacts),
    "Name"        = map_swig_chr(cl_reacts, "getObjectName"),
    "Reaction"    = map_swig_chr(cl_reacts, "getReactionScheme"),
    "Rate Law"    = cl_reacts %>% map_swig("getFunction") %>% get_key(),
    "Flux"        = cl_reacts %>% map_swig("getFluxReference") %>% as_ref(c_datamodel),
    "Number Flux" = cl_reacts %>% map_swig("getParticleFluxReference") %>% as_ref(c_datamodel)
  ) %>%
    transform_names()
}

#' Set reactions
#'
#' \code{setReactions} applies given values to reactions of the model depending on the \code{key} argument.
#'
#' Use the \code{key} argument to specify which reaction to modify and any of the other arguments to specify the value to set.
#' The function is fully vectorized.
#' If a \code{NA} value is supplied, the model value is kept unchanged.
#' 
#' @param key Identify which reaction to edit by specifying it's key, as string.
#' Also supports fragments of keys, if uniquely matching one reaction.
#' @param name Name to set, as string.
#' @param data A data frame as given by \code{\link{getReactions}} which will be applied before the other arguments.
#' @param model A model object.
#' @seealso \code{\link{getReactions}} \code{\link{getReactionReferences}}
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
  
  # gather vectors of what to actually do work on
  false_vec <- rep_along(cl_reacts, FALSE)
  do_name <- if (is.null(name)) false_vec else !is.na(name)

  # if data is provided with the data arg, run a recursive call
  # needs to be kept up to date with the function args
  if (!is.null(data))
    do.call(setReactions, data[names(data) %in% c("key", "name")])
  
  if (is_empty(cl_reacts))
    return(invisible())
  
  # apply names
  for (i in which(do_name))
    cl_reacts[[i]]$setObjectName(name[i])
  
  invisible()
}

#' Get valid function names for reaction
#' 
#' @param key Identify which reaction to read by specifying it's key, as string.
#' Also supports fragments of keys, if uniquely matching one reaction.
#' @param model A model object.
#' @return Function names, as character vector.
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
#' @param key Identify which reaction to edit by specifying it's key, as string.
#' Also supports fragments of keys, if uniquely matching one reaction.
#' @param fun Key of new kinetic function to set, as string.
#' Also supports fragments of keys, if uniquely matching one kinetic function.
#' @param mappings New parameter mappings, as named list.
#' @param model A model object.
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
  
  compile_and_check(c_model)
  
  invisible()
}

#' Get reaction parameter mappings
#' 
#' @param key Identify which reaction to read by specifying it's key, as string.
#' Also supports fragments of keys, if uniquely matching one reaction.
#' @param model A model object.
#' @return Reaction parameter mappings, as list.
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
#' @param key Identify which reaction to edit by specifying it's key, as string.
#' Also supports fragments of keys, if uniquely matching one reaction.
#' @param mappings New parameter mappings, as named list.
#' @param model A model object.
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
  
  compile_and_check(c_model)
  
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
      get_key(c_comp) %in% valid_vals,
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
        get_key(c_quant) %in% valid_vals,
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
#' @param key Optionally, a character vector specifying which reaction parameters to get.
#' @param model A model object.
#' @return Reaction parameters and associated information, as data frame.
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
  
  values <- rep_along(cl_params, NA_real_)
  values[are_local] <-
    cl_params[are_local] %>%
    map_swig_dbl("getDblValue")
  
  mappings <- rep_along(cl_params, NA_character_)
  mappings[!are_local] <- 
    map2_chr(names[!are_local], cl_reacts[!are_local],
      function(name, c_react) {
        val <- get_sv(c_react$getParameterMapping(name))
        
        # For now don't support multiple mappings
        if (length(val) > 1)
          return("<MULTIPLE>")
  
        get_key(c_keyfactory$get(val))
      }
    )
  
  # assemble output dataframe
  tibble::tibble(
    key        = get_key(cl_params),
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
#' @param key Optionally, a character vector specifying which reaction parameters to get.
#' @param model A model object.
#' @return Reaction parameters and associated references, as data frame.
#' @seealso \code{\link{getParameters}} \code{\link{setParameters}}
#' @family reaction functions
#' @export
getParameterReferences <- function(key = NULL, model = getCurrentModel()) {
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
  
  value_refs <- rep_along(cl_params, NA_character_)
  value_refs[!are_local] <-
    cl_params[!are_local] %>%
    map_swig("getValueReference") %>%
    as_ref(c_datamodel)

  mappings <- rep_along(cl_params, NA_character_)
  mappings[!are_local] <- 
    map2_chr(names[!are_local], cl_reacts[!are_local],
      function(name, c_react) {
        val <- get_sv(c_react$getParameterMapping(name))

        # For now don't support multiple mappings
        if (length(val) > 1)
          return("<MULTIPLE>")

        get_key(c_keyfactory$get(val))
      }
    )
  
  # assemble output dataframe
  tibble::tibble(
    key        = get_key(cl_params),
    "Name"     = names,
    "Reaction" = cl_reacts %>% map_swig_chr("getObjectName"),
    "Value"    = value_refs,
    "Mapping"  = mappings
  ) %>%
    transform_names()
}

#' Set reaction parameters
#'
#' \code{setParameters} applies given values to reaction parameters of the model depending on the \code{key} argument.
#'
#' Use the \code{key} argument to specify which reaction parameter to modify and any of the other arguments to specify the value to set.
#' The function is fully vectorized.
#' If a \code{NA} value is supplied, the model value is kept unchanged.
#' 
#' @param key Identify which reaction parameter to edit by specifying it's key, as string.
#' Also supports fragments of keys, if uniquely matching one reaction parameter.
#' @param name Name to set, as string.
#' @param value Value to set, as numeric.
#' @param mapping Key of global quantity to map to, as string.
#' Also supports fragments of keys, if uniquely matching one global quantity.
#' @param data A data frame as given by \code{\link{getParameters}} which will be applied before the other arguments.
#' @param model A model object.
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
  
  # gather vectors of what to actually do work on
  false_vec <- rep_along(cl_params, FALSE)
  do_name    <- if (is.null(name))    false_vec else !is.na(name)
  do_value   <- if (is.null(value))   false_vec else !is.na(value)
  do_mapping <- if (is.null(mapping)) false_vec else !is.na(mapping)
  
  # cut pointless actions
  do_value <- do_value & !do_mapping
  do_param <- do_value | do_mapping

  # Do this as assertion before we start changing values
  # Makes sure mapping is either NA or a copasi key
  if (any(do_mapping))
    mapping[do_mapping] <-
      mapping[do_mapping] %>%
      quantity_obj(c_datamodel) %>%
      map_swig_chr("getKey")
  
  # if data is provided with the data arg, run a recursive call
  # needs to be kept up to date with the function args
  if (!is.null(data))
    do.call(setParameters, data[names(data) %in% c("key", "name", "value", "mapping")])
  
  if (is_empty(cl_params))
    return(invisible())
  
  c_model <- c_datamodel$getModel()
  
  # apply names
  for (i in which(do_name))
    cl_params[[i]]$setObjectName(name[i])
  
  # Parameters are only those of type PARAMETER I think.
  # So I can safely set a value and make them local or set a mapping
  # as long as that mapping is a global quantity.
  # Changing the parameters directly seems to be unsafe.
  # The safe method seems to be to go back to the reaction and do manipulations from there.
  
  # apply values
  if (any(do_param)) {
    cl_reacts <- list_along(cl_params)
    cl_reacts[do_param] <-
      cl_params[do_param] %>%
      map_swig("getObjectAncestor", "Reaction") %>%
      map(as, "_p_CReaction")
    
    names <- rep_along(cl_params, NA_character_)
    names[do_param] <- map_swig_chr(cl_params[do_param], "getObjectName")
    
    for (i in which(do_value))
      cl_reacts[[i]]$setParameterValue(names[i], value[i])
    
    for (i in which(do_mapping))
      cl_reacts[[i]]$setParameterMapping(names[i], mapping[i])
    
    cl_reacts[do_param] %>%
      unique() %>%
      walk_swig("compile")
    
    c_model$setCompileFlag()
  }
  
  compile_and_check(c_model)
  
  invisible()
}

#' Get events
#'
#' \code{getEvents} returns events as a data frame.
#'
#' @param key Optionally, a character vector specifying which events to get.
#' @param raw_expressions Whether expressions should be raw (not converted to readable format), as flag.
#' @param model A model object.
#' @return Events and associated information, as data frame.
#' \itemize{
#'   \item \code{$assignment_target} is a list column containing possibly several targets per event.
#'   \item \code{$assignment_expression} is a list column containing possibly several expressions per event.
#' }
#' @seealso \code{\link{setEvents}}
#' @family event functions
#' @export
getEvents <- function(key = NULL, raw_expressions = FALSE, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(is.flag(raw_expressions), !anyNA(raw_expressions))
  
  if (is_empty(key))
    cl_events <- get_cdv(c_datamodel$getModel()$getEvents())
  else
    cl_events <- event_obj(key, c_datamodel)
  
  cl_assignments <-
    cl_events %>%
    map_swig("getAssignments") %>%
    map(get_cdv)

  trigger_expressions    <- map_swig_chr(cl_events, "getTriggerExpression")
  priority_expressions   <- map_swig_chr(cl_events, "getPriorityExpression")
  delay_expressions      <- map_swig_chr(cl_events, "getDelayExpression")
  assignment_expressions <- map(cl_assignments , ~ map_swig_chr(.x, "getExpression"))
  
  if (!raw_expressions) {
    trigger_expressions    <- read_expr(trigger_expressions, c_datamodel)
    priority_expressions   <- read_expr(priority_expressions, c_datamodel)
    delay_expressions      <- read_expr(delay_expressions, c_datamodel)
    assignment_expressions <- map(assignment_expressions, read_expr, c_datamodel)
  }
  
  trigger_expressions    <- emptychr_to_na(trigger_expressions)
  # priority_expressions   <- emptychr_to_na(priority_expressions)
  delay_expressions      <- emptychr_to_na(delay_expressions)
  assignment_expressions <- map(assignment_expressions, map_chr, emptychr_to_na)

  has_delay <- !is.na(delay_expressions)
  has_delay_calc <- map_swig_int(cl_events, "getDelayAssignment")
  delayed <- rep_along(cl_events, "no")
  delayed[has_delay & !has_delay_calc] <- "assignment"
  delayed[has_delay & has_delay_calc] <- "calculation"
  
  targets <-
    cl_assignments %>%
    map(~
      .x %>%
      map_swig_chr("getTargetKey") %>%
      cop_key_to_obj() %>%
      get_key()
    )
  
  # assemble output dataframe
  tibble::tibble(
    key                        = get_key(cl_events),
    "Name"                     = map_swig_chr(cl_events, "getObjectName"),
    "Trigger Expression"       = trigger_expressions,
    "Fire at initial time"     = as.logical(map_swig_int(cl_events, "getFireAtInitialTime")),
    "Trigger must remain true" = !as.logical(map_swig_int(cl_events, "getPersistentTrigger")),
    "Priority Expression"      = priority_expressions,
    "Delayed"                  = delayed,
    "Delay Expression"         = delay_expressions,
    "Assignment Target"        = targets,
    "Assignment Expression"    = assignment_expressions
  ) %>%
    transform_names()
}

#' Set events
#'
#' \code{setEvents} applies given values to events of the model depending on the \code{key} argument.
#'
#' Use the \code{key} argument to specify which event to modify and any of the other arguments to specify the value to set.
#' The function is fully vectorized.
#' If a \code{NA} value is supplied, the model value is kept unchanged.
#' 
#' @param key Identify which event to edit by specifying it's key, as string.
#' Also supports fragments of keys, if uniquely matching one event.
#' @param name Name to set, as string.
#' @param trigger_expression Trigger expression to set, as string, finite numeric, or logical.
#' @param fire_at_initial_time Whether to fire at initial time if true, as logical.
#' @param trigger_must_remain_true Whether the trigger must remain true, as logical.
#' @param priority_expression Priority expression to set, as string, finite numeric, or logical.
#' @param delayed Whether the event assignment and / or calculation is to be delayed ("no", "assignment", "calculation"), as string.
#' @param delay_expression Delay expression to set, as string, finite numeric, or logical.
#' @param assignment_target List of assignment target entities (species, compartments, global quantities) per event to set, as list containing strings.
#' @param assignment_expression List of assignment expressions per event to set, as list containing string, finite numeric, or logical.
#' @param data A data frame as given by \code{\link{getEvents}} which will be applied before the other arguments.
#' @param model A model object.
#' @seealso \code{\link{getEvents}}
#' @family event functions
#' @export
setEvents <- function(key = NULL, name = NULL, trigger_expression = NULL, fire_at_initial_time = NULL, trigger_must_remain_true = NULL, priority_expression = NULL, delayed = NULL, delay_expression = NULL, assignment_target = NULL, assignment_expression = NULL, data = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(
    is.null(name)                     || is.character(name)                   && length(name) == length(key),
    is.null(trigger_expression)       || is.cexpression(trigger_expression)   && length(trigger_expression) == length(key),
    is.null(fire_at_initial_time)     || is.logical(fire_at_initial_time)     && length(fire_at_initial_time) == length(key),
    is.null(trigger_must_remain_true) || is.logical(trigger_must_remain_true) && length(trigger_must_remain_true) == length(key),
    is.null(priority_expression)      || is.cexpression(priority_expression)  && length(priority_expression) == length(key),
    is.null(delayed)                  || is.character(delayed)                && length(delayed) == length(key),
    is.null(delay_expression)         || is.cexpression(delay_expression)     && length(delay_expression) == length(key),
    is.null(assignment_target)        || is.list(assignment_target)           && length(assignment_target) == length(key),
    is.null(assignment_expression)    || is.list(assignment_expression)       && length(assignment_expression) == length(key),
    is.null(data)                     || is.data.frame(data)
  )

  # Do this as assertion before we start changing values
  cl_events <- event_obj(key %||% character(), c_datamodel)

  # gather vectors of what to actually do work on
  false_vec <- rep_along(cl_events, FALSE)
  do_name                     <- if (is.null(name))                     false_vec else !is.na(name)
  do_trigger_expression       <- if (is.null(trigger_expression))       false_vec else !is.na(trigger_expression)
  do_fire_at_initial_time     <- if (is.null(fire_at_initial_time))     false_vec else !is.na(fire_at_initial_time)
  do_trigger_must_remain_true <- if (is.null(trigger_must_remain_true)) false_vec else !is.na(trigger_must_remain_true)
  do_priority_expression      <- if (is.null(priority_expression))      false_vec else !is.na(priority_expression)
  do_delayed                  <- if (is.null(delayed))                  false_vec else !is.na(delayed)
  
  if (any(do_delayed)) {
    delayed <-
      delayed %>%
      map_chr(function(delayed) rlang::arg_match(delayed, c(NA_character_, "no", "assignment", "calculation")))
    
    # if delayed is set to no we overwrite the expression and set delayed to default (assignment)
    nodelay <- delayed == "no"
    if (any(nodelay)) {
      if (is.null(delay_expression))
        delay_expression <- rep_along(cl_events, NA_character_)
      delay_expression[nodelay] <- ""
      delayed[nodelay] <- "assignment"
    }
  }
  
  do_delay_expression      <- if (is.null(delay_expression))     false_vec else !is.na(delay_expression)
  do_assignment_target     <- if (is.null(assignment_target))    false_vec else !is.na(assignment_target)
  do_assignment_expression <- if (is.null(assignment_expression)) false_vec else !is.na(assignment_expression)
  
  # the way you can set event assignments with this function
  # is a bit unwieldy but lets at least allow it if the data is perfectly structured.
  assert_that(
    identical(do_assignment_target, do_assignment_expression),
    msg = "Assignment targets and assignment expressions must always be set in pairs."
  )
  assert_that(
    identical(lengths(assignment_target[do_assignment_target]), lengths(assignment_expression[do_assignment_expression])),
    msg = "Argument `assignment_target` and `assignment_expression` must have identical structure."
  )
  assert_that(
    every(assignment_target[do_assignment_target], is.character),
    msg = "Argument `assignment_target` must be a list containing character vectors only."
  )
  assert_that(
    every(assignment_target[do_assignment_target], is.cexpression),
    msg = "Argument `assignment_expression` must be a list containing copasi expressions only."
  )
  assert_that(
    every(assignment_target[do_assignment_target], noNA),
    every(assignment_expression[do_assignment_expression], noNA),
    msg = "Assignment targets and assignment expressions must not contain <NA> in value pairs."
  )

  if (any(do_trigger_expression)) {
    trigger_expression[do_trigger_expression] <-
      trigger_expression[do_trigger_expression] %>%
      to_cexpr() %>%
      write_expr(c_datamodel)
  }
  
  if (any(do_priority_expression)) {
    priority_expression[do_priority_expression] <-
      priority_expression[do_priority_expression] %>%
      to_cexpr() %>%
      write_expr(c_datamodel)
  }
  
  if (any(do_delay_expression)) {
    delay_expression[do_delay_expression] <-
      delay_expression[do_delay_expression] %>%
      to_cexpr() %>%
      write_expr(c_datamodel)
  }
  
  if (any(do_assignment_target)) {
    assignment_target[do_assignment_target] <-
      assignment_target[do_assignment_target] %>%
      map(~ {
        cl_obj <- map(.x, dn_to_object, c_datamodel, accepted_types = c("_p_CMetab", "_p_CCompartment", "_p_CModelValue"))
        assert_that(!some(cl_obj, is.null), msg = "Invalid assignment target given.")
        map_swig_chr(cl_obj, "getKey")
      })
    assert_that(
      assignment_target[do_assignment_target] %>% map_lgl(~ anyDuplicated(.x) == 0) %>% all(),
      msg = "Assignment targets must be unique per event."
    )
    
    assignment_expression[do_assignment_target] <-
      assignment_expression[do_assignment_target] %>%
      map(to_cexpr) %>%
      map(write_expr, c_datamodel)
  }
  
  # if data is provided with the data arg, run a recursive call
  # needs to be kept up to date with the function args
  if (!is.null(data))
    do.call(setEvents, data[names(data) %in% c("key", "name", "trigger_expression", "fire_at_initial_time", "trigger_must_remain_true", "priority_expression", "delayed", "delay_expression", "assignment_target", "assignment_expression")])

  if (is_empty(cl_events))
    return(invisible())

  c_model <- c_datamodel$getModel()
  
  # apply names
  for (i in which(do_name))
    cl_events[[i]]$setObjectName(name[i])

  # apply trigger expressions
  for (i in which(do_trigger_expression))
    assert_that(
      grab_msg(cl_events[[i]]$setTriggerExpression(trigger_expression[i])),
      msg = "Failed when applying a trigger expression."
    )
  
  # apply fire_at_initial_time
  for (i in which(do_fire_at_initial_time))
    cl_events[[i]]$setFireAtInitialTime(fire_at_initial_time[i])
  
  # apply trigger_must_remain_true
  for (i in which(do_trigger_must_remain_true))
    cl_events[[i]]$setPersistentTrigger(!trigger_must_remain_true[i])
  
  # apply priority expression
  for (i in which(do_priority_expression))
    assert_that(
      grab_msg(cl_events[[i]]$setPriorityExpression(priority_expression[i])),
      msg = "Failed when applying a priority expression."
    )
  
  # apply delayed
  if (any(do_delayed)) {
    val <- rep_along(delayed, NA)
    val[delayed == "assignment"] <- FALSE
    val[delayed == "calculation"] <- TRUE
    for (i in which(do_delayed))
      cl_events[[i]]$setDelayAssignment(val[i])
  }
  
  # apply delay expression
  for (i in which(do_delay_expression))
    assert_that(
      grab_msg(cl_events[[i]]$setDelayExpression(delay_expression[i])),
      msg = "Failed when applying a delay expression."
    )
  
  # apply assignment_target
  for (i in which(do_assignment_target)) {
    c_event <- cl_events[[i]]
    c_assignments <- c_event$getAssignments()
    
    # create all new assignments
    cl_assignments_new <- map(assignment_target[[i]], ~ avert_gc(CEventAssignment(.x)))
    # if setting an an expression fails, terminate all recently created objects
    tryCatch({
      walk2(cl_assignments_new, assignment_expression[[i]],
        ~ assert_that(grab_msg(.x$setExpression(.y)), msg = "Failed when applying an assignment expression.")
      )
    },
    error = function(e) {
      walk(cl_assignments_new, delete)
      base::stop(e)
    })
    
    # delete old assignments, add new ones
    c_assignments$clear()
    # the add method should have argument adopt = TRUE
    # argument is missing
    # it seems like it does adopt by default though
    walk(cl_assignments_new, c_assignments$add)
  }
  
  compile_and_check(c_model)
  
  invisible()
}
