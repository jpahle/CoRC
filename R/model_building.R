#' Create a new species
#'
#' \code{newSpecies} creates a new species.
#' 
#' Default initial concentration is 1.
#' Arguments priority from lowest to highest is \code{initial_concentration}, \code{initial_number}, \code{initial_expression}.
#'
#' @param name string
#' @param compartment compartment key
#' @param type string
#' @param initial_concentration number
#' @param initial_number number
#' @param initial_expression string
#' @param expression string
#' @param model a model object
#' @return species key
#' @family species functions
#' @export
newSpecies <- function(name, compartment = NULL, type = c("reactions", "fixed", "assignment", "ode"), initial_concentration = NULL, initial_number = NULL, initial_expression = NULL, expression = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(
    is.string(name),
    is.null(compartment)           || is.string(compartment),
    is.null(initial_concentration) || is.number(initial_concentration),
    is.null(initial_number)        || is.number(initial_number),
    is.null(initial_expression)    || is.string(initial_expression) && noNA(initial_expression) && initial_expression != "",
    is.null(expression)            || is.string(expression)         && noNA(expression)         && expression != ""
  )
  
  # .__E___CModelEntity__Status has other weird entries
  type <- rlang::arg_match(type)
  
  if (!is.null(initial_expression))
    initial_expression <- write_expr(initial_expression, c_datamodel)
  
  if (!is.null(expression))
    expression <- write_expr(expression, c_datamodel)
  
  c_model <- c_datamodel$getModel()
  
  if (is.null(compartment)) {
    c_comps <- c_model$getCompartments()
    comps_size <- c_comps$size()
    
    if (c_comps$size() == 0L) {
      c_model$createCompartment("compartment")
      warning("No compartment exists. Created default compartment.")
    }
      
    # Even if multiple compartments exist, use the first as default
    c_comp <- c_comps$get(0L)
    
    if (c_comps$size() > 1L)
      warning("No compartment given, using default: ", c_comp$getObjectName())
  } else {
    c_comp <- compartment_obj(compartment, c_datamodel)[[1]]
  }
  
  c_metab <- c_model$createMetabolite(name, c_comp$getObjectName(), initial_concentration %||% 1, toupper(type))
  
  assert_that(inherits(c_metab, "_p_CMetab"), msg = "Species creation failed.")
  
  if (!is.null(initial_number))
    c_metab$setInitialValue(initial_number)
  
  tryCatch({
    if (!is.null(initial_expression)) {
      assert_that(
        grab_msg(c_metab$setInitialExpression(initial_expression)$isSuccess()),
        msg = "Species creation failed when applying the initial expression."
      )
    }
    
    if (!is.null(expression)) {
      assert_that(
        grab_msg(c_metab$setExpression(expression)$isSuccess()),
        msg = "Species creation failed when applying the expression."
      )
    }
  },
  error = function(e) {
    c_model$removeMetabolite(c_metab)
    base::stop(e)
  })
  
  c_model$compileIfNecessary()
  
  get_key(c_metab, is_species = TRUE)
}

#' Delete a species
#' 
#' @param key species keys
#' @param model a model object
#' @family species functions
#' @export
deleteSpecies <- function(key, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  cl_metabs <- species_obj(key, c_datamodel)
  
  c_model <- c_datamodel$getModel()
  
  cl_metabs %>%
    unique() %>%
    walk(~ c_model$removeMetabolite(.x))
  
  c_model$compileIfNecessary()
  
  invisible()
}

#' Create a new global quantity
#' 
#' \code{newGlobalQuantity} creates a new global quantity.
#' 
#' Default initial value is 1.
#' Arguments priority from lowest to highest is \code{initial_value}, \code{initial_expression}.
#'
#' @param name string
#' @param type string
#' @param initial_value number
#' @param initial_expression string
#' @param expression string
#' @param model a model object
#' @return quantity key
#' @family global quantity functions
#' @export
newGlobalQuantity <- function(name, type = c("fixed", "assignment", "ode"), initial_value = NULL, initial_expression = NULL, expression = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(
    is.string(name),
    is.null(initial_value)      || is.number(initial_value),
    is.null(initial_expression) || is.string(initial_expression) && noNA(initial_expression) && initial_expression != "",
    is.null(expression)         || is.string(expression)         && noNA(expression)         && expression != ""
  )
  
  # .__E___CModelEntity__Status has other weird entries
  type <- rlang::arg_match(type)
  
  if (!is.null(initial_expression))
    initial_expression <- write_expr(initial_expression, c_datamodel)
  
  if (!is.null(expression))
    expression <- write_expr(expression, c_datamodel)
  
  c_model <- c_datamodel$getModel()
  
  c_quant <- c_model$createModelValue(name, initial_value %||% 1)
  
  assert_that(inherits(c_quant, "_p_CModelValue"), msg = "Global quantity creation failed.")
  
  c_quant$setStatus(toupper(type))
  
  if (!is.null(initial_value))
    c_quant$setInitialValue(initial_value)
  
  tryCatch({
    if (!is.null(initial_expression)) {
      assert_that(
        grab_msg(c_quant$setInitialExpression(initial_expression)$isSuccess()),
        msg = "Global quantity failed when applying the initial expression."
      )
    }
    
    if (!is.null(expression)) {
      assert_that(
        grab_msg(c_quant$setExpression(expression)$isSuccess()),
        msg = "Global quantity creation failed when applying the expression."
      )
    }
  },
  error = function(e) {
    c_model$removeModelValue(c_quant)
    base::stop(e)
  })
  
  c_model$compileIfNecessary()
  
  get_key(c_quant)
}

#' Delete a global quantity
#' 
#' @param key quantity keys
#' @param model a model object
#' @family global quantity functions
#' @export
deleteGlobalQuantity <- function(key, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  cl_quants <- quantity_obj(key, c_datamodel)
  
  c_model <- c_datamodel$getModel()
  
  cl_quants %>%
    unique() %>%
    walk(~ c_model$removeModelValue(.x))
  
  c_model$compileIfNecessary()
  
  invisible()
}

#' Create a new compartment
#' 
#' \code{newCompartment} creates a new compartment.
#' 
#' Default initial size is 1.
#' Arguments priority from lowest to highest is \code{initial_size}, \code{initial_expression}.
#'
#' @param name string
#' @param type string
#' @param initial_size number
#' @param initial_expression string
#' @param expression string
#' @param model a model object
#' @family compartment functions
#' @export
newCompartment <- function(name, type = c("fixed", "assignment", "ode"), initial_size = NULL, initial_expression= NULL, expression = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(
    is.string(name),
    is.null(initial_size)       || is.number(initial_size),
    is.null(initial_expression) || is.string(initial_expression) && noNA(initial_expression) && initial_expression != "",
    is.null(expression)         || is.string(expression)         && noNA(expression)         && expression != ""
  )
  
  # .__E___CModelEntity__Status has other weird entries
  type <- rlang::arg_match(type)
  
  if (!is.null(initial_expression))
    initial_expression <- write_expr(initial_expression, c_datamodel)
  
  if (!is.null(expression))
    expression <- write_expr(expression, c_datamodel)
  
  c_model <- c_datamodel$getModel()
  
  # type is missing
  c_comp <- c_model$createCompartment(name, initial_size %||% 1)
  
  assert_that(inherits(c_comp, "_p_CCompartment"), msg = "Compartment creation failed.")
  
  c_comp$setStatus(toupper(type))
  
  tryCatch({
    if (!is.null(initial_expression)) {
      assert_that(
        grab_msg(c_comp$setInitialExpression(initial_expression)$isSuccess()),
        msg = "Compartment creation failed when applying the expression."
      )
    }
    
    if (!is.null(expression)) {
      assert_that(
        grab_msg(c_comp$setExpression(expression)$isSuccess()),
        msg = "Compartment creation failed when applying the expression."
      )
    }
  },
  error = function(e) {
    c_model$removeCompartment(c_comp)
    base::stop(e)
  })
  
  c_model$compileIfNecessary()
  
  get_key(c_comp)
}

#' Delete a compartment
#' 
#' @param key compartment keys
#' @param model a model object
#' @family compartment functions
#' @export
deleteCompartment <- function(key, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  cl_comps <- compartment_obj(key, c_datamodel)
  
  c_model <- c_datamodel$getModel()
  
  cl_comps %>%
    unique() %>%
    walk(~ c_model$removeCompartment(.x))
  
  c_model$compileIfNecessary()
  
  invisible()
}

#' Create a new reaction
#'
#' @param scheme string
#' @param name string
#' @param fun string
#' @param mappings named list
#' @param model a model object
#' @return reaction key
#' @family reaction functions
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
  
  assert_that(
    grab_msg(c_react$setReactionScheme(scheme)),
    msg = "Reaction scheme invalid"
  )
  
  dn <- get_key(c_react)
  
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
  
  c_model$compileIfNecessary()
  
  dn
}

#' Delete a reaction
#' 
#' @param key reaction keys
#' @param model a model object
#' @family reaction functions
#' @export
deleteReaction <- function(key, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  cl_reacts <- reaction_obj(key, c_datamodel)
  
  c_model <- c_datamodel$getModel()
  
  cl_reacts %>%
    unique() %>%
    walk(~ c_model$removeReaction(.x))
  
  c_model$compileIfNecessary()
  
  invisible()
}

#' @include swig_wrapper.R
function_role_enum <-
  names(.__E___CFunctionParameter__Role) %>%
  .[. != "VARIABLE" & . != "TEMPORARY"] %>%
  tolower()

#' Create a new kinetic function
#' 
#' @param name string
#' @param formula string
#' @eval paste0("@param parameters named character vector
#' 
#' Allowed values: ", rox_print_v(function_role_enum), ".")
#' @param function_type string
#' @return function key
#' @family reaction functions
#' @export
newKineticFunction <- function(name, formula, parameters, function_type = c("general", "reversible", "irreversible")) {
  assert_that(
    is.string(name), noNA(name),
    is.string(formula), noNA(formula)
  )
  
  c_fun_db <- CRootContainer_getFunctionList()
  
  assert_that(is.null(c_fun_db$findFunction(name)), msg = paste0("Name `", name, "` is already taken."))
  
  parameters <- to_param_vector(parameters, "character")
  
  assert_that(noNA(parameters))
  
  parameters <- tolower(parameters)
  
  parameters <- map_chr(parameters, function(parameters) rlang::arg_match(parameters, function_role_enum))
  
  function_type <- rlang::arg_match(function_type)
  function_type_map <- c(
    general      = "TriUnspecified",
    reversible   = "TriTrue",
    irreversible = "TriFalse"
  )
  function_type <- function_type_map[[function_type]]
  
  c_fun <- avert_gc(CFunction(name))
  tryCatch(
    {
      c_fun$setInfix(formula)
      c_fun$setReversible(function_type)
      c_params <- c_fun$getVariables()
      cl_params <- map(seq_along_v(c_params), c_params$getParameter)
      
      param_names <- cl_params %>% map_swig_chr("getObjectName")
      
      if (is.null(names(parameters))) {
        assert_that(
          length(parameters) == length(cl_params),
          msg = paste0("Unnamed parameters must be of length `", length(cl_params), "` for the given formula.")
        )
      } else {
        i <- match(names(parameters), param_names)
        
        assert_that(
          noNA(i),
          msg = "Invalid parameter names."
        )
        
        # Fill all missing names with "parameter"
        parameters <-
          replace(
            rep_along(cl_params, "parameter"),
            i,
            parameters
          )
      }
      
      parameters <- toupper(parameters)
      walk2(cl_params, parameters, ~ .x$setUsage(.y))
    },
    error = function(e) {
      delete(c_fun)
      base::stop(e)
    }
  )
  
  c_fun_db$addAndAdopt(c_fun)
  
  get_key(c_fun)
}

#' Delete a function
#' 
#' @param key function keys
#' @family reaction functions
#' @export
deleteKineticFunction <- function(key) {
  cl_funs <- kinfunction_obj(key)
  
  c_fun_db <- CRootContainer_getFunctionList()
  
  cl_funs <- unique(cl_funs)
  
  any_read_only <- which(map_swig_lgl(cl_funs, "isReadOnly"))[1]
  
  assert_that(
    is.na(any_read_only),
    msg = paste0("Function `", get_key(cl_funs[[any_read_only]]), " is read-only.")
  )
  
  cl_funs %>%
    map_swig_chr("getKey") %>%
    walk(c_fun_db$removeFunction)
  
  invisible()
}

#' Clear custom functions
#' 
#' @family reaction functions
#' @export
clearCustomKineticFunctions <- function() {
  c_fun_db <- CRootContainer_getFunctionList()
  
  cl_funs <-
    c_fun_db$loadedFunctions() %>%
    get_cdv() %>%
    discard(map_swig_lgl(., "isReadOnly")) %>%
    map_swig_chr("getKey") %>%
    walk(c_fun_db$removeFunction)
  
  invisible()
}
