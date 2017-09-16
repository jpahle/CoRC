#' Create a new species
#'
#' \code{newSpecies} creates a new species.
#'
#' @param name string
#' @param compartment compartment key
#' @param type string
#' @param initial.concentration number
#' @param expression string
#' @param model a model object
#' @return species key
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
  
  if (!is.null(expression))
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
  
  c_metab <- c_model$createMetabolite(name, c_comp$getObjectName(), initial.concentration, toupper(type))
  
  assert_that(inherits(c_metab, "_p_CMetab"), msg = "Species creation failed.")
  
  tryCatch({
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
  
  c_metab$getObjectDisplayName()
}

#' Delete a species
#' 
#' @param key species keys
#' @param model a model object
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
#' @param name string
#' @param type string
#' @param initial.value number
#' @param expression string
#' @param model a model object
#' @return quantity key
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
  
  if (!is.null(expression))
    expression <- write_expr(expression, c_datamodel)
  
  c_model <- c_datamodel$getModel()
  
  c_quant <- c_model$createModelValue(name, initial.value)
  
  assert_that(inherits(c_quant, "_p_CModelValue"), msg = "Global quantity creation failed.")
  
  c_quant$setStatus(toupper(type))
  
  tryCatch({
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
  
  c_quant$getObjectDisplayName()
}

#' Delete a global quantity
#' 
#' @param key quantity keys
#' @param model a model object
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
#' @param name string
#' @param type string
#' @param initial.volume number
#' @param expression string
#' @param model a model object
#' @export
newCompartment <- function(name, type = c("fixed", "assignment", "ode"), initial.volume = 1, expression = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(
    is.string(name),
    is.number(initial.volume), initial.volume >= 0,
    is.null(expression) || is.string(expression)
  )
  
  # .__E___CModelEntity__Status has other weird entries
  type <- rlang::arg_match(type)
  
  if (!is.null(expression))
    expression <- write_expr(expression, c_datamodel)
  
  c_model <- c_datamodel$getModel()
  
  # type is missing
  c_comp <- c_model$createCompartment(name, initial.volume)
  
  assert_that(inherits(c_comp, "_p_CCompartment"), msg = "Compartment creation failed.")
  
  c_comp$setStatus(toupper(type))
  
  tryCatch({
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
  
  c_comp$getObjectDisplayName()
}

#' Delete a compartment
#' 
#' @param key compartment keys
#' @param model a model object
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
  
  c_model$compileIfNecessary()
  
  dn
}

#' Delete a reaction
#' 
#' @param key reaction keys
#' @param model a model object
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
#' @eval rox_param("parameters", "named character vector", function_role_enum)
#' @param function.type string
#' @return function key
#' @export
newKineticFunction <- function(name, formula, parameters, function.type = c("general", "reversible", "irreversible")) {
  assert_that(
    is.string(name), noNA(name),
    is.string(formula), noNA(formula)
  )
  
  c_fun_db <- CRootContainer_getFunctionList()
  
  assert_that(is.null(c_fun_db$findFunction(name)), msg = paste0("Name ", name, " is already taken."))
  
  parameters <- to_param_vector(parameters, "character")
  
  assert_that(noNA(parameters))
  
  parameters <- tolower(parameters)
  
  parameters <- map_chr(parameters, function(parameters) rlang::arg_match(parameters, function_role_enum))
  
  function.type <- rlang::arg_match(function.type)
  function.type_map <- c(
    general      = "TriUnspecified",
    reversible   = "TriTrue",
    irreversible = "TriFalse"
  )
  function.type <- function.type_map[[function.type]]
  
  c_fun <- avert_gc(CFunction(name))
  tryCatch(
    {
      c_fun$setInfix(formula)
      c_fun$setReversible(function.type)
      c_params <- c_fun$getVariables()
      cl_params <- map_iswig(seq_along_v(c_params), c_params, "getParameter")
      
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
            rep("parameter", length(cl_params)),
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
  
  c_fun$getObjectDisplayName()
}

#' Delete a function
#' 
#' @param key function keys
#' @export
deleteKineticFunction <- function(key) {
  cl_funs <- kinfunction_obj(key)
  
  c_fun_db <- CRootContainer_getFunctionList()
  
  cl_funs <- unique(cl_funs)
  
  any_read_only <- which(map_swig_lgl(cl_funs, "isReadOnly"))[1]
  
  assert_that(
    is.na(any_read_only),
    msg = paste0("Function `", cl_funs[[any_read_only]]$getObjectDisplayName(), " is read-only.")
  )
  
  cl_funs %>%
    map_swig_chr("getKey") %>%
    walk_iswig(c_fun_db, "removeFunction")
  
  invisible()
}


