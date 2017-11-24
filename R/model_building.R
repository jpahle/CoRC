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
  
  grab_msg(c_model$compileIfNecessary())
  
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
  
  grab_msg(c_model$compileIfNecessary())
  
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
  
  grab_msg(c_model$compileIfNecessary())
  
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
  
  grab_msg(c_model$compileIfNecessary())
  
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
  
  grab_msg(c_model$compileIfNecessary())
  
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
  
  grab_msg(c_model$compileIfNecessary())
  
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
  
  grab_msg(c_model$compileIfNecessary())
  
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
  
  grab_msg(c_model$compileIfNecessary())
  
  invisible()
}

#' Create a new event
#' 
#' \code{newEvent} creates a new event.
#' 
#' Default initial value is 1.
#' Arguments priority from lowest to highest is \code{initial_value}, \code{initial_expression}.
#'
#' @param name Name to set, as string.
#' @param trigger_expression Trigger expression to set, as string, finite numeric, or logical.
#' @param fire_at_initial_time Whether to fire at initial time if true, as logical.
#' @param trigger_must_remain_true Whether the trigger must remain true, as logical.
#' @param priority_expression Priority expression to set, as string, finite numeric, or logical.
#' @param delayed Whether the event assignment and / or calculation is to be delayed ("no", "assignment", "calculation"), as string.
#' @param delay_expression Delay expression to set, as string, finite numeric, or logical.
#' @param assignment_target Assignment target entities (species, compartments, global quantities), as character vector.
#' @param assignment_expression Assignment expressions per event to set, as character, finite numeric, or logical vector.
#' @param model a model object
#' @return event key
#' @family event functions
#' @export
newEvent <- function(name, trigger_expression, fire_at_initial_time = FALSE, trigger_must_remain_true = TRUE, priority_expression = NULL, delayed = c("no", "assignment", "calculation"), delay_expression = NULL, assignment_target = NULL, assignment_expression = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  assert_that(
    is.string(name),
    is.cexpression(trigger_expression),
    is.flag(fire_at_initial_time)     && noNA(fire_at_initial_time),
    is.flag(trigger_must_remain_true) && noNA(trigger_must_remain_true),
    is.null(priority_expression)      || is.cexpression(priority_expression)   && noNA(priority_expression),
    is.null(delay_expression)         || is.cexpression(delay_expression)      && noNA(delay_expression),
    is.null(assignment_target)        || is.character(assignment_target)       && noNA(assignment_target),
    is.null(assignment_expression)    || is.cexpression(assignment_expression) && noNA(assignment_expression)
  )
  
  # In case of NULL set empty vector
  assignment_target <- assignment_target %||% character()
  assignment_expression <- assignment_expression %||% character()
  
  assert_that(length(assignment_target) == length(assignment_expression))

  delayed <- rlang::arg_match(delayed)
  
  trigger_expression <- write_expr(to_cexpr(trigger_expression), c_datamodel)
  
  if (!is.null(priority_expression))
    priority_expression <- write_expr(to_cexpr(priority_expression), c_datamodel)
  
  if (!is.null(delay_expression) && delay_expression != "") {
    assert_that(delayed != "no", msg = '`delay_expression` cannot be set when `delayed` == "no"')
    
    delay_expression <- write_expr(to_cexpr(delay_expression), c_datamodel)
  }
  
  # assignment_target
  cl_assignment_targets <- map(assignment_target, dn_to_object, c_datamodel, accepted_types = c("_p_CMetab", "_p_CCompartment", "_p_CModelValue"))
  assert_that(!some(cl_assignment_targets, is.null), msg = "Invalid assignment target given.")
  assignment_target_keys <- map_swig_chr(cl_assignment_targets, "getKey")
  assert_that(
    !anyDuplicated(assignment_target_keys),
    msg = "Assignment targets must be unique."
  )
  
  assignment_expression <- write_expr(to_cexpr(assignment_expression), c_datamodel)

  c_model <- c_datamodel$getModel()

  c_event <- c_model$createEvent(name)

  assert_that(inherits(c_event, "_p_CEvent"), msg = "Event creation failed.")
  
  c_event$setFireAtInitialTime(fire_at_initial_time)
  c_event$setPersistentTrigger(!trigger_must_remain_true)
  c_event$setDelayAssignment(delayed == "calculation")
  cl_assignments <- map(assignment_target_keys, ~ avert_gc(CEventAssignment(.x)))

  tryCatch({
    # if setting an an expression fails, terminate all recently created objects
    tryCatch({
      walk2(cl_assignments, assignment_expression,
        ~ assert_that(grab_msg(.x$setExpression(.y)), msg = "Failed when applying an assignment expression.")
      )
    },
    error = function(e) {
      walk(cl_assignments, delete)
      base::stop(e)
    })
    
    assert_that(
      grab_msg(c_event$setTriggerExpression(trigger_expression)),
      msg = "Failed when applying trigger_expression."
    )
    
    if (!is.null(priority_expression))
      assert_that(
        grab_msg(c_event$setPriorityExpression(priority_expression)),
        msg = "Failed when applying priority_expression."
      )
    
    if (!is.null(delay_expression))
      assert_that(
        grab_msg(c_event$setDelayExpression(delay_expression)),
        msg = "Failed when applying delay_expression."
      )
  },
  error = function(e) {
    c_model$removeEvent(c_event)
    base::stop(e)
  })
  
  c_assignments <- c_event$getAssignments()
  walk(cl_assignments, c_assignments$add)

  compile_and_check(c_model)
  
  get_key(c_event)
}

#' Delete events
#' 
#' \code{deleteEvent} deletes events.
#' 
#' Deletion quietly works recursively (deletes all connected entities) to prevent invalid model states.
#' 
#' @param key event keys
#' @param model a model object
#' @family event functions
#' @export
deleteEvent <- function(key, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  c_model <- c_datamodel$getModel()
  
  # Use copasi keys instead of pointers to prevent crashes
  key %>%
    event_obj(c_datamodel) %>%
    unique() %>%
    map_swig_chr("getKey") %>%
    walk(~ {
      # recursive removal without compilation in between seems to cause crashes.
      c_model$compileIfNecessary()
      c_model$removeEvent(.x, recursive = TRUE)
    })
  
  compile_and_check(c_model)
  
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
