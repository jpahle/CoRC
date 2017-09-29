#' Entity finders
#' 
#' The entity finder family of functions are a set of helpers to
#' get unique identifiers for the various entities defined in copasi.
#' They give a quick overview of available species, compartments etc..
#'
#' There are two classes of entity finders.
#' 
#' The main functions are flexible and return a vector of all entities matching a given name fragment.
#' 
#' The `_strict` varieties expect for all input keys to uniquely match one entity and throw errors otherwise.
#' This ensures that there are always as many identifiers returned as are given to the functions.
#' This mechanism is also used by CoRC internally to ensure that functions like \code{\link{getSpecies}}
#' return one row in the output for each key given as parameter.
#' 
#' Whereas the main functions generally won't help seperate an entity `a` from and entity `ab`, the
#' `_strict` varieties will accept a key "a" because it fully matches the entity `a`, which gets peference
#' over the partial match with `ab`.
#' 
#' The matching mechanism can be tuned using a \code{\link{regex}} mechanism.
#' 
#' The functions can also be used to find value references by giving the `reference` argument.
#' Various references such as "Value" or "InitialConcentration" are available for some types of entities.
#' 
#' @name entity_finders
#' @param key entity name framents
#' 
#'   * main varieties: a string.
#'   
#'   * `_strict` varieties: a character vector with each entry uniquely matching one entity.
#' @param reference an optional string naming the value references to be returned.
#' @param model a model object.
#' @return A character vector of species identifiers or references.
#'
#'   References are in the form of "\{name\}" or in rare cases in the form of "<CN=...>" and can
#'   be used in expressions or functions such as \code{\link{getValue}}.
NULL

#' @rdname entity_finders
#' @family species functions
#' @export
species <- function(key = "", reference = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  assert_that(
    is.string(key), !is.na(key),
    is.null(reference) || is.string(reference)
  )
  
  cl_metabs <- get_cdv(c_datamodel$getModel()$getMetabolites())
  
  if (key == "") {
    cl_matches <- cl_metabs
  } else {
    matches <- stringr::str_which(
      cl_metabs %>% map_swig_chr("getObjectDisplayName"),
      apply_eng(key)
    )
    cl_matches <- cl_metabs[matches]
  }
  
  if (is.null(reference))
    map_swig_chr(cl_matches, "getObjectDisplayName")
  else
    as_ref(apply_ref(cl_matches, reference), c_datamodel)
}

#' @rdname entity_finders
#' @family species functions
#' @export
species_strict <- function(key, reference = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  cl_metabs <- species_obj(key, c_datamodel, reference)
  
  if (is.null(reference))
    map_swig_chr(cl_metabs, "getObjectDisplayName")
  else
    as_ref(cl_metabs, c_datamodel)
}

species_obj <- function(key, c_datamodel, reference = NULL) {
  assert_that(
    is.character(key), noNA(key), !("" %in% key),
    is.null(reference) || is.string(reference) || is.character(reference) && length(key) == length(reference)
  )
  
  # If names are already DN to metabolites we accept them (disabled for regex)
  if (!inherits(key, "regex"))
    matches <- map(key, dn_to_object, c_datamodel, "_p_CMetab")
  else
    matches <- vector("list", length(key))
  
  matched <- lengths(matches) == 1L
  
  if (!all(matched)) {
    info <- "species"
    cl_metabs <- get_cdv(c_datamodel$getModel()$getMetabolites())
    dns <- cl_metabs %>% map_swig_chr("getObjectDisplayName")
    # keys are needed as list, else attributes are lost on subsetting
    key_l <- seq_along(key) %>% map(subset_eng, x = apply_eng(key))
    
    # find full matches to ObjectDisplayName
    # str_replace as hack to find complete matches
    matches[!matched] <- map(key_l[!matched], ~ which(stringr::str_replace(dns, .x, "") == ""))
    matched <- lengths(matches) == 1L
    
    if (!all(matched)) {
      ns <- cl_metabs %>% map_swig_chr("getObjectName")
      # find full matches to ObjectName
      matches[!matched] <- map(key_l[!matched], ~ which(stringr::str_replace(ns, .x, "") == ""))
      matched <- lengths(matches) == 1L
      
      if (!all(matched)) {
        # then partial matches to ObjectDisplayName
        matches[!matched] <- map(key_l[!matched], stringr::str_which, string = dns)
        assert_matches(matches, key, dns, info)
      
        matched <- lengths(matches) == 1L
      
        assert_that(all(matched), msg = paste0(
          "Couldn't match ", info, ' "',
          key[!matched], '".',
          collapse = '", "'
        ))
      }
    }
    
    # the matches list contains integers and objects
    # integers signal matches and have to be converted to objects before returning
    matches <-
      matches %>%
      map_if(
        map_lgl(., is_scalar_integer),
        ~ cl_metabs[[.x]]
      )
  }
  
  if (is.null(reference))
    matches
  else
    apply_ref(matches, reference)
}

#' @rdname entity_finders
#' @family global quantity functions
#' @export
quantity <- function(key = "", reference = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  assert_that(
    is.string(key), !is.na(key),
    is.null(reference) || is.string(reference)
  )
  
  cl_quants <- get_cdv(c_datamodel$getModel()$getModelValues())
  
  if (key == "") {
    cl_matches <- cl_quants
  } else {
    matches <- stringr::str_which(
      cl_quants %>% map_swig_chr("getObjectDisplayName"),
      apply_eng(key)
    )
    cl_matches <- cl_quants[matches]
  }
  
  if (is.null(reference))
    map_swig_chr(cl_matches, "getObjectDisplayName")
  else
    as_ref(apply_ref(cl_matches, reference), c_datamodel)
}

#' @rdname entity_finders
#' @family global quantity functions
#' @export
quantity_strict <- function(key, reference = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  cl_quants <- quantity_obj(key, c_datamodel, reference)
  
  if (is.null(reference))
    map_swig_chr(cl_quants, "getObjectDisplayName")
  else
    as_ref(cl_quants, c_datamodel)
}

quantity_obj <- function(key, c_datamodel, reference = NULL) {
  assert_that(
    is.character(key), noNA(key), !("" %in% key),
    is.null(reference) || is.string(reference) || is.character(reference) && length(key) == length(reference)
  )
  
  # If names are already DN to metabolites we accept them (disabled for regex)
  if (!inherits(key, "regex"))
    matches <- map(key, dn_to_object, c_datamodel, "_p_CModelValue")
  else
    matches <- vector("list", length(key))
  
  matched <- lengths(matches) == 1L
  
  if (!all(matched)) {
    info <- "global quantity(s)"
    cl_quants <- get_cdv(c_datamodel$getModel()$getModelValues())
    dns <- cl_quants %>% map_swig_chr("getObjectDisplayName")
    # keys are needed as list, else attributes are lost on subsetting
    key_l <- seq_along(key) %>% map(subset_eng, x = apply_eng(key))
    
    # find full matches to ObjectDisplayName
    # str_replace as hack to find complete matches
    matches[!matched] <- map(key_l[!matched], ~ which(stringr::str_replace(dns, .x, "") == ""))
    matched <- lengths(matches) == 1L
    
    if (!all(matched)) {
      ns <- cl_quants %>% map_swig_chr("getObjectName")
      # find full matches to ObjectName
      matches[!matched] <- map(key_l[!matched], ~ which(stringr::str_replace(ns, .x, "") == ""))
      matched <- lengths(matches) == 1L
      
      if (!all(matched)) {
        # then partial matches to ObjectDisplayName
        matches[!matched] <- map(key_l[!matched], stringr::str_which, string = dns)
        assert_matches(matches, key, dns, info)
        
        matched <- lengths(matches) == 1L
        
        assert_that(all(matched), msg = paste0(
          "Couldn't match ", info, ' "',
          key[!matched], '".',
          collapse = '", "'
        ))
      }
    }
    
    # the matches list contains integers and objects
    # integers signal matches and have to be converted to objects before returning
    matches <-
      matches %>%
      map_if(
        map_lgl(., is_scalar_integer),
        ~ cl_quants[[.x]]
      )
  }
  
  if (is.null(reference))
    matches
  else
    apply_ref(matches, reference)
}

#' @rdname entity_finders
#' @family compartment functions
#' @export
compartment <- function(key = "", reference = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  assert_that(
    is.string(key), !is.na(key),
    is.null(reference) || is.string(reference)
  )
  
  cl_comps <- get_cdv(c_datamodel$getModel()$getCompartments())
  
  if (key == "") {
    cl_matches <- cl_comps
  } else {
    matches <- stringr::str_which(
      cl_comps %>% map_swig_chr("getObjectDisplayName"),
      apply_eng(key)
    )
    cl_matches <- cl_comps[matches]
  }
  
  if (is.null(reference))
    map_swig_chr(cl_matches, "getObjectDisplayName")
  else
    as_ref(apply_ref(cl_matches, reference), c_datamodel)
}

#' @rdname entity_finders
#' @family compartment functions
#' @export
compartment_strict <- function(key, reference = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  cl_comps <- compartment_obj(key, c_datamodel, reference)
  
  if (is.null(reference))
    map_swig_chr(cl_comps, "getObjectDisplayName")
  else
    as_ref(cl_comps, c_datamodel)
}

compartment_obj <- function(key, c_datamodel, reference = NULL) {
  assert_that(
    is.character(key), noNA(key), !("" %in% key),
    is.null(reference) || is.string(reference) || is.character(reference) && length(key) == length(reference)
  )
  
  # If names are already DN to metabolites we accept them (disabled for regex)
  if (!inherits(key, "regex"))
    matches <- map(key, dn_to_object, c_datamodel, "_p_CCompartment")
  else
    matches <- vector("list", length(key))
  
  matched <- lengths(matches) == 1L
  
  if (!all(matched)) {
    info <- "compartment(s)"
    cl_comps <- get_cdv(c_datamodel$getModel()$getCompartments())
    dns <- cl_comps %>% map_swig_chr("getObjectDisplayName")
    # keys are needed as list, else attributes are lost on subsetting
    key_l <- seq_along(key) %>% map(subset_eng, x = apply_eng(key))
    
    # find full matches to ObjectDisplayName
    # str_replace as hack to find complete matches
    matches[!matched] <- map(key_l[!matched], ~ which(stringr::str_replace(dns, .x, "") == ""))
    matched <- lengths(matches) == 1L
    
    if (!all(matched)) {
      ns <- cl_comps %>% map_swig_chr("getObjectName")
      # find full matches to ObjectName
      matches[!matched] <- map(key_l[!matched], ~ which(stringr::str_replace(ns, .x, "") == ""))
      matched <- lengths(matches) == 1L
      
      if (!all(matched)) {
        # then partial matches to ObjectDisplayName
        matches[!matched] <- map(key_l[!matched], stringr::str_which, string = dns)
        assert_matches(matches, key, dns, info)
        
        matched <- lengths(matches) == 1L
        
        assert_that(all(matched), msg = paste0(
          "Couldn't match ", info, ' "',
          key[!matched], '".',
          collapse = '", "'
        ))
      }
    }
    
    # the matches list contains integers and objects
    # integers signal matches and have to be converted to objects before returning
    matches <-
      matches %>%
      map_if(
        map_lgl(., is_scalar_integer),
        ~ cl_comps[[.x]]
      )
  }
  
  if (is.null(reference))
    matches
  else
    apply_ref(matches, reference)
}

#' @rdname entity_finders
#' @family reaction functions
#' @export
reaction <- function(key = "", reference = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  assert_that(
    is.string(key), !is.na(key),
    is.null(reference) || is.string(reference)
  )
  
  cl_reacts <- get_cdv(c_datamodel$getModel()$getReactions())
  
  if (key == "") {
    cl_matches <- cl_reacts
  } else {
    matches <- stringr::str_which(
      cl_reacts %>% map_swig_chr("getObjectDisplayName"),
      apply_eng(key)
    )
    cl_matches <- cl_reacts[matches]
  }
  
  if (is.null(reference))
    map_swig_chr(cl_matches, "getObjectDisplayName")
  else
    as_ref(apply_ref(cl_matches, reference), c_datamodel)
}

#' @rdname entity_finders
#' @family reaction functions
#' @export
reaction_strict <- function(key, reference = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  cl_reacts <- reaction_obj(key, c_datamodel, reference)
  
  if (is.null(reference))
    map_swig_chr(cl_reacts, "getObjectDisplayName")
  else
    as_ref(cl_reacts, c_datamodel)
}

reaction_obj <- function(key, c_datamodel, reference = NULL) {
  assert_that(
    is.character(key), noNA(key), !("" %in% key),
    is.null(reference) || is.string(reference) || is.character(reference) && length(key) == length(reference)
  )
  
  # If names are already DN to metabolites we accept them (disabled for regex)
  if (!inherits(key, "regex"))
    matches <- map(key, dn_to_object, c_datamodel, "_p_CReaction")
  else
    matches <- vector("list", length(key))
  
  matched <- lengths(matches) == 1L
  
  if (!all(matched)) {
    info <- "reaction(s)"
    cl_reacts <- get_cdv(c_datamodel$getModel()$getReactions())
    dns <- cl_reacts %>% map_swig_chr("getObjectDisplayName")
    # keys are needed as list, else attributes are lost on subsetting
    key_l <- seq_along(key) %>% map(subset_eng, x = apply_eng(key))
    
    # find full matches to ObjectDisplayName
    # str_replace as hack to find complete matches
    matches[!matched] <- map(key_l[!matched], ~ which(stringr::str_replace(dns, .x, "") == ""))
    matched <- lengths(matches) == 1L
    
    if (!all(matched)) {
      ns <- cl_reacts %>% map_swig_chr("getObjectName")
      # find full matches to ObjectName
      matches[!matched] <- map(key_l[!matched], ~ which(stringr::str_replace(ns, .x, "") == ""))
      matched <- lengths(matches) == 1L
      
      if (!all(matched)) {
        # then partial matches to ObjectDisplayName
        matches[!matched] <- map(key_l[!matched], stringr::str_which, string = dns)
        assert_matches(matches, key, dns, info)
        
        matched <- lengths(matches) == 1L
        
        assert_that(all(matched), msg = paste0(
          "Couldn't match ", info, ' "',
          key[!matched], '".',
          collapse = '", "'
        ))
      }
    }
    
    # the matches list contains integers and objects
    # integers signal matches and have to be converted to objects before returning
    matches <-
      matches %>%
      map_if(
        map_lgl(., is_scalar_integer),
        ~ cl_reacts[[.x]]
      )
  }
  
  if (is.null(reference))
    matches
  else
    apply_ref(matches, reference)
}

#' @rdname entity_finders
#' @family reaction functions
#' @export
parameter <- function(key = "", reference = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  assert_that(
    is.string(key), !is.na(key),
    is.null(reference) || is.string(reference)
  )
  
  cl_params <-
    get_cdv(c_datamodel$getModel()$getReactions()) %>%
    map_swig("getParameters") %>%
    map(function(paramgrp) {
      seq_along_v(paramgrp) %>% map(~ paramgrp$getParameter(.x))
    }) %>%
    flatten()
  
  if (key == "") {
    cl_matches <- cl_params
  } else {
    matches <- stringr::str_which(
      cl_params %>% map_swig_chr("getObjectDisplayName"),
      apply_eng(key)
    )
    cl_matches <- cl_params[matches]
  }
  
  if (is.null(reference))
    map_swig_chr(cl_matches, "getObjectDisplayName")
  else
    as_ref(apply_ref(cl_matches, reference), c_datamodel)
}

#' @rdname entity_finders
#' @family reaction functions
#' @export
parameter_strict <- function(key, reference = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  cl_params <- parameter_obj(key, c_datamodel, reference)

  if (is.null(reference))
    map_swig_chr(cl_params, "getObjectDisplayName")
  else
    as_ref(cl_params, c_datamodel)
}

parameter_obj <- function(key, c_datamodel, reference = NULL) {
  assert_that(
    is.character(key), noNA(key), !("" %in% key),
    is.null(reference) || is.string(reference) || is.character(reference) && length(key) == length(reference)
  )
  
  # If names are already DN to metabolites we accept them (disabled for regex)
  if (!inherits(key, "regex"))
    matches <- map(key, dn_to_object, c_datamodel, "_p_CCopasiParameter")
  else
    matches <- vector("list", length(key))
  
  matched <- lengths(matches) == 1L
  
  if (!all(matched)) {
    info <- "parameter(s)"
    cl_params <-
      get_cdv(c_datamodel$getModel()$getReactions()) %>%
      map_swig("getParameters") %>%
      map(function(paramgrp) {
        seq_along_v(paramgrp) %>% map(~ paramgrp$getParameter(.x))
      }) %>%
      flatten()
    dns <- cl_params %>% map_swig_chr("getObjectDisplayName")
    # keys are needed as list, else attributes are lost on subsetting
    key_l <- seq_along(key) %>% map(subset_eng, x = apply_eng(key))
    
    # find full matches to ObjectDisplayName
    # str_replace as hack to find complete matches
    matches[!matched] <- map(key_l[!matched], ~ which(stringr::str_replace(dns, .x, "") == ""))
    matched <- lengths(matches) == 1L
    
    if (!all(matched)) {
      ns <- cl_params %>% map_swig_chr("getObjectName")
      # find full matches to ObjectName
      matches[!matched] <- map(key_l[!matched], ~ which(stringr::str_replace(ns, .x, "") == ""))
      matched <- lengths(matches) == 1L
      
      if (!all(matched)) {
        # then partial matches to ObjectDisplayName
        matches[!matched] <- map(key_l[!matched], stringr::str_which, string = dns)
        assert_matches(matches, key, dns, info)
        
        matched <- lengths(matches) == 1L
        
        assert_that(all(matched), msg = paste0(
          "Couldn't match ", info, ' "',
          key[!matched], '".',
          collapse = '", "'
        ))
      }
    }
    
    # the matches list contains integers and objects
    # integers signal matches and have to be converted to objects before returning
    matches <-
      matches %>%
      map_if(
        map_lgl(., is_scalar_integer),
        ~ cl_params[[.x]]
      )
  }
  
  if (is.null(reference))
    matches
  else
    apply_ref(matches, reference)
}

#' @rdname entity_finders
#' @family reaction functions
#' @export
kinfunction <- function(key = "") {
  assert_that(
    is.string(key), !is.na(key)
  )
  
  c_fun_db <- CRootContainer_getFunctionList()
  
  cl_funs <- c_fun_db$loadedFunctions() %>% get_cdv()
  
  if (key == "") {
    cl_matches <- cl_funs
  } else {
    matches <- stringr::str_which(
      cl_funs %>% map_swig_chr("getObjectDisplayName"),
      apply_eng(key)
    )
    cl_matches <- cl_funs[matches]
  }
  
  map_swig_chr(cl_matches, "getObjectDisplayName")
}

#' @rdname entity_finders
#' @family reaction functions
#' @export
kinfunction_strict <- function(key) {
  cl_funs <- kinfunction_obj(key)
  
  map_swig_chr(cl_funs, "getObjectDisplayName")
}

kinfunction_obj <- function(key) {
  assert_that(
    is.character(key), noNA(key), !("" %in% key)
  )
  
  c_fun_db <- CRootContainer_getFunctionList()
  
  matches <- vector("list", length(key))
  
  matched <- rep(FALSE, length(key))
  
  info <- "functions(s)"
  cl_funs <- c_fun_db$loadedFunctions() %>% get_cdv()
  dns <- cl_funs %>% map_swig_chr("getObjectDisplayName")
  # keys are needed as list, else attributes are lost on subsetting
  key_l <- seq_along(key) %>% map(subset_eng, x = apply_eng(key))
  
  # find full matches to ObjectDisplayName
  # str_replace as hack to find complete matches
  matches[!matched] <- map(key_l[!matched], ~ which(stringr::str_replace(dns, .x, "") == ""))
  matched <- lengths(matches) == 1L
  
  if (!all(matched)) {
    ns <- cl_funs %>% map_swig_chr("getObjectName")
    # find full matches to ObjectName
    matches[!matched] <- map(key_l[!matched], ~ which(stringr::str_replace(ns, .x, "") == ""))
    matched <- lengths(matches) == 1L
    
    if (!all(matched)) {
      # then partial matches to ObjectDisplayName
      matches[!matched] <- map(key_l[!matched], stringr::str_which, string = dns)
      assert_matches(matches, key, dns, info)
      
      matched <- lengths(matches) == 1L
      
      assert_that(all(matched), msg = paste0(
        "Couldn't match ", info, ' "',
        key[!matched], '".',
        collapse = '", "'
      ))
    }
  }
  
  # the matches list contains integers and objects
  # integers signal matches and have to be converted to objects before returning
  matches %>%
    map_if(
      map_lgl(., is_scalar_integer),
      ~ cl_funs[[.x]]
    )
}

assert_matches <- function(matches, keys, names, info) {
  iwalk(matches, ~ {
    assert_that(
      length(.x) <= 1L,
      msg = paste0(
        '"', keys[.y], '" matches ', info, ' "',
        paste0(names[.x], collapse = '", "'),
        '".'
      )
    )
  })
}

apply_ref <- function(cl_objs, refs) {
  # If given reference string is scalar we replicate it for all matches
  if (length(refs) == 1L) refs <- rep(refs, length(cl_objs))
  
  map2(
    cl_objs,
    refs,
    ~ {
      c_obj <- .x$getObject(CCommonName(paste0("Reference=", .y)))
      assert_that(!is.null(c_obj), msg = paste0('Failed to gather reference "', .y, '".'))
      c_obj
    }
  )
}
