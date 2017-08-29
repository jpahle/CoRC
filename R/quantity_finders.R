#' Identify species by name
#'
#' \code{species} identifies species from the given name fragments.
#'
#' @param key a vector of strings to identify species.
#' @param reference a scalar character or vector of characters naming the value references to be returned.
#' @param model a model object
#' @return a character vector of copasi keys
#' @export
species <- function(key, reference = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  cl_metabs <- species_obj(key, c_datamodel, reference)
  
  if (is_null(reference))
    map_swig_chr(cl_metabs, "getObjectDisplayName")
  else
    as_ref(cl_metabs, c_datamodel)
}

species_obj <- function(key, c_datamodel, reference = NULL) {
  assert_that(
    is.character(key), !anyNA(key), !("" %in% key),
    is.null(reference) || is.string(reference) || is.character(reference) && length(key) == length(reference)
  )
  
  # If names are already DN to metabolites we accept them (disabled for regex)
  if (inherits(key, "regex"))
    matches <- map(key, dn_to_object, c_datamodel, "_p_CMetab")
  else
    matches <- vector("list", length(key))
  
  matched <- map_int(matches, length) == 1L
  
  if (!all(matched)) {
    info <- "species"
    cl_metabs <- get_cdv(c_datamodel$getModel()$getMetabolites())
    ns <- cl_metabs %>% map_swig_chr("getObjectName")
    dns <- cl_metabs %>% map_swig_chr("getObjectDisplayName")
    # keys are needed as list, else attributes are lost on subsetting
    key_l <- seq_along(key) %>% map(subset_eng, x = apply_eng(key))
    
    # find matches to ObjectName
    matches[!matched] <- map(key_l[!matched], stringr::str_which, string = ns)
    assert_matches(matches, key, dns, info)
    
    matched <- map_int(matches, length) == 1L
    
    if (!all(matched)) {
      # find matches to ObjectDisplayName
      matches[!matched] <- map(key_l[!matched], stringr::str_which, string = dns)
      assert_matches(matches, key, dns, info)
      
      matched <- map_int(matches, length) == 1L
      
      assert_that(all(matched), msg = paste0(
        "Couldn't match ", info, ' "',
        key[!matched], '".',
        collapse = '", "'
      ))
    }
    
    # apply all missing matches
    matches <-
      matches %>%
      map_if(
        map_lgl(., is_scalar_integer),
        ~ cl_metabs[[.x]]
      )
  }
  
  if (is_null(reference))
    matches
  else
    apply_ref(matches, reference)
}

#' Identify quantity by name
#'
#' \code{quantity} identifies global quantities from the given name fragments.
#'
#' @param key a vector of strings to identify quantities
#' @param reference a scalar character or vector of characters naming the value references to be returned.
#' @param model a model object
#' @return a character vector of copasi keys
#' @export
quantity <- function(key, reference = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  cl_quants <- quantity_obj(key, c_datamodel, reference)
  
  if (is_null(reference))
    map_swig_chr(cl_quants, "getObjectDisplayName")
  else
    as_ref(cl_quants, c_datamodel)
}

quantity_obj <- function(key, c_datamodel, reference = NULL) {
  assert_that(
    is.character(key), !anyNA(key), !("" %in% key),
    is.null(reference) || is.string(reference) || is.character(reference) && length(key) == length(reference)
  )
  
  # If names are already DN to metabolites we accept them (disabled for regex)
  if (inherits(key, "regex"))
    matches <- map(key, dn_to_object, c_datamodel, "_p_CModelValue")
  else
    matches <- vector("list", length(key))
  
  matched <- map_int(matches, length) == 1L
  
  if (!all(matched)) {
    info <- "global quantity(s)"
    cl_quants <- get_cdv(c_datamodel$getModel()$get$getModelValues())
    ns <- cl_quants %>% map_swig_chr("getObjectName")
    # keys are needed as list, else attributes are lost on subsetting
    key_l <- seq_along(key) %>% map(subset_eng, x = apply_eng(key))
    
    # find matches to ObjectName
    matches[!matched] <- map(key_l[!matched], stringr::str_which, string = ns)
    assert_matches(matches, key, ns, info)
    
    matched <- map_int(matches, length) == 1L
      
    assert_that(all(matched), msg = paste0(
      "Couldn't match ", info, ' "',
      key[!matched], '".',
      collapse = '", "'
    ))
    
    # apply all missing matches
    matches <-
      matches %>%
      map_if(
        map_lgl(., is_scalar_integer),
        ~ cl_quants[[.x]]
      )
  }
  
  if (is_null(reference))
    matches
  else
    apply_ref(matches, reference)
}

#' Identify compartment by name
#'
#' \code{compartment} identifies compartments from the given name fragments.
#'
#' @param key a vector of strings to identify compartments
#' @param reference a scalar character or vector of characters naming the value references to be returned.
#' @param model a model object
#' @return a character vector of copasi keys
#' @export
compartment <- function(key, reference = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  cl_comps <- compartment_obj(key, c_datamodel, reference)
  
  if (is_null(reference))
    map_swig_chr(cl_comps, "getObjectDisplayName")
  else
    as_ref(cl_comps, c_datamodel)
}

compartment_obj <- function(key, c_datamodel, reference = NULL) {
  assert_that(
    is.character(key), !anyNA(key), !("" %in% key),
    is.null(reference) || is.string(reference) || is.character(reference) && length(key) == length(reference)
  )
  
  # If names are already DN to metabolites we accept them (disabled for regex)
  if (inherits(key, "regex"))
    matches <- map(key, dn_to_object, c_datamodel, "_p_CCompartment")
  else
    matches <- vector("list", length(key))
  
  matched <- map_int(matches, length) == 1L
  
  if (!all(matched)) {
    info <- "compartment(s)"
    cl_comps <- get_cdv(c_datamodel$getModel()$getCompartments())
    ns <- cl_comps %>% map_swig_chr("getObjectName")
    # keys are needed as list, else attributes are lost on subsetting
    key_l <- seq_along(key) %>% map(subset_eng, x = apply_eng(key))
    
    # find matches to ObjectName
    matches[!matched] <- map(key_l[!matched], stringr::str_which, string = ns)
    assert_matches(matches, key, ns, info)
    
    matched <- map_int(matches, length) == 1L
    
    assert_that(all(matched), msg = paste0(
      "Couldn't match ", info, ' "',
      key[!matched], '".',
      collapse = '", "'
    ))
    
    # apply all missing matches
    matches <-
      matches %>%
      map_if(
        map_lgl(., is_scalar_integer),
        ~ cl_comps[[.x]]
      )
  }
  
  if (is_null(reference))
    matches
  else
    apply_ref(matches, reference)
}

#' Identify reaction by name
#'
#' \code{reaction} identifies reactions from the given name fragments.
#'
#' @param key a vector of strings to identify reactions
#' @param reference a scalar character or vector of characters naming the value references to be returned.
#' @param model a model object
#' @return a character vector of copasi keys
#' @export
reaction <- function(key, reference = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  cl_reacts <- reaction_obj(key, c_datamodel, reference) %>% map_swig_chr("getObjectDisplayName")
  
  if (is_null(reference))
    map_swig_chr(cl_reacts, "getObjectDisplayName")
  else
    as_ref(cl_reacts, c_datamodel)
}

reaction_obj <- function(key, c_datamodel, reference = NULL) {
  assert_that(
    is.character(key), !anyNA(key), !("" %in% key),
    is.null(reference) || is.string(reference) || is.character(reference) && length(key) == length(reference)
  )
  
  # If names are already DN to metabolites we accept them (disabled for regex)
  if (inherits(key, "regex"))
    matches <- map(key, dn_to_object, c_datamodel, "_p_CReaction")
  else
    matches <- vector("list", length(key))
  
  matched <- map_int(matches, length) == 1L
  
  if (!all(matched)) {
    info <- "reaction(s)"
    cl_reacts <- get_cdv(c_datamodel$getModel()$getReactions())
    ns <- cl_reacts %>% map_swig_chr("getObjectName")
    # keys are needed as list, else attributes are lost on subsetting
    key_l <- seq_along(key) %>% map(subset_eng, x = apply_eng(key))
    
    # find matches to ObjectName
    matches[!matched] <- map(key_l[!matched], stringr::str_which, string = ns)
    assert_matches(matches, key, ns, info)
    
    matched <- map_int(matches, length) == 1L
    
    assert_that(all(matched), msg = paste0(
      "Couldn't match ", info, ' "',
      key[!matched], '".',
      collapse = '", "'
    ))
    
    # apply all missing matches
    matches <-
      matches %>%
      map_if(
        map_lgl(., is_scalar_integer),
        ~ cl_reacts[[.x]]
      )
  }
  
  if (is_null(reference))
    matches
  else
    apply_ref(matches, reference)
}

#' Identify reaction parameter by name
#'
#' \code{parameter} identifies reaction parameters from the given name fragments.
#'
#' @param key a vector of strings to identify reaction parameters
#' @param reference a scalar character or vector of characters naming the value references to be returned.
#' @param model a model object
#' @return a character vector of copasi keys
#' @export
parameter <- function(key, reference = NULL, model = getCurrentModel()) {
  c_datamodel <- assert_datamodel(model)
  
  cl_params <- parameter_obj(key, c_datamodel, reference)

  if (is_null(reference))
    map_swig_chr(cl_params, "getObjectDisplayName")
  else
    as_ref(cl_params, c_datamodel)
}

parameter_obj <- function(key, c_datamodel, reference = NULL) {
  assert_that(
    is.character(key), !anyNA(key), !("" %in% key),
    is.null(reference) || is.string(reference) || is.character(reference) && length(key) == length(reference)
  )
  
  # If names are already DN to metabolites we accept them (disabled for regex)
  if (!inherits(key, "regex"))
    matches <- map(key, dn_to_object, c_datamodel, "_p_CCopasiParameter")
  else
    matches <- vector("list", length(key))
  
  matched <- map_int(matches, length) == 1L
  
  if (!all(matched)) {
    info <- "parameter(s)"
    cl_params <-
      get_cdv(c_datamodel$getModel()$getReactions()) %>%
      map_swig("getParameters") %>%
      map(function(paramgrp) {
        seq_along_v(paramgrp) %>% map(~ paramgrp$getParameter(.x))
      }) %>%
      flatten()
    ns <- cl_params %>% map_swig_chr("getObjectDisplayName")
    # keys are needed as list, else attributes are lost on subsetting
    key_l <- seq_along(key) %>% map(subset_eng, x = apply_eng(key))
    
    # find matches to ObjectName
    matches[!matched] <- map(key_l[!matched], stringr::str_which, string = ns)
    assert_matches(matches, key, ns, info)
    
    matched <- map_int(matches, length) == 1L
    
    assert_that(all(matched), msg = paste0(
      "Couldn't match ", info, ' "',
      key[!matched], '".',
      collapse = '", "'
    ))
    
    # apply all missing matches
    matches <-
      matches %>%
      map_if(
        map_lgl(., is_scalar_integer),
        ~ cl_params[[.x]]
      )
  }
  
  if (is_null(reference))
    matches
  else
    apply_ref(matches, reference)
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
  if (length(reference) == 1L) reference <- rep(reference, length(cl_objs))
  
  map2(
    cl_objs,
    reference,
    ~ {
      c_obj <- .x$getObject(CCommonName(paste0("Reference=", .y)))
      assert_that(!is_null(c_obj), msg = paste0('Failed to gather reference "', .y, '".'))
      c_obj
    }
  )
}
