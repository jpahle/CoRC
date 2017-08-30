#' Identify species by name
#'
#' \code{species} identifies species matching a given name fragment.
#'
#' @param key a string to identify species.
#' @param reference an optional string naming the value references to be returned.
#' @param model a model object.
#' @return a character vector of species identifiers or references.
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
  
  if (is_null(reference))
    map_swig_chr(cl_matches, "getObjectDisplayName")
  else
    as_ref(apply_ref(cl_matches, reference), c_datamodel)
}

#' Identify single species by name
#'
#' \code{species_strict} identifies strictly one species per given name fragment.
#'
#' @param key a vector of strings to identify species.
#' @param reference a string or vector of strings naming the value references to be returned.
#' @param model a model object.
#' @return a character vector of species identifiers or references.
#' @export
species_strict <- function(key, reference = NULL, model = getCurrentModel()) {
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
    # first full (str_replace is hacky to find complete matches)
    matches[!matched] <- map(key_l[!matched], ~ which(stringr::str_replace(ns, .x, "") == ""))
    matched <- map_int(matches, length) == 1L
    
    # then partial
    matches[!matched] <- map(key_l[!matched], stringr::str_which, string = ns)
    assert_matches(matches, key, dns, info)
    
    matched <- map_int(matches, length) == 1L
    
    if (!all(matched)) {
      # find matches to ObjectDisplayName
      # first full
      matches[!matched] <- map(key_l[!matched], ~ which(stringr::str_replace(dns, .x, "") == ""))
      matched <- map_int(matches, length) == 1L
             
      # then partial
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

#' Identify global quantities by name
#'
#' \code{quantity} identifies global quantities matching a given name fragment.
#'
#' @param key a string to identify global quantities.
#' @param reference an optional string naming the value references to be returned.
#' @param model a model object.
#' @return a character vector of global quantity identifiers or references.
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
  
  if (is_null(reference))
    map_swig_chr(cl_matches, "getObjectDisplayName")
  else
    as_ref(apply_ref(cl_matches, reference), c_datamodel)
}

#' Identify single global quantities by name
#'
#' \code{quantity_strict} identifies strictly one global quantity per given name fragment.
#'
#' @param key a vector of strings to identify global quantities.
#' @param reference a scalar character or vector of characters naming the value references to be returned.
#' @param model a model object.
#' @return a character vector of global quantity identifiers or references.
#' @export
quantity_strict <- function(key, reference = NULL, model = getCurrentModel()) {
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
    cl_quants <- get_cdv(c_datamodel$getModel()$getModelValues())
    ns <- cl_quants %>% map_swig_chr("getObjectName")
    # keys are needed as list, else attributes are lost on subsetting
    key_l <- seq_along(key) %>% map(subset_eng, x = apply_eng(key))
    
    # find matches to ObjectName
    # first full (str_replace is hacky to find complete matches)
    matches[!matched] <- map(key_l[!matched], ~ which(stringr::str_replace(ns, .x, "") == ""))
    matched <- map_int(matches, length) == 1L
    
    # then partial
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

#' Identify compartments by name
#'
#' \code{compartment} identifies compartments matching a given name fragments.
#'
#' @param key a string to identify compartments.
#' @param reference an optional string naming the value references to be returned.
#' @param model a model object.
#' @return a character vector of compartment identifiers or references.
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
  
  if (is_null(reference))
    map_swig_chr(cl_matches, "getObjectDisplayName")
  else
    as_ref(apply_ref(cl_matches, reference), c_datamodel)
}

#' Identify single compartments by name
#'
#' \code{compartment_strict} identifies strictly one compartment per given name fragment.
#'
#' @param key a vector of strings to identify compartments.
#' @param reference a scalar character or vector of characters naming the value references to be returned.
#' @param model a model object.
#' @return a character vector of compartment identifiers or references.
#' @export
compartment_strict <- function(key, reference = NULL, model = getCurrentModel()) {
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
    # first full (str_replace is hacky to find complete matches)
    matches[!matched] <- map(key_l[!matched], ~ which(stringr::str_replace(ns, .x, "") == ""))
    matched <- map_int(matches, length) == 1L
    
    # then partial
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

#' Identify reactions by name
#'
#' \code{reaction} identifies reactions matching a given name fragment.
#'
#' @param key a string to identify reactions.
#' @param reference an optional string naming the value references to be returned.
#' @param model a model object.
#' @return a character vector of reaction identifiers or references.
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
  
  if (is_null(reference))
    map_swig_chr(cl_matches, "getObjectDisplayName")
  else
    as_ref(apply_ref(cl_matches, reference), c_datamodel)
}

#' Identify single reactions by name
#'
#' \code{reaction_strict} identifies strictly one reaction per given name fragment.
#'
#' @param key a vector of strings to identify reactions.
#' @param reference a scalar character or vector of characters naming the value references to be returned.
#' @param model a model object.
#' @return a character vector of reaction identifiers or references.
#' @export
reaction_strict <- function(key, reference = NULL, model = getCurrentModel()) {
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
    # first full (str_replace is hacky to find complete matches)
    matches[!matched] <- map(key_l[!matched], ~ which(stringr::str_replace(ns, .x, "") == ""))
    matched <- map_int(matches, length) == 1L
    
    # then partial
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

#' Identify reaction parameters by name
#'
#' \code{parameter} identifies reaction parameters matching a given name fragment.
#'
#' @param key a string to identify reaction parameters.
#' @param reference an optional string naming the value references to be returned.
#' @param model a model object.
#' @return a character vector of reaction parameter identifiers or references.
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
  
  if (is_null(reference))
    map_swig_chr(cl_matches, "getObjectDisplayName")
  else
    as_ref(apply_ref(cl_matches, reference), c_datamodel)
}

#' Identify single reaction parameters by name
#'
#' \code{reaction_strict} identifies strictly one reaction parameter per given name fragment.
#'
#' @param key a vector of strings to identify reaction parameters.
#' @param reference a scalar character or vector of characters naming the value references to be returned.
#' @param model a model object.
#' @return a character vector of reaction parameter identifiers or references.
#' @export
parameter_strict <- function(key, reference = NULL, model = getCurrentModel()) {
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
    # first full (str_replace is hacky to find complete matches)
    matches[!matched] <- map(key_l[!matched], ~ which(stringr::str_replace(ns, .x, "") == ""))
    matched <- map_int(matches, length) == 1L
    
    # then partial
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
  if (length(refs) == 1L) refs <- rep(refs, length(cl_objs))
  
  map2(
    cl_objs,
    refs,
    ~ {
      c_obj <- .x$getObject(CCommonName(paste0("Reference=", .y)))
      assert_that(!is_null(c_obj), msg = paste0('Failed to gather reference "', .y, '".'))
      c_obj
    }
  )
}
