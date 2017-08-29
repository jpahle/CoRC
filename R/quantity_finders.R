#' Identify species by name
#'
#' \code{species} identifies species from the given name fragments.
#'
#' @param key a vector of strings to identify species
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
  
  # If names are already DN to metabolites we accept them
  cl_matched_metabs <- map(key, dn_to_object, c_datamodel, "_p_CMetab")
  
  is_matched <- cl_matched_metabs %>% map_lgl(negate(is_null))
  
  if(!all(is_matched)) {
    cl_metabs <- get_cdv(c_datamodel$getModel()$getMetabolites())
    
    matches_by_dispnames <-
      match_worker(
        keysvec = key[!is_matched],
        namesvec = cl_metabs %>% map_swig_chr("getObjectName"),
        info = "species"
      )
    
    # fill matched_comps list
    cl_matched_metabs[!is_matched] <- cl_metabs[matches_by_dispnames]
  }
  
  # If a reference is given, we use the matched metabolites to get value references
  if (!is_null(reference)) {
    # If given reference string is scalar we replicate it for all matches
    if (length(reference) == 1L) reference <- rep(reference, length(key))
    
    cl_matched_metabs <- map2(
      cl_matched_metabs,
      reference,
      ~ .x$getObject(CCommonName(paste0("Reference=", .y)))
    )
  }
  
  assert_that(
    !has_element(cl_matched_metabs, NULL),
    msg = "Failed to gather some references."
  )
  
  cl_matched_metabs
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
  
  # If names are already DN to quantities we accept them
  cl_matched_quants <- map(key, dn_to_object, c_datamodel, "_p_CModelValue")
  
  is_matched <- cl_matched_quants %>% map_lgl(negate(is_null))
  
  if(!all(is_matched)) {
    cl_quants <- get_cdv(c_datamodel$getModel()$getModelValues())
    
    matches_by_dispnames <-
      match_worker(
        keysvec = key[!is_matched],
        namesvec = cl_quants %>% map_swig_chr("getObjectName"),
        info = "global quantity(s)"
      )
    
    # fill matched_comps list
    cl_matched_quants[!is_matched] <- cl_quants[matches_by_dispnames]
  }
  
  # If a reference is given, we use the matched metabolites to get value references
  if (!is_null(reference)) {
    # If given reference string is scalar we replicate it for all matches
    if (length(reference) == 1L) reference <- rep(reference, length(key))
    
    cl_matched_quants <- map2(
      cl_matched_quants,
      reference,
      ~ .x$getObject(CCommonName(paste0("Reference=", .y)))
    )
  }
  
  assert_that(
    !has_element(cl_matched_quants, NULL),
    msg = "Failed to gather some references."
  )
  
  cl_matched_quants
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
  
  # If names are already DN to compartment we accept them
  cl_matched_comps <- map(key, dn_to_object, c_datamodel, "_p_CCompartment")
  
  is_matched <- cl_matched_comps %>% map_lgl(negate(is_null))
  
  if(!all(is_matched)) {
    cl_comps <- get_cdv(c_datamodel$getModel()$getCompartments())
    
    matches_by_dispnames <-
      match_worker(
        keysvec = key[!is_matched],
        namesvec = cl_comps %>% map_swig_chr("getObjectName"),
        info = "compartment(s)"
      )
    
    # fill matched_comps list
    cl_matched_comps[!is_matched] <- cl_comps[matches_by_dispnames]
  }
  
  # If a reference is given, we use the matched metabolites to get value references
  if (!is_null(reference)) {
    # If given reference string is scalar we replicate it for all matches
    if (length(reference) == 1L) reference <- rep(reference, length(key))
    
    cl_matched_comps <- map2(
      cl_matched_comps,
      reference,
      ~ .x$getObject(CCommonName(paste0("Reference=", .y)))
    )
  }
  
  assert_that(
    !has_element(cl_matched_comps, NULL),
    msg = "Failed to gather some references."
  )
  
  cl_matched_comps
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
  
  # If names are already DN to reactions we accept them
  cl_matched_reacts <- map(key, dn_to_object, c_datamodel, "_p_CReaction")
  
  is_matched <- cl_matched_reacts %>% map_lgl(negate(is_null))
  
  if(!all(is_matched)) {
    cl_reacts <- get_cdv(c_datamodel$getModel()$getReactions())
    
    matches_by_dispnames <-
      match_worker(
        keysvec = key[!is_matched],
        namesvec = cl_reacts %>% map_swig_chr("getObjectName"),
        info = "reaction(s)"
      )
    
    # fill matched_reactions list
    cl_matched_reacts[!is_matched] <- cl_reacts[matches_by_dispnames]
  }
  
  # If a reference is given, we use the matched metabolites to get value references
  if (!is_null(reference)) {
    # If given reference string is scalar we replicate it for all matches
    if (length(reference) == 1L) reference <- rep(reference, length(key))
    
    cl_matched_reacts <- map2(
      cl_matched_reacts,
      reference,
      ~ .x$getObject(CCommonName(paste0("Reference=", .y)))
    )
  }
  
  assert_that(
    !has_element(cl_matched_reacts, NULL),
    msg = "Failed to gather some references."
  )
  
  cl_matched_reacts
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
  
  # If names are already DN to parameters we accept them
  cl_matched_params <- map(key, dn_to_object, c_datamodel, "_p_CCopasiParameter")
  
  is_matched <- cl_matched_params %>% map_lgl(negate(is_null))
  
  if(!all(is_matched)) {
    cl_params <-
      get_cdv(c_datamodel$getModel()$getReactions()) %>%
      map_swig("getParameters") %>%
      map(function(paramgrp) {
        seq_along_v(paramgrp) %>% map(~ paramgrp$getParameter(.x))
      }) %>%
      flatten()
    
    matches_by_dispnames <-
      match_worker(
        keysvec = key[!is_matched],
        namesvec = cl_params %>% map_swig_chr("getObjectDisplayName"),
        info = "parameter(s)"
      )
    
    # fill matched_params list
    cl_matched_params[!is_matched] <- cl_params[matches_by_dispnames]
  }
  
  # If a reference is given, we use the matched metabolites to get value references
  if (!is_null(reference)) {
    # If given reference string is scalar we replicate it for all matches
    if (length(reference) == 1L) reference <- rep(reference, length(key))
    
    cl_matched_params <- map2(
      cl_matched_params,
      reference,
      ~ .x$getObject(CCommonName(paste0("Reference=", .y)))
    )
  }
  
  assert_that(
    !has_element(cl_matched_params, NULL),
    msg = "Failed to gather some references."
  )
  
  cl_matched_params
}

# Find a uniquely matching string in namesvec for every string in keysvec
match_worker <- function(keysvec, namesvec, info, partial = TRUE) {
  # Find all full matches of displaynames
  # This is needed because in later steps "C" would not resolve if "C" and "C1" are matching.
  matches <- match(keysvec, namesvec)
  
  not_matched <- is.na(matches)
  
  if (partial && any(not_matched)) {
    keysvec_remaining <- keysvec[not_matched]
    
    # Use give names as fixed pattern for searching in v_displaynames
    matches_partial <- keysvec_remaining %>% map(~ stringr::str_which(namesvec, stringr::coll(.x)))
    
    multi_matches <- which(map_int(matches_partial, length) > 1L)
    assert_that(
      is_empty(multi_matches),
      msg = paste0(
        "Could not correctly identify some ", info, ":\n",
        paste0(
          multi_matches %>% map_chr(~
            paste0(
              '"', keysvec_remaining[.x], '" matches ', info, ' "',
              paste0(namesvec[matches_partial[[.x]]], collapse = '", "'),
              '".'
            )
          ),
          collapse = "\n"
        )
      )
    )
    
    # Empty entries get NAed
    matches_partial[map_lgl(matches_partial, is_empty)] <- NA_integer_
    
    matches[not_matched] <- flatten_int(matches_partial)
  }
  
  no_matches <- is.na(matches)
  assert_that(
    !any(no_matches),
    msg = paste0(
      'Could not match ', info, ' "',
      keysvec[no_matches], '".',
      collapse = '", "'
    )
  )
  
  matches
}
