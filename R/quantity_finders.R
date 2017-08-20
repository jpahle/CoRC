#' Identify species by name
#'
#' \code{species} identifies species from the given name fragments.
#'
#' @param key a vector of strings to identify species
#' @param reference a scalar character or vector of characters naming the value references to be returned.
#' @param datamodel a model object
#' @return a character vector of copasi keys
#' @export
species <- function(key, reference = NULL, datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  
  species_obj(key, reference, datamodel) %>% map_swig_chr("getObjectDisplayName")
}

species_obj <- function(key, reference = NULL, datamodel) {
  assert_that(
    is.character(key), !anyNA(key), !("" %in% key),
    is.null(reference) || is.string(reference) || is.character(reference) && length(key) == length(reference)
  )
  
  # If names are already DN to metabolites we accept them
  matched_metabs <- dn_to_object(key, datamodel, accepted_types = "_p_CMetab")
  
  is_matched <- matched_metabs %>% map_lgl(negate(is_null))
  
  if(!all(is_matched)) {
    metabs <- get_cdv(datamodel$getModel()$getMetabolites())
    
    matches_by_dispnames <-
      match_worker(
        keysvec = key[!is_matched],
        namesvec = metabs %>% map_swig_chr("getObjectDisplayName"),
        info = "species"
      )
    
    # fill matched_comps list
    matched_metabs[!is_matched] <- metabs[matches_by_dispnames]
  }
  
  # If a reference is given, we use the matched metabolites to get value references
  if (!is_null(reference)) {
    # If given reference string is scalar we replicate it for all matches
    if (length(reference) == 1L) reference <- rep(reference, length(key))
    
    matched_metabs <- map2(
      matched_metabs,
      reference,
      ~ .x$getObject(CCommonName(paste0("Reference=", .y)))
    )
  }
  
  assert_that(
    !has_element(matched_metabs, NULL),
    msg = "Failed to gather some references."
  )
  
  matched_metabs
}

#' Identify quantity by name
#'
#' \code{quantity} identifies global quantities from the given name fragments.
#'
#' @param key a vector of strings to identify quantities
#' @param reference a scalar character or vector of characters naming the value references to be returned.
#' @param datamodel a model object
#' @return a character vector of copasi keys
#' @export
quantity <- function(key, reference = NULL, datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  
  quantity_obj(key, reference, datamodel) %>% map_swig_chr("getObjectDisplayName")
}

quantity_obj <- function(key, reference = NULL, datamodel) {
  assert_that(
    is.character(key), !anyNA(key), !("" %in% key),
    is.null(reference) || is.string(reference) || is.character(reference) && length(key) == length(reference)
  )
  
  # If names are already DN to quantities we accept them
  matched_quants <- dn_to_object(key, datamodel, accepted_types = "_p_CModelValue")
  
  is_matched <- matched_quants %>% map_lgl(negate(is_null))
  
  if(!all(is_matched)) {
    quants <- get_cdv(datamodel$getModel()$getModelValues())
    
    matches_by_dispnames <-
      match_worker(
        keysvec = key[!is_matched],
        namesvec = quants %>% map_swig_chr("getObjectDisplayName"),
        info = "global quantity(s)"
      )
    
    # fill matched_comps list
    matched_quants[!is_matched] <- quants[matches_by_dispnames]
  }
  
  # If a reference is given, we use the matched metabolites to get value references
  if (!is_null(reference)) {
    # If given reference string is scalar we replicate it for all matches
    if (length(reference) == 1L) reference <- rep(reference, length(key))
    
    matched_quants <- map2(
      matched_quants,
      reference,
      ~ .x$getObject(CCommonName(paste0("Reference=", .y)))
    )
  }
  
  assert_that(
    !has_element(matched_quants, NULL),
    msg = "Failed to gather some references."
  )
  
  matched_quants
}

#' Identify compartment by name
#'
#' \code{compartment} identifies compartments from the given name fragments.
#'
#' @param key a vector of strings to identify compartments
#' @param reference a scalar character or vector of characters naming the value references to be returned.
#' @param datamodel a model object
#' @return a character vector of copasi keys
#' @export
compartment <- function(key, reference = NULL, datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  
  compartment_obj(key, reference, datamodel) %>% map_swig_chr("getObjectDisplayName")
}

compartment_obj <- function(key, reference = NULL, datamodel) {
  assert_that(
    is.character(key), !anyNA(key), !("" %in% key),
    is.null(reference) || is.string(reference) || is.character(reference) && length(key) == length(reference)
  )
  
  # If names are already DN to compartment we accept them
  matched_comps <- dn_to_object(key, datamodel, accepted_types = "_p_CCompartment")
  
  is_matched <- matched_comps %>% map_lgl(negate(is_null))
  
  if(!all(is_matched)) {
    comps <- get_cdv(datamodel$getModel()$getCompartments())
    
    matches_by_dispnames <-
      match_worker(
        keysvec = key[!is_matched],
        namesvec = comps %>% map_swig_chr("getObjectDisplayName"),
        info = "compartment(s)"
      )
    
    # fill matched_comps list
    matched_comps[!is_matched] <- comps[matches_by_dispnames]
  }
  
  # If a reference is given, we use the matched metabolites to get value references
  if (!is_null(reference)) {
    # If given reference string is scalar we replicate it for all matches
    if (length(reference) == 1L) reference <- rep(reference, length(key))
    
    matched_comps <- map2(
      matched_comps,
      reference,
      ~ .x$getObject(CCommonName(paste0("Reference=", .y)))
    )
  }
  
  assert_that(
    !has_element(matched_comps, NULL),
    msg = "Failed to gather some references."
  )
  
  matched_comps
}

#' Identify reaction by name
#'
#' \code{reaction} identifies reactions from the given name fragments.
#'
#' @param key a vector of strings to identify reactions
#' @param reference a scalar character or vector of characters naming the value references to be returned.
#' @param datamodel a model object
#' @return a character vector of copasi keys
#' @export
reaction <- function(key, reference = NULL, datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  
  reaction_obj(key, reference, datamodel) %>% map_swig_chr("getObjectDisplayName")
}

reaction_obj <- function(key, reference = NULL, datamodel) {
  assert_that(
    is.character(key), !anyNA(key), !("" %in% key),
    is.null(reference) || is.string(reference) || is.character(reference) && length(key) == length(reference)
  )
  
  # If names are already DN to reactions we accept them
  matched_reactions <- dn_to_object(key, datamodel, accepted_types = "_p_CReaction")
  
  is_matched <- matched_reactions %>% map_lgl(negate(is_null))
  
  if(!all(is_matched)) {
    reactions <- get_cdv(datamodel$getModel()$getReactions())
    
    matches_by_dispnames <-
      match_worker(
        keysvec = key[!is_matched],
        namesvec = reactions %>% map_swig_chr("getObjectDisplayName"),
        info = "reaction(s)"
      )
    
    # fill matched_reactions list
    matched_reactions[!is_matched] <- reactions[matches_by_dispnames]
  }
  
  # If a reference is given, we use the matched metabolites to get value references
  if (!is_null(reference)) {
    # If given reference string is scalar we replicate it for all matches
    if (length(reference) == 1L) reference <- rep(reference, length(key))
    
    matched_reactions <- map2(
      matched_reactions,
      reference,
      ~ .x$getObject(CCommonName(paste0("Reference=", .y)))
    )
  }
  
  assert_that(
    !has_element(matched_reactions, NULL),
    msg = "Failed to gather some references."
  )
  
  matched_reactions
}

#' Identify reaction parameter by name
#'
#' \code{parameter} identifies reaction parameters from the given name fragments.
#'
#' @param key a vector of strings to identify reaction parameters
#' @param reference a scalar character or vector of characters naming the value references to be returned.
#' @param datamodel a model object
#' @return a character vector of copasi keys
#' @export
parameter <- function(key, reference = NULL, datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  
  parameter_obj(key, reference, datamodel) %>% map_swig_chr("getObjectDisplayName")
}

parameter_obj <- function(key, reference = NULL, datamodel) {
  assert_that(
    is.character(key), !anyNA(key), !("" %in% key),
    is.null(reference) || is.string(reference) || is.character(reference) && length(key) == length(reference)
  )
  
  # If names are already DN to parameters we accept them
  matched_params <- dn_to_object(key, datamodel, accepted_types = "_p_CCopasiParameter")
  
  is_matched <- matched_params %>% map_lgl(negate(is_null))
  
  if(!all(is_matched)) {
    params <-
      get_cdv(datamodel$getModel()$getReactions()) %>%
      map_swig("getParameters") %>%
      map(function(paramgrp) {
        seq_along_v(paramgrp) %>% map(~ paramgrp$getParameter(.x))
      }) %>%
      flatten()
    
    matches_by_dispnames <-
      match_worker(
        keysvec = key[!is_matched],
        namesvec = params %>% map_swig_chr("getObjectDisplayName"),
        info = "parameter(s)"
      )
    
    # fill matched_params list
    matched_params[!is_matched] <- params[matches_by_dispnames]
  }
  
  # If a reference is given, we use the matched metabolites to get value references
  if (!is_null(reference)) {
    # If given reference string is scalar we replicate it for all matches
    if (length(reference) == 1L) reference <- rep(reference, length(key))
    
    matched_params <- map2(
      matched_params,
      reference,
      ~ .x$getObject(CCommonName(paste0("Reference=", .y)))
    )
  }
  
  assert_that(
    !has_element(matched_params, NULL),
    msg = "Failed to gather some references."
  )
  
  matched_params
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
    matches_partial <- keysvec_remaining %>% map(~ stringr::str_which(namesvec, stringr::fixed(.x)))
    
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
