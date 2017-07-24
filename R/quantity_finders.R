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
  assert_that(
    is.character(key), !anyNA(key),
    is.null(reference) || is.string(reference) || is.character(reference) && length(key) == length(reference)
  )
  
  # If names are already CN to metabolites we accept them
  matched_metabs <- cn_to_object(key, datamodel, accepted_types = "_p_CMetab")
  
  is_matched <- matched_metabs %>% map_lgl(negate(is_null))
  
  if(!all(is_matched)) {
    key_remaining <- key[!is_matched]
    
    # Assemble vector of all displaynames
    metabs <- get_cdv(datamodel$getModel()$getMetabolites())
    v_dispnames <- metabs %>% map_swig("getObjectDisplayName") %>% flatten_chr()
    
    # Find all full matches of displaynames
    # This is needed because in later steps "C" would not resolve if "C" and "C1" are matching.
    matched_metabs[!is_matched] <-
      key_remaining %>% map(~ {
        match <- which(.x == v_dispnames)
        if (length(match) == 1L)
          metabs[[match]]
      })
    
    is_matched <- matched_metabs %>% map_lgl(negate(is_null))
    
    if(!all(is_matched)) {
      key_remaining <- key[!is_matched]
      
      # Use give names as fixed pattern for searching in v_displaynames
      matches <- key_remaining %>% map(~ stringr::str_which(v_dispnames, stringr::fixed(.x)))
      
      multi_matches <- which(map_int(matches, length) > 1L)
      assert_that(
        is_empty(multi_matches),
        msg = paste0(
          "Could not correctly identify some species:\n",
          paste0(
            multi_matches %>% map_chr(~
              paste0(
                '"', key_remaining[.x], '" matches species "',
                paste0(v_dispnames[matches[[.x]]], collapse = '", "'),
                '".'
              )
            ),
            collapse = "\n"
          )
        )
      )
      
      no_matches <- which(map_int(matches, length) == 0L)
      assert_that(
        is_empty(no_matches),
        msg = paste0(
          'Could not match species "',
          key_remaining[no_matches], '".',
          collapse = '", "'
        )
      )
      
      matched_metabs[!is_matched] <- metabs[flatten_int(matches)]
    }
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
  
  # # Return matches as named (DisplayNames) character vector of CommonNames
  # matched_metabs %>% map_swig("getCN") %>% map_swig("getString") %>% flatten_chr() %>%
  #   set_names(
  #     matched_metabs %>% map_swig("getObjectDisplayName") %>% flatten_chr()
  #   )
  matched_metabs %>% map_swig("getCN") %>% map_swig("getString") %>% flatten_chr()
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
  assert_that(
    is.character(key), !anyNA(key),
    is.null(reference) || is.string(reference) || is.character(reference) && length(key) == length(reference)
  )
  
  # If names are already CN to metabolites we accept them
  matched_quants <- cn_to_object(key, datamodel, accepted_types = "_p_CModelValue")
  
  is_matched <- matched_quants %>% map_lgl(negate(is_null))
  
  if(!all(is_matched)) {
    key_remaining <- key[!is_matched]
    
    # Assemble vector of all displaynames
    quants <- get_cdv(datamodel$getModel()$getModelValues())
    v_dispnames <- quants %>% map_swig("getObjectDisplayName") %>% flatten_chr()
    
    # Find all full matches of displaynames
    # This is needed because in later steps "C" would not resolve if "C" and "C1" are matching.
    matched_quants[!is_matched] <-
      key_remaining %>% map(~ {
        match <- which(.x == v_dispnames)
        if (length(match) == 1L)
          quants[[match]]
      })
    
    is_matched <- matched_quants %>% map_lgl(negate(is_null))
    
    if(!all(is_matched)) {
      key_remaining <- key[!is_matched]
      
      # Use give names as fixed pattern for searching in v_displaynames
      matches <- key_remaining %>% map(~ stringr::str_which(v_dispnames, stringr::fixed(.x)))
      
      multi_matches <- which(map_int(matches, length) > 1L)
      assert_that(
        is_empty(multi_matches),
        msg = paste0(
          "Could not correctly identify some global quantites:\n",
          paste0(
            multi_matches %>% map_chr(~
              paste0(
                '"', key_remaining[.x], '" matches quantites "',
                paste0(v_dispnames[matches[[.x]]], collapse = '", "'),
                '".'
              )
            ),
            collapse = "\n"
          )
        )
      )
      
      no_matches <- which(map_int(matches, length) == 0L)
      assert_that(
        is_empty(no_matches),
        msg = paste0(
          'Could not match global quantity "',
          key_remaining[no_matches], '".',
          collapse = '", "'
        )
      )
      
      matched_quants[!is_matched] <- quants[flatten_int(matches)]
    }
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
  
  # # Return matches as named (DisplayNames) character vector of CommonNames
  # matched_quants %>% map_swig("getCN") %>% map_swig("getString") %>% flatten_chr() %>%
  #   set_names(
  #     matched_quants %>% map_swig("getObjectDisplayName") %>% flatten_chr()
  #   )
  matched_quants %>% map_swig("getObjectDisplayName") %>% flatten_chr()
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
  assert_that(
    is.character(key), !anyNA(key),
    is.null(reference) || is.string(reference) || is.character(reference) && length(key) == length(reference)
  )
  
  # If names are already CN to metabolites we accept them
  matched_comps <- cn_to_object(key, datamodel, accepted_types = "_p_CCompartment")
  
  is_matched <- matched_comps %>% map_lgl(negate(is_null))
  
  if(!all(is_matched)) {
    key_remaining <- key[!is_matched]
    
    # Assemble vector of all displaynames
    comps <- get_cdv(datamodel$getModel()$getCompartments())
    v_dispnames <- comps %>% map_swig("getObjectDisplayName") %>% flatten_chr()
    
    # Find all full matches of displaynames
    # This is needed because in later steps "C" would not resolve if "C" and "C1" are matching.
    matched_comps[!is_matched] <-
      key_remaining %>% map(~ {
        match <- which(.x == v_dispnames)
        if (length(match) == 1L)
          comps[[match]]
      })
    
    is_matched <- matched_comps %>% map_lgl(negate(is_null))
    
    if(!all(is_matched)) {
      key_remaining <- key[!is_matched]
      
      # Use give names as fixed pattern for searching in v_displaynames
      matches <- key_remaining %>% map(~ stringr::str_which(v_dispnames, stringr::fixed(.x)))
      
      multi_matches <- which(map_int(matches, length) > 1L)
      assert_that(
        is_empty(multi_matches),
        msg = paste0(
          "Could not correctly identify some compartments:\n",
          paste0(
            multi_matches %>% map_chr(~
              paste0(
                '"', key_remaining[.x], '" matches compartments "',
                paste0(v_dispnames[matches[[.x]]], collapse = '", "'),
                '".'
              )
            ),
            collapse = "\n"
          )
        )
      )
      
      no_matches <- which(map_int(matches, length) == 0L)
      assert_that(
        is_empty(no_matches),
        msg = paste0(
          'Could not match compartment "',
          key_remaining[no_matches], '".',
          collapse = '", "'
        )
      )
      
      matched_comps[!is_matched] <- comps[flatten_int(matches)]
    }
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
  
  # # Return matches as named (DisplayNames) character vector of CommonNames
  # matched_comps %>% map_swig("getCN") %>% map_swig("getString") %>% flatten_chr() %>%
  #   set_names(
  #     matched_comps %>% map_swig("getObjectDisplayName") %>% flatten_chr()
  #   )
  matched_comps %>% map_swig("getCN") %>% map_swig("getString") %>% flatten_chr()
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
  assert_that(
    is.character(key), !anyNA(key),
    is.null(reference) || is.string(reference) || is.character(reference) && length(key) == length(reference)
  )
  
  # If names are already CN to metabolites we accept them
  matched_reactions <- cn_to_object(key, datamodel, accepted_types = "_p_CReaction")
  
  is_matched <- matched_reactions %>% map_lgl(negate(is_null))
  
  if(!all(is_matched)) {
    key_remaining <- key[!is_matched]
    
    # Assemble vector of all displaynames
    reactions <- get_cdv(datamodel$getModel()$getReactions())
    v_dispnames <- reactions %>% map_swig("getObjectDisplayName") %>% flatten_chr()
    
    # Find all full matches of displaynames
    # This is needed because in later steps "C" would not resolve if "C" and "C1" are matching.
    matched_reactions[!is_matched] <-
      key_remaining %>% map(~ {
        match <- which(.x == v_dispnames)
        if (length(match) == 1L)
          reactions[[match]]
      })
    
    is_matched <- matched_reactions %>% map_lgl(negate(is_null))
    
    if(!all(is_matched)) {
      key_remaining <- key[!is_matched]
      
      # Use give names as fixed pattern for searching in v_displaynames
      matches <- key_remaining %>% map(~ stringr::str_which(v_dispnames, stringr::fixed(.x)))
      
      multi_matches <- which(map_int(matches, length) > 1L)
      assert_that(
        is_empty(multi_matches),
        msg = paste0(
          "Could not correctly identify some reactions:\n",
          paste0(
            multi_matches %>% map_chr(~
              paste0(
                '"', key_remaining[.x], '" matches reactions "',
                paste0(v_dispnames[matches[[.x]]], collapse = '", "'),
                '".'
              )
            ),
            collapse = "\n"
          )
        )
      )
      
      no_matches <- which(map_int(matches, length) == 0L)
      assert_that(
        is_empty(no_matches),
        msg = paste0(
          'Could not match reaction "',
          key_remaining[no_matches], '".',
          collapse = '", "'
        )
      )
      
      matched_reactions[!is_matched] <- reactions[flatten_int(matches)]
    }
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
  
  # # Return matches as named (DisplayNames) character vector of CommonNames
  # matched_reactions %>% map_swig("getCN") %>% map_swig("getString") %>% flatten_chr() %>%
  #   set_names(
  #     matched_reactions %>% map_swig("getObjectDisplayName") %>% flatten_chr()
  #   )
  matched_reactions %>% map_swig("getCN") %>% map_swig("getString") %>% flatten_chr()
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
  assert_that(
    is.character(key), !anyNA(key),
    is.null(reference) || is.string(reference) || is.character(reference) && length(key) == length(reference)
  )
  
  # If names are already CN to metabolites we accept them
  matched_params <- cn_to_object(key, datamodel, accepted_types = "_p_CCopasiParameter")
  
  is_matched <- matched_params %>% map_lgl(negate(is_null))
  
  if(!all(is_matched)) {
    key_remaining <- key[!is_matched]
    
    # Assemble vector of all displaynames
    params <-
      get_cdv(datamodel$getModel()$getReactions()) %>%
      map_swig("getParameters") %>%
      map(function(paramgrp) {
        seq_along_v(paramgrp) %>% map(~ paramgrp$getParameter(.x))
      }) %>%
      flatten()
    v_dispnames <- params %>% map_swig("getObjectDisplayName") %>% flatten_chr()
    
    # Find all full matches of displaynames
    # This is needed because in later steps "C" would not resolve if "C" and "C1" are matching.
    matched_params[!is_matched] <-
      key_remaining %>% map(~ {
        match <- which(.x == v_dispnames)
        if (length(match) == 1L)
          params[[match]]
      })
    
    is_matched <- matched_params %>% map_lgl(negate(is_null))
    
    if(!all(is_matched)) {
      key_remaining <- key[!is_matched]
      
      # Use give names as fixed pattern for searching in v_displaynames
      matches <- key_remaining %>% map(~ stringr::str_which(v_dispnames, stringr::fixed(.x)))
      
      multi_matches <- which(map_int(matches, length) > 1L)
      assert_that(
        is_empty(multi_matches),
        msg = paste0(
          "Could not correctly identify some parameters:\n",
          paste0(
            multi_matches %>% map_chr(~
              paste0(
                '"', key_remaining[.x], '" matches parameters "',
                paste0(v_dispnames[matches[[.x]]], collapse = '", "'),
                '".'
              )
            ),
            collapse = "\n"
          )
        )
      )
      
      no_matches <- which(map_int(matches, length) == 0L)
      assert_that(
        is_empty(no_matches),
        msg = paste0(
          'Could not match parameter "',
          key_remaining[no_matches], '".',
          collapse = '", "'
        )
      )
      
      matched_params[!is_matched] <- params[flatten_int(matches)]
    }
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
  
  # # Return matches as named (DisplayNames) character vector of CommonNames
  # matched_params %>% map_swig("getCN") %>% map_swig("getString") %>% flatten_chr() %>%
  #   set_names(
  #     matched_params %>% map_swig("getObjectDisplayName") %>% flatten_chr()
  #   )
  matched_params %>% map_swig("getCN") %>% map_swig("getString") %>% flatten_chr()
}
