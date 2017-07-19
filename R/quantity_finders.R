#' Identify species by name
#'
#' \code{species} identifies species from the given name fragments.
#'
#' @param key a vector of strings to identify species
#' @param reference a scalar character or vetor of characters naming the value references to be returned.
#' @param datamodel a model object
#' @return a character vector of copasi keys
#' @export
species <- function(key, reference = NULL, datamodel = pkg_env$curr_dm) {
  assert_datamodel(datamodel)
  assert_that(
    is_character(key), !anyNA(key),
    is_null(reference) || is_scalar_character(reference) || is_character(reference) && length(key) == length(reference)
  )
  
  # If names are already CN to metabolites we accept them
  matched_metabs <- cn_to_object(key, datamodel, accepted_types = "_p_CMetab")
  
  is_matched <- matched_metabs %>% map_lgl(negate(is_null))
  
  if(!all(is_matched)) {
    key_remaining <- key[!is_matched]
    
    # Assemble vector of all metabolite displaynames
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
    !any(map_lgl(matched_metabs, is_null)),
    msg = "Failed to gather some references."
  )
  
  matched_metabs %>% map_swig("getCN") %>% map_swig("getString") %>% flatten_chr()
}
