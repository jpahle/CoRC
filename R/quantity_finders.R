#' @export
species <- function(name, reference = NULL, datamodel = pkg_env$curr_dm) {
  assert_that(
    confirmDatamodel(datamodel),
    is_character(name), !any(is.na(name)),
    is_null(reference) || is_scalar_character(reference) || is_character(reference) && length(name) == length(reference)
  )
  
  metabs_list <- get_cdv(datamodel$getModel()$getMetabolites())
  
  v_names <- metabs_list %>% map_swig("getObjectName") %>% flatten_chr()
  indices <- pmatch(name, v_names)
  
  not_matched <- which(is.na(indices))
  
  if (!is_empty(not_matched)) {
    v_dispnames <- metabs_list %>% map_swig("getObjectDisplayName") %>% flatten_chr()
    indices %>% na.omit() %>% walk(~ {v_dispnames[.x] <<- NA_character_})
    
    indices[not_matched] <- pmatch(name[not_matched], v_dispnames)
  }
  
  ret <- metabs_list[indices]
  
  if (!is_null(reference)) {
    if (length(reference) == 1L) reference <- rep(reference, length(name))
    
    ret <- map2(
      ret,
      reference,
      ~ {if (is_null(.x)) NULL else .x$getObject(CCommonName(paste0("Reference=", .y)))}
    )
  }
  
  ret %>% map_chr(~ {if (is_null(.x)) NA_character_ else .x$getCN()$getString()})
}
