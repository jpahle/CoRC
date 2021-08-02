# By comparing the output of getObjectType with this list, we can safely typecast it seems
copasi_object_types <-
  c(
    Model           = "_p_CModel",
    Compartment     = "_p_CCompartment",
    Metabolite      = "_p_CMetab",
    ModelValue      = "_p_CModelValue",
    Reaction        = "_p_CReaction",
    Parameter       = "_p_CCopasiParameter",
    Event           = "_p_CEvent",
    EventAssignment = "_p_CEventAssignment",
    Function        = "_p_CFunction"
  )

# automatically cast according to copasi_object_types
auto_cast <- function(c_object) {
  type <- copasi_object_types[c_object$getObjectType()]
  
  if (is.na(type))
    c_object
  else
    as(c_object, type)
}

# Takes CN as character and gives object
cn_to_object <- function(cn, c_datamodel, accepted_types = NULL) {
  c_object <- c_datamodel$getObjectFromCN(CCommonName(cn))
  
  if (is.null(c_object))
    return()
  
  c_object <- auto_cast(c_object$getDataObject())
  
  if (!is.null(accepted_types) && !inherits(c_object, accepted_types))
    return()
  
  c_object
}

# Takes DisplayName as character and gives object
dn_to_object <- function(dn, c_datamodel, accepted_types = NULL) {
  c_object <- c_datamodel$findObjectByDisplayName(dn)
  
  if (is.null(c_object))
    return()
  
  c_object <- auto_cast(c_object$getDataObject())
  
  if (!is.null(accepted_types) && !inherits(c_object, accepted_types))
    return()
  
  c_object
}

# Takes wrapped CN "<*>" or reference DN "{*}" and gives object
# Only meant for references (for consistency)
xn_to_object <- function(xn, c_datamodel, accepted_types = NULL) {
  if (xn == "")
    return()
  
  if (stringr::str_detect(xn, "^<CN=.*>$"))
    return(cn_to_object(stringr::str_sub(xn, 2L, -2L), c_datamodel, accepted_types = accepted_types))
  
  # Presumably, all references are wrapped in either <CN> (handled before) or {DN} 
  assert_that(
    stringr::str_detect(xn, "^\\{.*\\}$"),
    msg = paste0("`", xn, "` could not be interpreted as value reference.")
  )
  
  dn <- unescape_ref(xn)
  c_obj <- dn_to_object(dn, c_datamodel, accepted_types = accepted_types)
  
  assert_that(
    !is.null(c_obj),
    msg = paste0("`", dn, "` in value reference `", xn, "` could not be  be resolved.")
  )
  
  # if dn_to_object doesn't return a reference I think its always
  # supposed to be the ValueReference
  if (c_obj$getObjectType() != "Reference")
    c_obj <- c_obj$getValueReference()
  
  c_obj
}

# get the CN of an object as string
get_cn <- function(c_object) {
  if (is.null(c_object))
    return(NA_character_)
  
  # cl_object$getCN()$getString()
  # For performance reasons:
  CCommonName_getString(CDataObject_getCN(c_object))
}

# get the DN of an object or list of objects as character vector
# argument `is_species` defines if the function has to check for species in the objects list
# species need to have their objectdisplayname gathered via a different function than other objects
# type can have values TRUE, FALSE, NA
# TRUE is all species, FALSE is no species, NA forces to check for each member
get_key <- function(objects, is_species = FALSE) {
  if (!is.list(objects))
    cl_objects <- list(objects)
  else
    cl_objects <- objects
  
  are_species <- rep_along(cl_objects, is_species)
  
  if (is.na(is_species)) {
    are_species <- map_swig_chr(cl_objects, "getObjectType") == "Metabolite"
  }
  
  dns <- character(length(cl_objects))
  
  dns[!are_species] <- map_swig_chr(cl_objects[!are_species], "getObjectDisplayName")
  
  dns[are_species] <-
    cl_objects[are_species] %>%
    map(as, "_p_CMetab") %>%
    map_chr(CMetabNameInterface_createUniqueDisplayName, FALSE)
    
  dns
}

# If a DN is a reference, wrap it in {} and escape it
escape_ref <- function(x) {
  paste0(
    "{",
    stringr::str_replace_all(x, coll("}"), "\\}"),
    "}"
  )
}

# revert escape_ref
unescape_ref <- function(x) {
  x %>%
    stringr::str_sub(2L, -2L) %>%
    stringr::str_replace_all(coll("\\}"), "}")
}

# Takes a reference object and returns either the reference "{DN}"
# or if that won't resolve back returns "<CN>"
as_ref <- function(cl_objects, c_datamodel) {
  refs <- get_key(cl_objects, is_species = FALSE)
  
  unresolvable <-
    refs %>%
    map(dn_to_object, c_datamodel = c_datamodel) %>%
    map_lgl(is.null)
  
  refs[unresolvable] <- paste0("<", map_chr(cl_objects[unresolvable], get_cn), ">")
  refs[!unresolvable] <- escape_ref(refs[!unresolvable])
  
  refs
}

# gather a list of objects from the internal COPASI Key identifier
cop_key_to_obj <- function(x) {
  map(x, CRootContainer_getKeyFactory()$get)
}
