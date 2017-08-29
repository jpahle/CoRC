# By comparing the output of getObjectType with this list, we can safely typecast it seems
copasi_object_types <-
  c(
    Model = "_p_CModel",
    Compartment = "_p_CCompartment",
    Metabolite = "_p_CMetab",
    ModelValue = "_p_CModelValue",
    Reaction = "_p_CReaction",
    Parameter = "_p_CCopasiParameter"
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
  if (is_null(c_object)) return()
  c_object <- auto_cast(c_object$getDataObject())
  if (!is_null(accepted_types) && !inherits(c_object, accepted_types)) return()
  c_object
}

# Takes DisplayName as character and gives object
dn_to_object <- function(dn, c_datamodel, accepted_types = NULL) {
  c_object <- c_datamodel$findObjectByDisplayName(dn)
  if (is_null(c_object)) return()
  c_object <- auto_cast(c_object$getDataObject())
  if (!is_null(accepted_types) && !inherits(c_object, accepted_types)) return()
  c_object
}

# Takes wrapped CN "<*>", reference DN "{*}" and gives object
# Only meant for references (for consistency)
xn_to_object <- function(xn, c_datamodel, accepted_types = NULL) {
  if (stringr::str_detect(xn, "^<CN=.*>$"))
    return(
      cn_to_object(stringr::str_sub(xn, 2L, -2L), c_datamodel, accepted_types)
    )
  
  # All references should be wrapped either <CN> or {DN} 
  assert_that(
    stringr::str_detect(xn, "^\\{.*\\}$"),
    msg = paste0("Internal: Couldn't resove ref \"", xn, '" in xn_to_object.')
  )
  
  xn <- unescape_ref(xn)
  c_obj <- dn_to_object(xn, c_datamodel, accepted_types)
  
  # if dn_to_object doesn't return a reference I think its always
  # supposed to be the ValueReference
  if (c_obj$getObjectType() != "Reference")
    c_obj <- c_obj$getValueReference()
  
  c_obj
}

# get the CN of an object as string
get_cn <- function(c_object) {
  if (is_null(c_object)) return(NA_character_)
  
  # cl_object$getCN()$getString()
  # For performance reasons:
  CCommonName_getString(CDataObject_getCN(c_object))
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

# Takes an object and returns either the reference "{DN}"
# or if that won't resolve back returns "<CN>"
as_ref <- function(cl_objects, c_datamodel) {
  refs <- cl_objects %>% map_swig_chr("getObjectDisplayName")
  
  dn_unresolvable <- map(refs, dn_to_object, c_datamodel) %>% map_lgl(is_null)
  
  refs[dn_unresolvable] <- map_chr(cl_objects[dn_unresolvable], get_cn)
  
  refs[!dn_unresolvable] <- escape_ref(refs[!dn_unresolvable])
  refs[dn_unresolvable] <- paste0("<", refs[dn_unresolvable], ">")
  
  refs
}
