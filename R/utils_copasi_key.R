copasi_object_types <-
  c(
    Model = "_p_CModel",
    Compartment = "_p_CCompartment",
    Metabolite = "_p_CMetab",
    ModelValue = "_p_CModelValue",
    Reaction = "_p_CReaction",
    Parameter = "_p_CCopasiParameter"
  )

auto_cast <- function(c_object) {
  type <- copasi_object_types[c_object$getObjectType()]
  if (is.na(type))
    c_object
  else
    as(c_object, type)
}

# Give CNs as character and get object
cn_to_object <- function(cn, c_datamodel, accepted_types = NULL) {
  c_object <- c_datamodel$getObjectFromCN(CCommonName(cn))
  if (is_null(c_object)) return()
  c_object <- auto_cast(c_object$getDataObject())
  if (!is_null(accepted_types) && !inherits(c_object, accepted_types)) return()
  c_object
}

# Give DisplayName as character and get object
dn_to_object <- function(dn, c_datamodel, accepted_types = NULL) {
  c_object <- c_datamodel$findObjectByDisplayName(dn)
  if (is_null(c_object)) return()
  c_object <- auto_cast(c_object$getDataObject())
  if (!is_null(accepted_types) && !inherits(c_object, accepted_types)) return()
  c_object
}

xn_to_object <- function(xn, c_datamodel, accepted_types = NULL) {
  if (stringr::str_detect(xn, "^<CN=.*>$"))
    return(cn_to_object(stringr::str_sub(xn, 2L, -2L), c_datamodel, accepted_types))
  else if (stringr::str_detect(xn, "^\\{.*\\}$"))
    xn <- unescape_ref(xn)
  
  dn_to_object(xn, c_datamodel, accepted_types)
}

get_cn <- function(c_object) {
  if (is_null(c_object)) return(NA_character_)
  
  # cl_object$getCN()$getString()
  # For performance reasons:
  CCommonName_getString(CDataObject_getCN(c_object))
}
