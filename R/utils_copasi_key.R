# copasi_key <- function(x) {
#   assert_that(is.character(x))
#   structure(x, class = c("copasi_key", is(x)))
# }
#
# #' @export
# is.copasi_key <- function(x) {
#   inherits(x, "copasi_key")
# }
#
# #' @export
# as.copasi_key <- function(x) {
#   copasi_key(as.character(x))
# }

# #' @export
# format.copasi_key <- function(x, ...) {
#   descriptors <- stringr::str_match(x, "=([^=]+)s\\[([^\\[]+)\\]$")
#   paste0(
#     "# A copasi key object: ", descriptors[2L], ": ", descriptors[3L], "\n"
#     # "Key value: ", x
#   )
# }

# #' @export
# print.copasi_key <-
#   function(x, ...) {
#     cat(format(x, ...), "\n")
#     invisible(x)
#   }
#
# #' @export
# show.copasi_key <-
#   function(x) {
#     print(x)
#     invisible(x)
#   }

# #' @importFrom tibble is_vector_s3
# #' @export
# is_vector_s3.copasi_key <- function(x) {
#   TRUE
# }
#
# #' @importFrom tibble type_sum
# #' @export
# type_sum.copasi_key <- function(x) {
#   "ckey"
# }
#
# #' @importFrom tibble obj_sum
# #' @export
# type_sum.copasi_key <- function(x) {
#   paste0("ckey [", length(x), "]")
# }

copasi_object_types <-
  c(
    Model = "_p_CModel",
    Compartment = "_p_CCompartment",
    Metabolite = "_p_CMetab",
    ModelValue = "_p_CModelValue",
    Reaction = "_p_CReaction",
    Parameter = "_p_CCopasiParameter"
  )

auto_cast <- function(object) {
  type <- copasi_object_types[object$getObjectType()]
  if (is.na(type))
    object
  else
    as(object, type)
}

# Give CNs as character and get object
cn_to_object <- function(x, datamodel, accepted_types = NULL) {
  object <- datamodel$getObjectFromCN(CCommonName(x))
  if (is_null(object)) return()
  object <- auto_cast(object$getDataObject())
  if (!is_null(accepted_types) && !inherits(object, accepted_types)) return()
  object
}

# Give DisplayName as character and get object
dn_to_object <- function(x, datamodel, accepted_types = NULL) {
  object <- datamodel$findObjectByDisplayName(x)
  if (is_null(object)) return()
  object <- auto_cast(object$getDataObject())
  if (!is_null(accepted_types) && !inherits(object, accepted_types)) return()
  object
}
