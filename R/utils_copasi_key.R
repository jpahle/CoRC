#' @export
format.copasi_key <- function(x, ...) {
  descriptors <- stringr::str_match(x, "=([^=]+)s\\[([^\\[]+)\\]$")
  paste0(
    "# A copasi key object: ", descriptors[2L], ": ", descriptors[3L], "\n",
    "Key value: ", x
  )
}

#' @importFrom tibble type_sum
#' @export
type_sum.copasi_key <- function(x) {
  descriptors <- stringr::str_match(x, "=([^=]+)s\\[([^\\[]+)\\]$")
  paste0(descriptors[2L], ": ", descriptors[3L])
}

#' @export
print.copasi_key <-
  function(x, ...) {
    cat(format(x, ...), "\n")
    invisible(x)
  }

#' @export
show.copasi_key <-
  function(object) {
    print(object)
    invisible(object)
  }

#' @export
is.copasi_key <-
  function(object) {
    is(object, "copasi_key")
  }

#' @export
as.copasi_key <-
  function(object) {
    assert_that(is_scalar_character(object))
    structure(object, class = "copasi_key")
  }

copasi_object_types <-
  c(
    CN = "_p_CDataModel",
    Model = "_p_CModel",
    Compartment = "_p_CCompartment",
    Metabolite = "_p_CMetab",
    ModelValue = "_p_CModelValue",
    Reaction = "_p_CReaction"
  )

auto_cast <- function(object) {
  as(object, copasi_object_types[object$getObjectType()])
}

# Give CNs as character vector and get objects
cn_to_object <- function(strings, datamodel, accepted_types = NULL) {
  objects <-
    map(
      strings,
      ~ {
        cn <- CCommonName(.x)
        object <- datamodel$getObjectFromCN(cn)
        delete(cn)
        
        if (!is_null(object)) {
          auto_cast(object$getDataObject())
        } else {
          NULL
        }
      }
    )
  
  # if accepted_types were given, NULL all objects that do not have any of those types.
  if (!is_null(accepted_types)) {
    objects <- map_if(
      objects,
      ~ !any(is(.x) %in% accepted_types),
      ~ NULL
    )
  }
  
  objects
}