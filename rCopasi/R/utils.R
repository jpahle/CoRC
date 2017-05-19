pkg_env <- new.env(parent = emptyenv())

# Using any function with the datamodel parameter set will redefine the currently active datamodel
pkg_env$curr_dm <- NULL

# Format function for the CDataModel class which is used as a basis for the print method
#' @include swig_wrapper.R
#' @export
format._p_CDataModel <- function(x, ...) {
  model <- x$getModel()

  string <- ""
  string <- paste0(string, 'Model name: "' , model$getObjectName() , '"\n')
  string <- paste0(string, '@ref is set to: ' , capture.output(datamodel@ref) , '\n')
  n <- model$getCompartments()$size()
  string <- paste0(string, "Number of Compartments: " , n, "\n")
  n <- model$getMetabolites()$size()
  string <- paste0(string, "Number of Metabolites: " , n, "\n")
  n <- model$getReactions()$size()
  paste0(string, "Number of Reactions: " , n, "\n")
}

#' @include swig_wrapper.R
#' @export
setMethod("print",
          "_p_CDataModel",
          function(x, ...) {
            cat(format(x, ...), "\n")
          }
)

# Works like seq_along for CDataVectors (0 based index)
seq_along_cv <- function(copasivector) {0L:(copasivector$size() - 1)}

# Attempts to guess what Object a CDataVector returns when $get() is called
get_from_cv <- function(copasivector, index) {
  type <- is(copasivector)[1]

  # exise the items class from the classname of the vector
  type <- paste0("_p_", stringr::str_sub(type, 17L, -3L))

  # typecasting the result
  as(copasivector$get(index), type)
}

# Checks whether the datamodel parameter is valid
confirmDatamodel <- function(datamodel) {
  success <- is(datamodel, "_p_CDataModel")

  if (success) pkg_env$curr_dm <- datamodel

  success
}
