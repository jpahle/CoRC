pkg_env <- new.env(parent = emptyenv())

# Using any function with the datamodel parameter set will redefine the currently active datamodel
pkg_env$curr_dm <- NULL

# Format function for the CDataModel class which is used as a basis for the print method
#' @include swig_wrapper.R
#' @export
setMethod("format",
  "_p_CDataModel",
  function(x, ...) {
    model <- x$getModel()

    string <- "# A copasi model reference:\n"
    string <- paste0(string, "Model name: \"" , model$getObjectName() , "\"\n")
    string <- paste0(string, "@ref is set to: " , capture.output(x@ref) , "\n")
    string <- paste0(string, "Number of compartments: " , model$getCompartments()$size(), "\n")
    string <- paste0(string, "Number of species: " , model$getMetabolites()$size(), "\n")
    string <- paste0(string, "Number of reactions: " , model$getReactions()$size(), "\n")

    string
  }
)

#' @include swig_wrapper.R
#' @export
setMethod("print",
  "_p_CDataModel",
  function(x, ...) {
    cat(format(x, ...), "\n")
    invisible(x)
  }
)

#' @include swig_wrapper.R
#' @export
setMethod("show",
  "_p_CDataModel",
  function(object) {
    print(object)
    invisible(object)
  }
)

#' @export
format.copasi_key <- function(x, ...) {
  paste0("# A copasi species key: ", stringr::str_match(x, "([^\\[]+)\\]$")[2L])
}

#' @importFrom tibble type_sum
#' @export
type_sum.copasi_key <- function(x) {
  paste0("key: ", stringr::str_match(x, "([^\\[]+)\\]$")[2L])
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

#' Autoplot method for copasi timeseries objects.
#'
#' Uses ggplot2 to plot timeseries.
#' The plot has a geom_line layer.
#'
#' @param object A copasi timeseries object
#' @param \dots Species names selected for plotting
#' @return A ggplot2 plot
#' @importFrom ggplot2 autoplot
#' @export
autoplot.copasi_ts <- function(object, ...) {
  # use partial matching with ... args
  selected <- flatten_chr(list(...)) %>% pmatch(names(object))
  
  if (anyNA(selected)) {
    warning("Partial matching failed for some species")
    selected <- selected[!is.na(selected)]
  }
  
  # only add species selected in ...
  if (!is_empty(selected)) object <- object %>% dplyr::select(Time, !!!selected)
  
  # reshape data frame for ggplot and define the plot
  object %>%
    tidyr::gather(Species, Concentration, -Time) %>%
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Concentration, group = Species, color = Species)) +
    ggplot2::geom_line()
}

# Works like seq_along for CDataVectors (0 based index)
seq_along_cv <- function(copasivector) {
  len <- copasivector$size()
  
  if (len == 0L) return(integer())
  
  0L:(len - 1L)
}

# Attempts to guess what Object a CDataVector returns when $get() is called
get_from_cv <- function(copasivector, index) {
  type <- is(copasivector)[1L]

  # exise the items class from the classname of the vector
  type <- paste0("_p_", stringr::str_match(type, "^_p_CDataVector\\w+_(\\w+)_t$")[2L])

  # typecasting the result
  as(copasivector$get(index), type)
}

# Checks whether the datamodel parameter is valid
confirmDatamodel <- function(datamodel) {
  success <- is(datamodel, "_p_CDataModel")

  if (success) pkg_env$curr_dm <- datamodel

  success
}

# Better error message for assert_that
assertthat::on_failure(confirmDatamodel) <- function(call, env) {
  paste0(deparse(call$datamodel), " is not a datamodel")
}

# CParameter gives us only feedback on what kind of value it needs.
# It is our responsibility to call the right function which is why this list is needed.
cparameter_set_functions <-
  list(
    DOUBLE = CCopasiParameter_setDblValue,
    UDOUBLE = CCopasiParameter_setUDblValue,
    INT = CCopasiParameter_setIntValue,
    UINT = CCopasiParameter_setUIntValue,
    BOOL = CCopasiParameter_setBoolValue,
    STRING = CCopasiParameter_setStringValue,
    GROUP = NULL, # setGroupValue
    CN = NULL, # setCNValue
    KEY = NULL, # setKeyValue
    FILE = NULL, # setFileValue
    EXPRESSION = NULL,
    INVALID = NULL
  )

cparameter_get_functions <-
  list(
    DOUBLE = CCopasiParameter_getDblValue,
    UDOUBLE = CCopasiParameter_getUDblValue,
    INT = CCopasiParameter_getIntValue,
    UINT = CCopasiParameter_getUIntValue,
    BOOL = CCopasiParameter_getBoolValue,
    STRING = CCopasiParameter_getStringValue,
    GROUP = NULL, # setGroupValue
    CN = NULL, # setCNValue
    KEY = NULL, # setKeyValue
    FILE = NULL, # setFileValue
    EXPRESSION = NULL,
    INVALID = NULL
  )

cparameter_control_functions <-
  list(
    DOUBLE = is_numeric,
    UDOUBLE = (function(x) {is_scalar_numeric(x) && x >= 0}),
    INT = rlang::is_scalar_integerish,
    UINT = (function(x) {rlang::is_scalar_integerish && x >= 0}),
    BOOL = is_scalar_logical,
    STRING = is_scalar_character,
    GROUP = NULL, # setGroupValue
    CN = NULL, # setCNValue
    KEY = NULL, # setKeyValue
    FILE = NULL, # setFileValue
    EXPRESSION = NULL,
    INVALID = NULL
  )

methodstructure <- function(method) {
  names <- seq_along_cv(method) %>% map_chr(~ method$getParameter(.x)$getObjectName()) %>% make.names(unique = TRUE)
  
  types <- seq_along_cv(method) %>% map_chr(~ method$getParameter(.x)$getType())
  if (contains(types, NULL)) warning("Unknown type found with parameters: ", paste(names[map_lgl(types, is.null)], collapse = "; "))
  
  types %>% set_names(names)
}
